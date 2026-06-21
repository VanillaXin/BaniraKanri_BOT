package xin.vanilla.banira.plugin.chat;

import com.google.gson.JsonObject;
import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import com.mikuac.shiro.enums.MsgTypeEnum;
import com.mikuac.shiro.model.ArrayMsg;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import xin.vanilla.banira.domain.AiMemory;
import xin.vanilla.banira.enums.EnumCacheFileType;
import xin.vanilla.banira.enums.EnumMessageType;
import xin.vanilla.banira.mapper.param.AiMemoryQueryParam;
import xin.vanilla.banira.plugin.chat.agent.AgentContext;
import xin.vanilla.banira.plugin.chat.capability.AiDirectResult;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.service.IAiMemoryManager;
import xin.vanilla.banira.util.*;

import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.ThreadLocalRandom;

/**
 * 用轻量记忆保存可复用表情包 CQ 码。
 */
@Slf4j
@Service
public class ChatStickerService {

    public static final String STICKER_PREFIX = "[STICKER] ";
    public static final String STICKER_TAG = "type:sticker";
    private static final int MAX_STICKER_CQ_LENGTH = 1200;
    private static final int MAX_STICKER_BYTES = 5 * 1024 * 1024;
    private static final int QUERY_LIMIT = 40;

    @Resource
    private IAiMemoryManager aiMemoryManager;

    public void collectFromMessage(@Nonnull BaniraBot bot, @Nonnull AnyMessageEvent event) {
        if (event.getUserId() == bot.getSelfId()
                || CollectionUtils.isNullOrEmpty(event.getArrayMsg())
                || !isCollectableStickerMessage(event.getArrayMsg())) {
            return;
        }
        String message = StringUtils.nullToEmpty(event.getMessage()).trim();
        if (StringUtils.isNullOrEmptyEx(message) || message.length() > MAX_STICKER_CQ_LENGTH) {
            return;
        }
        StickerSource source = firstStickerSource(event.getArrayMsg(), message);
        if (source == null || StringUtils.isNullOrEmptyEx(source.url())) {
            return;
        }
        if (!isDownloadableImageSource(source.url())) {
            return;
        }
        long groupId = BaniraUtils.isGroupIdValid(event.getGroupId()) ? event.getGroupId() : 0L;
        if (stickerExists(bot.getSelfId(), groupId, source.url())) {
            return;
        }
        byte[] bytes = downloadStickerBytes(source.url());
        if (bytes == null || bytes.length == 0 || bytes.length > MAX_STICKER_BYTES) {
            LOGGER.debug("AI ignored sticker group={} user={} msgId={} bytes={}",
                    groupId, event.getUserId(), event.getMessageId(), bytes != null ? bytes.length : -1);
            return;
        }
        String cachePath = BaniraUtils.saveFileToCachePath(bytes, EnumCacheFileType.image);
        if (StringUtils.isNullOrEmptyEx(cachePath)) {
            return;
        }
        String plainText = plainText(event.getArrayMsg());
        String description = StringUtils.isNotNullOrEmpty(plainText)
                ? "带文字说明的表情包：" + plainText
                : "自动收集的表情包";
        String scene = StringUtils.isNotNullOrEmpty(plainText)
                ? "适合回应类似“" + AiTextLimits.truncate(plainText, 40) + "”的语境"
                : "适合需要用表情包简单回应的语境";
        String content = STICKER_PREFIX + JsonUtils.toJsonString(Map.of(
                "path", cachePath,
                "originalCq", message,
                "sourceUrl", source.url(),
                "description", description,
                "scene", scene,
                "sourceSender", event.getUserId(),
                "sourceGroup", groupId,
                "sourceMsgId", event.getMessageId(),
                "size", bytes.length
        ));
        long now = DateUtils.getTimestamp(new Date());
        AiMemory memory = new AiMemory()
                .setBotId(bot.getSelfId())
                .setGroupId(groupId)
                .setUserId(0L)
                .setContent(content)
                .setTags(STICKER_TAG + ",source:auto_collect")
                .setSourceMsgId(String.valueOf(event.getMessageId()))
                .setCreatedAt(now)
                .setLastUsedAt(now);
        aiMemoryManager.addMemory(memory);
        LOGGER.debug("AI collected sticker group={} user={} msgId={} chars={}",
                groupId, event.getUserId(), event.getMessageId(), message.length());
    }

    @Nonnull
    public String sendSticker(@Nonnull AgentContext ctx, @Nullable String keyword) {
        List<AiMemory> candidates = findStickers(ctx, keyword);
        if (candidates.isEmpty()) {
            return "没有找到合适的表情包。";
        }
        AiMemory memory = candidates.get(ThreadLocalRandom.current().nextInt(candidates.size()));
        StickerEntry sticker = parseSticker(memory);
        if (!sticker.usable()) {
            return "这条表情包记录不可用。";
        }
        String message = sticker.toMessage();
        if (ctx.msgType() == EnumMessageType.GROUP && ctx.scopeGroupId() > 0) {
            ctx.bot().sendGroupMsg(ctx.scopeGroupId(), message, false);
        } else if (ctx.senderId() != null && ctx.senderId() > 0) {
            ctx.bot().sendPrivateMsg(ctx.senderId(), message, false);
        } else {
            return "当前会话不能发送表情包。";
        }
        if (memory.getId() != null) {
            aiMemoryManager.touchMemory(memory.getId(), DateUtils.getTimestamp(new Date()));
        }
        LOGGER.debug("AI sent sticker group={} user={} memoryId={}",
                ctx.scopeGroupId(), ctx.senderId(), memory.getId());
        return AiDirectResult.sent("已发送表情包。");
    }

    @Nonnull
    public List<String> stickerForwardMessages(@Nonnull AgentContext ctx, @Nullable String keyword, int count) {
        int limit = Math.max(1, Math.min(count, 8));
        List<AiMemory> candidates = findStickers(ctx, keyword);
        if (candidates.isEmpty()) {
            return List.of();
        }
        List<AiMemory> shuffled = new ArrayList<>(candidates);
        java.util.Collections.shuffle(shuffled);
        List<String> result = new ArrayList<>();
        long now = DateUtils.getTimestamp(new Date());
        for (AiMemory memory : shuffled) {
            StickerEntry sticker = parseSticker(memory);
            if (!sticker.usable()) {
                continue;
            }
            result.add(sticker.toMessage());
            if (memory.getId() != null) {
                aiMemoryManager.touchMemory(memory.getId(), now);
            }
            if (result.size() >= limit) {
                break;
            }
        }
        LOGGER.debug("AI prepared sticker forward group={} user={} requested={} actual={} keyword={}",
                ctx.scopeGroupId(), ctx.senderId(), limit, result.size(), keyword);
        return result;
    }

    @Nonnull
    public String describeStickers(@Nonnull AgentContext ctx, @Nullable String keyword, int count) {
        int limit = Math.max(1, Math.min(count, 12));
        List<AiMemory> candidates = findStickers(ctx, keyword);
        if (candidates.isEmpty()) {
            return "没有找到匹配的表情包。";
        }
        StringBuilder builder = new StringBuilder("可用表情包摘要，仅供选择，不要原样发出：");
        int emitted = 0;
        for (AiMemory memory : candidates) {
            StickerEntry sticker = parseSticker(memory);
            if (!sticker.usable()) {
                continue;
            }
            builder.append('\n')
                    .append("- id=").append(memory.getId() != null ? memory.getId() : 0)
                    .append("；描述=").append(StringUtils.isNotNullOrEmpty(sticker.description()) ? sticker.description() : "无")
                    .append("；场景=").append(StringUtils.isNotNullOrEmpty(sticker.scene()) ? sticker.scene() : "无");
            emitted++;
            if (emitted >= limit) {
                break;
            }
        }
        return emitted > 0 ? builder.toString() : "没有找到可用的表情包。";
    }

    @Nonnull
    public String updateStickerUsage(@Nonnull AgentContext ctx,
                                     @Nullable String keyword,
                                     @Nullable String description,
                                     @Nullable String scene) {
        if (!BaniraUtils.isOwner(ctx.senderId())) {
            return ownerOnlyText("表情包描述修正");
        }
        String normalized = StringUtils.nullToEmpty(keyword).trim();
        String normalizedDescription = StringUtils.nullToEmpty(description).trim();
        String normalizedScene = StringUtils.nullToEmpty(scene).trim();
        if (StringUtils.isNullOrEmptyEx(normalized)) {
            return "要修正的表情包不明确，需要给关键词。";
        }
        if (StringUtils.isNullOrEmptyEx(normalizedDescription) && StringUtils.isNullOrEmptyEx(normalizedScene)) {
            return "新的描述或使用场景不能为空。";
        }
        List<AiMemory> memories = findStickers(ctx, normalized);
        int updated = 0;
        for (AiMemory memory : memories) {
            StickerEntry entry = parseSticker(memory);
            if (!entry.usable()) {
                continue;
            }
            StickerEntry rewritten = entry.with(
                    StringUtils.isNotNullOrEmpty(normalizedDescription) ? normalizedDescription : entry.description(),
                    StringUtils.isNotNullOrEmpty(normalizedScene) ? normalizedScene : entry.scene()
            );
            memory.setContent(STICKER_PREFIX + rewritten.toJson());
            if (aiMemoryManager.updateMemory(memory)) {
                updated++;
            }
        }
        LOGGER.debug("AI updated sticker usage group={} user={} count={} keyword={}",
                ctx.scopeGroupId(), ctx.senderId(), updated, normalized);
        return updated > 0 ? "已修正 " + updated + " 个表情包的描述。" : "没找到可修正的表情包。";
    }

    @Nonnull
    public String discardStickers(@Nonnull AgentContext ctx, @Nullable String keyword) {
        if (!BaniraUtils.isOwner(ctx.senderId())) {
            return ownerOnlyText("表情包清理");
        }
        String normalized = StringUtils.nullToEmpty(keyword).trim();
        if (StringUtils.isNullOrEmptyEx(normalized)) {
            return "要丢弃的表情包不明确，需要给关键词；如果要全清，明确说全部。";
        }
        List<AiMemory> memories = findStickers(ctx, keyword);
        if (memories.isEmpty()) {
            return "没找到要丢弃的表情包。";
        }
        int deleted = deleteAll(memories);
        LOGGER.debug("AI discarded stickers group={} user={} count={} keyword={}",
                ctx.scopeGroupId(), ctx.senderId(), deleted, keyword);
        return "已丢弃 " + deleted + " 个表情包。";
    }

    @Nonnull
    public String forgetMemories(@Nonnull AgentContext ctx, @Nullable String keyword) {
        if (!BaniraUtils.isOwner(ctx.senderId())) {
            return ownerOnlyText("记忆清理");
        }
        String normalized = StringUtils.nullToEmpty(keyword).trim();
        if (StringUtils.isNullOrEmptyEx(normalized) || normalized.length() < 2) {
            return "要忘记的内容太模糊，需要给一个关键词。";
        }
        AiMemoryQueryParam param = new AiMemoryQueryParam()
                .setBotId(ctx.botId())
                .setGroupIdInGlobal(ctx.scopeGroupId())
                .setContentLike(normalized)
                .setLimit(QUERY_LIMIT);
        List<AiMemory> memories = aiMemoryManager.getMemoryList(param);
        int deleted = deleteAll(memories);
        LOGGER.debug("AI forgot memories group={} user={} count={} keyword={}",
                ctx.scopeGroupId(), ctx.senderId(), deleted, normalized);
        return deleted > 0 ? "已忘记 " + deleted + " 条相关记忆。" : "没找到相关记忆。";
    }

    @Nonnull
    private List<AiMemory> findStickers(@Nonnull AgentContext ctx, @Nullable String keyword) {
        String normalized = StringUtils.nullToEmpty(keyword).trim();
        AiMemoryQueryParam param = new AiMemoryQueryParam()
                .setBotId(ctx.botId())
                .setGroupIdInGlobal(ctx.scopeGroupId())
                .setTagsLike(STICKER_TAG)
                .setLimit(QUERY_LIMIT)
                .addOrderByLastUsedAt(false);
        if (StringUtils.isNotNullOrEmpty(normalized) && !isAllKeyword(normalized)) {
            param.setContentLike(normalized);
        }
        List<AiMemory> memories = aiMemoryManager.getMemoryList(param);
        List<AiMemory> result = new ArrayList<>();
        for (AiMemory memory : memories) {
            if (parseSticker(memory).usable()) {
                result.add(memory);
            }
        }
        return result;
    }

    private int deleteAll(@Nullable List<AiMemory> memories) {
        if (CollectionUtils.isNullOrEmpty(memories)) {
            return 0;
        }
        int deleted = 0;
        for (AiMemory memory : memories) {
            if (memory != null && memory.getId() != null && aiMemoryManager.deleteMemory(memory.getId())) {
                deleted++;
            }
        }
        return deleted;
    }

    @Nonnull
    private static StickerEntry parseSticker(@Nullable AiMemory memory) {
        if (memory == null) {
            return StickerEntry.empty();
        }
        String content = StringUtils.nullToEmpty(memory.getContent()).trim();
        if (!content.startsWith(STICKER_PREFIX)) {
            return StickerEntry.empty();
        }
        String payload = content.substring(STICKER_PREFIX.length()).trim();
        JsonObject json = JsonUtils.parseJsonObject(payload);
        if (json == null) {
            return new StickerEntry("", payload, "", "", "", "", 0);
        }
        return new StickerEntry(
                JsonUtils.getString(json, "path", ""),
                JsonUtils.getString(json, "originalCq", ""),
                JsonUtils.getString(json, "sourceUrl", ""),
                JsonUtils.getString(json, "description", ""),
                JsonUtils.getString(json, "scene", ""),
                JsonUtils.getString(json, "sourceMsgId", ""),
                JsonUtils.getLong(json, "size", 0L)
        );
    }

    private boolean stickerExists(long botId, long groupId, @Nonnull String sourceUrl) {
        AiMemoryQueryParam param = new AiMemoryQueryParam()
                .setBotId(botId)
                .setGroupIdInGlobal(groupId)
                .setTagsLike(STICKER_TAG)
                .setContentLike(sourceUrl)
                .setLimit(1);
        return !aiMemoryManager.getMemoryList(param).isEmpty();
    }

    private static boolean isCollectableStickerMessage(@Nonnull List<ArrayMsg> msgs) {
        boolean hasSticker = false;
        for (ArrayMsg msg : msgs) {
            if (msg == null) {
                continue;
            }
            MsgTypeEnum type = msg.getType();
            if (type == MsgTypeEnum.image || type == MsgTypeEnum.mface || type == MsgTypeEnum.marketface) {
                hasSticker = true;
                continue;
            }
            if (type == MsgTypeEnum.text || type == MsgTypeEnum.at || type == MsgTypeEnum.reply) {
                continue;
            }
            return false;
        }
        return hasSticker;
    }

    @Nullable
    private static StickerSource firstStickerSource(@Nonnull List<ArrayMsg> msgs, @Nonnull String originalCq) {
        for (ArrayMsg msg : msgs) {
            if (msg == null) {
                continue;
            }
            MsgTypeEnum type = msg.getType();
            if (type == MsgTypeEnum.image || type == MsgTypeEnum.mface || type == MsgTypeEnum.marketface) {
                String url = msg.getStringData("url");
                if (StringUtils.isNullOrEmptyEx(url)) {
                    url = msg.getStringData("file");
                }
                if (StringUtils.isNotNullOrEmpty(url)) {
                    return new StickerSource(url, originalCq);
                }
            }
        }
        return null;
    }

    @Nonnull
    private static String plainText(@Nonnull List<ArrayMsg> msgs) {
        StringBuilder builder = new StringBuilder();
        for (ArrayMsg msg : msgs) {
            if (msg != null && msg.getType() == MsgTypeEnum.text) {
                String text = StringUtils.nullToEmpty(msg.getStringData("text")).trim();
                if (StringUtils.isNotNullOrEmpty(text)) {
                    if (!builder.isEmpty()) {
                        builder.append(' ');
                    }
                    builder.append(text);
                }
            }
        }
        return AiTextLimits.truncate(builder.toString().trim(), 80);
    }

    @Nullable
    private static byte[] downloadStickerBytes(@Nonnull String url) {
        String trimmed = url.trim();
        String lower = trimmed.toLowerCase(java.util.Locale.ROOT);
        try {
            if (lower.startsWith("data:image/")) {
                int comma = trimmed.indexOf(',');
                if (comma > 0 && trimmed.substring(0, comma).toLowerCase(java.util.Locale.ROOT).contains(";base64")) {
                    return Base64.getDecoder().decode(trimmed.substring(comma + 1));
                }
                return null;
            }
            if (lower.startsWith("file:")) {
                Path path = Path.of(URI.create(trimmed));
                if (Files.isRegularFile(path) && Files.size(path) <= MAX_STICKER_BYTES) {
                    return Files.readAllBytes(path);
                }
                return null;
            }
            if (Files.isRegularFile(Path.of(trimmed)) && Files.size(Path.of(trimmed)) <= MAX_STICKER_BYTES) {
                return Files.readAllBytes(Path.of(trimmed));
            }
        } catch (Exception ignored) {
        }
        return HttpUtils.downloadBytes(trimmed);
    }

    private static boolean isDownloadableImageSource(@Nonnull String url) {
        String trimmed = url.trim();
        String lower = trimmed.toLowerCase(java.util.Locale.ROOT);
        if (lower.startsWith("http://") || lower.startsWith("https://") || lower.startsWith("data:image/") || lower.startsWith("file:")) {
            return true;
        }
        try {
            return Files.isRegularFile(Path.of(trimmed));
        } catch (Exception ignored) {
            return false;
        }
    }

    private static boolean isAllKeyword(@Nonnull String keyword) {
        String compact = keyword.replaceAll("\\s+", "").toLowerCase(java.util.Locale.ROOT);
        return compact.equals("全部") || compact.equals("all") || compact.equals("*");
    }

    @Nonnull
    private static String ownerOnlyText(@Nonnull String action) {
        String display = BaniraUtils.getOwnerDisplayName();
        if (StringUtils.isNullOrEmptyEx(display) || "能处理的人".equals(display)) {
            return action + "不能听你的。";
        }
        return action + "只听 " + display + " 的。";
    }

    private record StickerSource(String url, String originalCq) {
    }

    private record StickerEntry(String path,
                                String originalCq,
                                String sourceUrl,
                                String description,
                                String scene,
                                String sourceMsgId,
                                long size) {

        static StickerEntry empty() {
            return new StickerEntry("", "", "", "", "", "", 0);
        }

        boolean usable() {
            return StringUtils.isNotNullOrEmpty(path) || StringUtils.isNotNullOrEmpty(originalCq);
        }

        @Nonnull
        String toMessage() {
            if (StringUtils.isNotNullOrEmpty(path)) {
                return MsgUtils.builder().img(path).build();
            }
            return originalCq;
        }

        @Nonnull
        StickerEntry with(@Nonnull String description, @Nonnull String scene) {
            return new StickerEntry(path, originalCq, sourceUrl, description, scene, sourceMsgId, size);
        }

        @Nonnull
        String toJson() {
            return JsonUtils.toJsonString(Map.of(
                    "path", StringUtils.nullToEmpty(path),
                    "originalCq", StringUtils.nullToEmpty(originalCq),
                    "sourceUrl", StringUtils.nullToEmpty(sourceUrl),
                    "description", StringUtils.nullToEmpty(description),
                    "scene", StringUtils.nullToEmpty(scene),
                    "sourceMsgId", StringUtils.nullToEmpty(sourceMsgId),
                    "size", size
            ));
        }
    }

}
