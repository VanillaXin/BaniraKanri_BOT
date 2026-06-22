package xin.vanilla.banira.plugin.chat;

import com.google.gson.JsonObject;
import com.mikuac.shiro.common.utils.MessageConverser;
import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import com.mikuac.shiro.enums.MsgTypeEnum;
import com.mikuac.shiro.model.ArrayMsg;
import dev.langchain4j.data.message.Content;
import dev.langchain4j.data.message.ImageContent;
import dev.langchain4j.data.message.TextContent;
import dev.langchain4j.data.message.UserMessage;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.domain.AiMemory;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.enums.EnumCacheFileType;
import xin.vanilla.banira.enums.EnumMessageType;
import xin.vanilla.banira.mapper.param.AiMemoryQueryParam;
import xin.vanilla.banira.mapper.param.MessageRecordQueryParam;
import xin.vanilla.banira.plugin.chat.agent.AgentContext;
import xin.vanilla.banira.plugin.chat.capability.AiDirectResult;
import xin.vanilla.banira.plugin.chat.model.ChatModelRouter;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.service.IAiMemoryManager;
import xin.vanilla.banira.service.IMessageRecordManager;
import xin.vanilla.banira.util.*;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.ThreadLocalRandom;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

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
    private static final int MAX_ARCHIVE_BYTES = 256 * 1024 * 1024;
    private static final int MAX_ARCHIVE_ENTRIES = 2000;
    private static final int MAX_AUTO_IMAGE_SIDE = 320;
    private static final int MAX_AUTO_IMAGE_PIXELS = 320 * 320;
    private static final int MAX_EXPLICIT_IMAGE_SIDE = 900;
    private static final int MAX_EXPLICIT_IMAGE_PIXELS = 900 * 900;
    private static final int QUERY_LIMIT = 40;
    private static final int MAX_METADATA_TEXT = 160;
    private static final int MAX_KEYWORD_COUNT = 12;
    private static final Pattern JSON_OBJECT_PATTERN = Pattern.compile("\\{.*}", Pattern.DOTALL);

    @Resource
    private IAiMemoryManager aiMemoryManager;
    @Resource
    private IMessageRecordManager messageRecordManager;

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
        byte[] bytes = downloadStickerBytes(source.url(), MAX_STICKER_BYTES);
        if (bytes == null || bytes.length == 0 || bytes.length > MAX_STICKER_BYTES) {
            LOGGER.debug("AI ignored sticker group={} user={} msgId={} bytes={}",
                    groupId, event.getUserId(), event.getMessageId(), bytes != null ? bytes.length : -1);
            return;
        }
        if (!looksLikeAutoCollectStickerImage(event.getArrayMsg(), bytes)) {
            LOGGER.debug("AI ignored non-sticker image group={} user={} msgId={} sourceType={} bytes={}",
                    groupId, event.getUserId(), event.getMessageId(), source.type(), bytes.length);
            return;
        }
        String plainText = plainText(event.getArrayMsg());
        StickerMetadata metadata = fallbackStickerMetadata(
                StringUtils.isNotNullOrEmpty(plainText) ? plainText : source.url(),
                imageInfo(bytes),
                StringUtils.isNotNullOrEmpty(plainText) ? "带文字说明的表情包：" + plainText : "",
                StringUtils.isNotNullOrEmpty(plainText) ? "适合回应类似“" + AiTextLimits.truncate(plainText, 40) + "”的语境" : ""
        );
        if (!saveStickerMemory(bot.getSelfId(), groupId, event.getUserId(), event.getMessageId(),
                source.url(), message, bytes, metadata.description(), metadata.scene(), metadata.keywords(),
                "source:auto_collect," + metadata.tagCsv())) {
            return;
        }
        LOGGER.debug("AI collected sticker group={} user={} msgId={} chars={}",
                groupId, event.getUserId(), event.getMessageId(), message.length());
    }

    @Nonnull
    public String collectStickerArchive(@Nonnull AgentContext ctx,
                                        @Nullable String source,
                                        @Nullable String description,
                                        @Nullable String scene) {
        return collectStickerArchive(ctx, source, description, scene, null);
    }

    @Nonnull
    public String collectStickerArchive(@Nonnull AgentContext ctx,
                                        @Nullable String source,
                                        @Nullable String description,
                                        @Nullable String scene,
                                        @Nullable ChatConfig chatConfig) {
        if (!BaniraUtils.hasKanriOperatorAccess(ctx.bot(), ctx.scopeGroupId(), ctx.senderId())) {
            return ownerOnlyText("表情包压缩包导入");
        }
        String resolved = resolveArchiveSource(ctx, source, messageRecordManager);
        if (StringUtils.isNullOrEmptyEx(resolved)) {
            return "没有找到可下载的压缩包链接。请直接发 zip 链接，或回复包含文件链接的消息再让我导入。";
        }
        if (!isArchiveSource(resolved)) {
            return "只支持 zip 压缩包。";
        }
        byte[] archive = downloadStickerBytes(resolved, MAX_ARCHIVE_BYTES);
        if (archive == null || archive.length == 0) {
            return "压缩包下载失败。";
        }
        if (archive.length > MAX_ARCHIVE_BYTES) {
            return "压缩包太大，已跳过。";
        }
        int scanned = 0;
        int saved = 0;
        int skipped = 0;
        int analyzed = 0;
        long sourceMsgId = StringUtils.toLong(ctx.msgId(), 0L);
        StickerVisionAnalyzer analyzer = StickerVisionAnalyzer.create(chatConfig);
        try (ZipInputStream zip = new ZipInputStream(new ByteArrayInputStream(archive))) {
            ZipEntry entry;
            while ((entry = zip.getNextEntry()) != null && scanned < MAX_ARCHIVE_ENTRIES) {
                if (entry.isDirectory()) {
                    continue;
                }
                scanned++;
                String name = StringUtils.nullToEmpty(entry.getName());
                if (!isImageFileName(name)) {
                    skipped++;
                    continue;
                }
                byte[] bytes = readZipEntry(zip, MAX_STICKER_BYTES + 1);
                ImageInfo info = imageInfo(bytes);
                if (bytes.length == 0 || bytes.length > MAX_STICKER_BYTES
                        || !info.valid()
                        || !looksLikeExplicitStickerInfo(info)) {
                    skipped++;
                    continue;
                }
                StickerMetadata metadata = analyzeStickerMetadata(name, bytes, info, description, scene, analyzer);
                String sourceUrl = resolved + "#" + name;
                if (stickerExists(ctx.botId(), ctx.scopeGroupId(), sourceUrl)) {
                    skipped++;
                    continue;
                }
                boolean ok = saveStickerMemory(ctx.botId(), ctx.scopeGroupId(), ctx.senderId(),
                        sourceMsgId, sourceUrl, "", bytes, metadata.description(), metadata.scene(), metadata.keywords(),
                        "source:archive," + metadata.tagCsv());
                if (ok) {
                    saved++;
                    analyzed++;
                } else {
                    skipped++;
                }
            }
        } catch (Exception e) {
            LOGGER.warn("AI collect sticker archive failed group={} user={} source={}",
                    ctx.scopeGroupId(), ctx.senderId(), resolved, e);
            return "压缩包解压失败。";
        }
        LOGGER.debug("AI collected sticker archive group={} user={} scanned={} saved={} skipped={} analyzed={} source={}",
                ctx.scopeGroupId(), ctx.senderId(), scanned, saved, skipped, analyzed, resolved);
        return saved > 0
                ? "已导入 " + saved + " 个表情包，跳过 " + skipped + " 个，已写入逐图描述和标签。"
                : "没有导入到合适的表情包。";
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
                    .append("；场景=").append(StringUtils.isNotNullOrEmpty(sticker.scene()) ? sticker.scene() : "无")
                    .append("；关键词=").append(StringUtils.isNotNullOrEmpty(sticker.keywords()) ? sticker.keywords() : "无");
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
        List<AiMemory> memories = new ArrayList<>();
        if (StringUtils.isNullOrEmptyEx(normalized) || isAllKeyword(normalized)) {
            memories.addAll(queryStickerMemories(ctx, ""));
        } else {
            for (String term : expandStickerKeyword(normalized)) {
                memories.addAll(queryStickerMemories(ctx, term));
                if (memories.size() >= QUERY_LIMIT) {
                    break;
                }
            }
        }
        List<AiMemory> result = new ArrayList<>();
        Set<String> seen = new HashSet<>();
        for (AiMemory memory : memories) {
            String key = memory.getId() != null ? "id:" + memory.getId() : "content:" + memory.getContent();
            if (seen.add(key) && parseSticker(memory).usable()) {
                result.add(memory);
                if (result.size() >= QUERY_LIMIT) {
                    break;
                }
            }
        }
        return result;
    }

    @Nonnull
    private List<AiMemory> queryStickerMemories(@Nonnull AgentContext ctx, @Nonnull String keyword) {
        AiMemoryQueryParam param = new AiMemoryQueryParam()
                .setBotId(ctx.botId())
                .setGroupIdInGlobal(ctx.scopeGroupId())
                .setTagsLike(STICKER_TAG)
                .setLimit(QUERY_LIMIT)
                .addOrderByLastUsedAt(false);
        if (StringUtils.isNotNullOrEmpty(keyword)) {
            param.setContentLike(keyword);
        }
        return aiMemoryManager.getMemoryList(param);
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
            return new StickerEntry("", payload, "", "", "", "", "", 0);
        }
        return new StickerEntry(
                JsonUtils.getString(json, "path", ""),
                JsonUtils.getString(json, "originalCq", ""),
                JsonUtils.getString(json, "sourceUrl", ""),
                JsonUtils.getString(json, "description", ""),
                JsonUtils.getString(json, "scene", ""),
                JsonUtils.getString(json, "keywords", ""),
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
                    return new StickerSource(url, originalCq, type);
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
    private static byte[] downloadStickerBytes(@Nonnull String url, int maxBytes) {
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
                if (Files.isRegularFile(path) && Files.size(path) <= maxBytes) {
                    return Files.readAllBytes(path);
                }
                return null;
            }
            if (Files.isRegularFile(Path.of(trimmed)) && Files.size(Path.of(trimmed)) <= maxBytes) {
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

    private boolean saveStickerMemory(long botId,
                                      long groupId,
                                      long senderId,
                                      long sourceMsgId,
                                      @Nonnull String sourceUrl,
                                      @Nonnull String originalCq,
                                      @Nonnull byte[] bytes,
                                      @Nonnull String description,
                                      @Nonnull String scene,
                                      @Nonnull String keywords,
                                      @Nonnull String extraTag) {
        String cachePath = BaniraUtils.saveFileToCachePath(bytes, EnumCacheFileType.image);
        if (StringUtils.isNullOrEmptyEx(cachePath)) {
            return false;
        }
        Map<String, Object> payload = new LinkedHashMap<>();
        payload.put("path", cachePath);
        payload.put("originalCq", originalCq);
        payload.put("sourceUrl", sourceUrl);
        payload.put("description", description);
        payload.put("scene", scene);
        payload.put("keywords", keywords);
        payload.put("sourceSender", senderId);
        payload.put("sourceGroup", groupId);
        payload.put("sourceMsgId", sourceMsgId);
        payload.put("size", bytes.length);
        String content = STICKER_PREFIX + JsonUtils.toJsonString(payload);
        long now = DateUtils.getTimestamp(new Date());
        AiMemory memory = new AiMemory()
                .setBotId(botId)
                .setGroupId(groupId)
                .setUserId(0L)
                .setContent(content)
                .setTags(normalizeTags(STICKER_TAG + "," + extraTag))
                .setSourceMsgId(String.valueOf(sourceMsgId))
                .setCreatedAt(now)
                .setLastUsedAt(now);
        aiMemoryManager.addMemory(memory);
        return true;
    }

    private static boolean looksLikeAutoCollectStickerImage(@Nonnull List<ArrayMsg> msgs, @Nonnull byte[] bytes) {
        ImageInfo info = imageInfo(bytes);
        if (!info.valid()) {
            return false;
        }
        String text = plainText(msgs).replaceAll("\\s+", "");
        if (containsStickerCue(text)) {
            return looksLikeExplicitStickerInfo(info);
        }
        if (info.width() <= MAX_AUTO_IMAGE_SIDE
                && info.height() <= MAX_AUTO_IMAGE_SIDE
                && (long) info.width() * info.height() <= MAX_AUTO_IMAGE_PIXELS
                && info.aspectRatio() >= 0.6
                && info.aspectRatio() <= 1.8) {
            return true;
        }
        return false;
    }

    private static boolean looksLikeExplicitStickerImage(@Nonnull byte[] bytes) {
        ImageInfo info = imageInfo(bytes);
        return info.valid() && looksLikeExplicitStickerInfo(info);
    }

    private static boolean looksLikeExplicitStickerInfo(@Nonnull ImageInfo info) {
        return info.width() <= MAX_EXPLICIT_IMAGE_SIDE
                && info.height() <= MAX_EXPLICIT_IMAGE_SIDE
                && (long) info.width() * info.height() <= MAX_EXPLICIT_IMAGE_PIXELS
                && info.aspectRatio() >= 0.35
                && info.aspectRatio() <= 2.8;
    }

    @Nonnull
    private static ImageInfo imageInfo(@Nonnull byte[] bytes) {
        try {
            BufferedImage image = ImageIO.read(new ByteArrayInputStream(bytes));
            if (image == null) {
                return ImageInfo.invalid();
            }
            return new ImageInfo(image.getWidth(), image.getHeight());
        } catch (Exception e) {
            return ImageInfo.invalid();
        }
    }

    private static boolean containsStickerCue(@Nonnull String text) {
        return StringUtils.isNotNullOrEmpty(text)
                && (text.contains("表情")
                || text.contains("贴纸")
                || text.contains("收藏")
                || text.contains("收下")
                || text.contains("拿去")
                || text.contains("用这个")
                || text.contains("发这个"));
    }

    @Nonnull
    private static StickerMetadata analyzeStickerMetadata(@Nonnull String entryName,
                                                          @Nonnull byte[] bytes,
                                                          @Nonnull ImageInfo info,
                                                          @Nullable String description,
                                                          @Nullable String scene,
                                                          @Nullable StickerVisionAnalyzer analyzer) {
        StickerMetadata fallback = fallbackStickerMetadata(entryName, info, description, scene);
        if (analyzer == null) {
            return fallback;
        }
        StickerMetadata vision = analyzer.analyze(entryName, bytes, info);
        return vision != null ? fallback.merge(vision) : fallback;
    }

    @Nonnull
    static StickerMetadata fallbackStickerMetadata(@Nonnull String entryName,
                                                   @Nonnull ImageInfo info,
                                                   @Nullable String description,
                                                   @Nullable String scene) {
        String safeName = safeArchiveEntryName(entryName);
        LinkedHashSet<String> keywords = new LinkedHashSet<>();
        addNameTokens(keywords, entryName);
        addCueKeywords(keywords, entryName);
        if (info.valid()) {
            keywords.add(info.width() + "x" + info.height());
            if (info.aspectRatio() > 1.8) {
                keywords.add("横图");
            } else if (info.aspectRatio() < 0.6) {
                keywords.add("竖图");
            } else {
                keywords.add("方图");
            }
        }
        if (entryName.toLowerCase(Locale.ROOT).endsWith(".gif")) {
            keywords.add("动图");
        }
        if (keywords.isEmpty()) {
            keywords.add("表情包");
        }
        List<String> visible = keywords.stream()
                .filter(keyword -> !keyword.matches("\\d+x\\d+"))
                .limit(5)
                .toList();
        String inferred = visible.isEmpty()
                ? "从压缩包导入的表情包：" + safeName
                : "从压缩包导入的表情包：" + safeName + "；标签：" + String.join("、", visible);
        String inferredScene = inferScene(keywords);
        return new StickerMetadata(
                mergeMetadataText(description, inferred),
                mergeMetadataText(scene, inferredScene),
                joinKeywords(keywords),
                normalizeTags("label:" + joinKeywords(keywords))
        );
    }

    @Nonnull
    static StickerMetadata fallbackStickerMetadata(@Nonnull String entryName) {
        return fallbackStickerMetadata(entryName, ImageInfo.invalid(), "", "");
    }

    @Nonnull
    private static String inferScene(@Nonnull Set<String> keywords) {
        if (containsAnyKeyword(keywords, "疑问", "问号", "困惑", "不解")) {
            return "适合表达疑问、困惑、反问或看不懂的时候使用";
        }
        if (containsAnyKeyword(keywords, "震惊", "惊讶", "害怕")) {
            return "适合表达震惊、意外或被吓到的时候使用";
        }
        if (containsAnyKeyword(keywords, "无语", "尴尬", "汗", "沉默")) {
            return "适合无语、尴尬、冷场或不知道怎么接话时使用";
        }
        if (containsAnyKeyword(keywords, "开心", "笑", "乐", "大笑")) {
            return "适合开心、接梗、调侃或气氛轻松时使用";
        }
        if (containsAnyKeyword(keywords, "生气", "愤怒", "怒")) {
            return "适合表达生气、不满或轻度炸毛时使用";
        }
        if (containsAnyKeyword(keywords, "难过", "哭", "委屈")) {
            return "适合表达难过、委屈、破防或卖惨时使用";
        }
        if (containsAnyKeyword(keywords, "赞同", "可以", "ok", "牛", "666")) {
            return "适合表示赞同、认可、可以或夸一句时使用";
        }
        if (containsAnyKeyword(keywords, "安慰", "抱抱", "摸摸")) {
            return "适合安慰、哄人或缓和语气时使用";
        }
        if (containsAnyKeyword(keywords, "睡觉", "困", "晚安")) {
            return "适合表达困了、想睡、晚安或懒得动时使用";
        }
        if (containsAnyKeyword(keywords, "盯", "观察", "看")) {
            return "适合盯人、观察、等对方解释或轻轻施压时使用";
        }
        return "适合轻松接梗、简短回应或需要用表情包带过时使用";
    }

    private static boolean containsAnyKeyword(@Nonnull Set<String> keywords, String... needles) {
        for (String keyword : keywords) {
            for (String needle : needles) {
                if (keyword.contains(needle) || needle.contains(keyword)) {
                    return true;
                }
            }
        }
        return false;
    }

    private static void addNameTokens(@Nonnull LinkedHashSet<String> keywords, @Nonnull String entryName) {
        String normalized = entryName.replace("\\", "/");
        String withoutExt = normalized.replaceAll("(?i)\\.(png|jpe?g|gif|webp)$", "");
        for (String part : withoutExt.split("[/\\\\._\\-\\s\\[\\]【】()（）]+")) {
            String token = cleanKeyword(part);
            if (isUsefulKeyword(token)) {
                keywords.add(token);
            }
        }
    }

    private static void addCueKeywords(@Nonnull LinkedHashSet<String> keywords, @Nonnull String text) {
        String lower = text.toLowerCase(Locale.ROOT);
        addIfContains(keywords, lower, "疑问", "问号", "困惑", "不解", "疑惑", "what", "why", "huh", "？", "?");
        addIfContains(keywords, lower, "无语", "沉默", "汗", "尴尬", "呆", "地铁老人", "speechless");
        addIfContains(keywords, lower, "震惊", "惊讶", "害怕", "惊恐", "啊", "shock", "surprise");
        addIfContains(keywords, lower, "开心", "笑", "大笑", "乐", "哈哈", "喜", "happy", "laugh");
        addIfContains(keywords, lower, "生气", "愤怒", "怒", "气", "angry");
        addIfContains(keywords, lower, "难过", "哭", "委屈", "破防", "悲", "cry", "sad");
        addIfContains(keywords, lower, "赞同", "可以", "ok", "牛", "666", "赞", "good");
        addIfContains(keywords, lower, "拒绝", "不要", "不行", "达咩", "no");
        addIfContains(keywords, lower, "安慰", "抱抱", "摸摸", "pat", "hug");
        addIfContains(keywords, lower, "睡觉", "困", "晚安", "sleep");
        addIfContains(keywords, lower, "盯", "观察", "看", "盯着", "watch");
        addIfContains(keywords, lower, "吃瓜", "围观", "瓜");
        addIfContains(keywords, lower, "吐槽", "草", "绷", "乐子");
    }

    private static void addIfContains(@Nonnull LinkedHashSet<String> keywords,
                                      @Nonnull String haystack,
                                      @Nonnull String label,
                                      String... cues) {
        for (String cue : cues) {
            if (StringUtils.isNotNullOrEmpty(cue) && haystack.contains(cue.toLowerCase(Locale.ROOT))) {
                keywords.add(label);
                for (String synonym : stickerSynonyms(label)) {
                    keywords.add(synonym);
                }
                return;
            }
        }
    }

    @Nonnull
    private static List<String> expandStickerKeyword(@Nonnull String keyword) {
        LinkedHashSet<String> terms = new LinkedHashSet<>();
        String normalized = keyword.trim();
        terms.add(normalized);
        for (String token : normalized.split("[\\s,，、;；]+")) {
            String cleaned = cleanKeyword(token);
            if (isUsefulKeyword(cleaned)) {
                terms.add(cleaned);
                terms.addAll(stickerSynonyms(cleaned));
            }
        }
        terms.addAll(stickerSynonyms(normalized));
        return terms.stream().filter(StringUtils::isNotNullOrEmpty).limit(10).toList();
    }

    @Nonnull
    private static List<String> stickerSynonyms(@Nonnull String keyword) {
        String compact = keyword.replaceAll("\\s+", "").toLowerCase(Locale.ROOT);
        if (containsAnyText(compact, "疑问", "问号", "困惑", "疑惑", "不解", "what", "why", "huh", "？", "?")) {
            return List.of("疑问", "问号", "困惑", "疑惑", "不解", "反问");
        }
        if (containsAnyText(compact, "无语", "尴尬", "汗", "沉默", "speechless")) {
            return List.of("无语", "尴尬", "汗", "沉默");
        }
        if (containsAnyText(compact, "震惊", "惊讶", "惊恐", "害怕", "shock", "surprise")) {
            return List.of("震惊", "惊讶", "惊恐", "害怕");
        }
        if (containsAnyText(compact, "开心", "笑", "乐", "哈哈", "happy", "laugh")) {
            return List.of("开心", "笑", "大笑", "乐", "接梗");
        }
        if (containsAnyText(compact, "生气", "愤怒", "怒", "angry")) {
            return List.of("生气", "愤怒", "怒", "不满");
        }
        if (containsAnyText(compact, "难过", "哭", "委屈", "破防", "sad", "cry")) {
            return List.of("难过", "哭", "委屈", "破防");
        }
        if (containsAnyText(compact, "赞同", "可以", "ok", "牛", "666", "赞", "good")) {
            return List.of("赞同", "可以", "ok", "牛", "666", "认可");
        }
        if (containsAnyText(compact, "安慰", "抱抱", "摸摸", "pat", "hug")) {
            return List.of("安慰", "抱抱", "摸摸", "哄人");
        }
        if (containsAnyText(compact, "拒绝", "不要", "不行", "no", "达咩")) {
            return List.of("拒绝", "不要", "不行", "达咩");
        }
        return List.of();
    }

    private static boolean containsAnyText(@Nonnull String text, String... needles) {
        for (String needle : needles) {
            if (StringUtils.isNotNullOrEmpty(needle) && text.contains(needle.toLowerCase(Locale.ROOT))) {
                return true;
            }
        }
        return false;
    }

    @Nonnull
    private static String cleanKeyword(@Nullable String value) {
        return StringUtils.nullToEmpty(value)
                .replaceAll("[\"'`，。！？、：:；;（）()\\[\\]{}<>《》|/\\\\]+", "")
                .trim();
    }

    private static boolean isUsefulKeyword(@Nonnull String token) {
        if (StringUtils.isNullOrEmptyEx(token)) {
            return false;
        }
        if (token.length() > 24) {
            return false;
        }
        return !token.matches("\\d+");
    }

    @Nonnull
    private static String mergeMetadataText(@Nullable String base, @Nonnull String generated) {
        String normalizedBase = AiTextLimits.truncate(StringUtils.nullToEmpty(base).trim(), MAX_METADATA_TEXT);
        String normalizedGenerated = AiTextLimits.truncate(generated.trim(), MAX_METADATA_TEXT);
        if (StringUtils.isNullOrEmptyEx(normalizedBase)) {
            return normalizedGenerated;
        }
        if (normalizedBase.contains(normalizedGenerated)) {
            return normalizedBase;
        }
        return AiTextLimits.truncate(normalizedBase + "；" + normalizedGenerated, MAX_METADATA_TEXT);
    }

    @Nonnull
    private static String joinKeywords(@Nonnull Collection<String> keywords) {
        return keywords.stream()
                .map(ChatStickerService::cleanKeyword)
                .filter(ChatStickerService::isUsefulKeyword)
                .distinct()
                .limit(MAX_KEYWORD_COUNT)
                .reduce((a, b) -> a + "," + b)
                .orElse("表情包");
    }

    @Nonnull
    private static String normalizeTags(@Nullable String tags) {
        LinkedHashSet<String> normalized = new LinkedHashSet<>();
        for (String part : StringUtils.nullToEmpty(tags).split("[,，\\s]+")) {
            String tag = StringUtils.nullToEmpty(part)
                    .replaceAll("[\"'`，。！？、；;（）()\\[\\]{}<>《》|/\\\\]+", "")
                    .trim();
            if (StringUtils.isNotNullOrEmpty(tag)) {
                normalized.add(tag);
            }
        }
        return String.join(",", normalized);
    }

    @Nonnull
    static String resolveArchiveSource(@Nonnull AgentContext ctx,
                                       @Nullable String source,
                                       @Nullable IMessageRecordManager messageRecordManager) {
        String explicit = firstDownloadableSource(StringUtils.nullToEmpty(source));
        if (StringUtils.isNotNullOrEmpty(explicit)) {
            return explicit;
        }
        String current = firstDownloadableSource(ctx.userMessage());
        if (StringUtils.isNotNullOrEmpty(current)) {
            return current;
        }
        if (ctx.messageContext() != null && CollectionUtils.isNotNullOrEmpty(ctx.messageContext().originalMsg())) {
            String fromCurrent = firstDownloadableSource(ctx.messageContext().originalMsg());
            if (StringUtils.isNotNullOrEmpty(fromCurrent)) {
                return fromCurrent;
            }
            String fromQuotedRecord = firstQuotedArchiveSource(ctx, messageRecordManager);
            if (StringUtils.isNotNullOrEmpty(fromQuotedRecord)) {
                return fromQuotedRecord;
            }
            try {
                List<ArrayMsg> replyContent = ctx.bot().getReplyContent(ctx.messageContext().originalMsg());
                String fromReply = firstDownloadableSource(replyContent);
                if (StringUtils.isNotNullOrEmpty(fromReply)) {
                    return fromReply;
                }
            } catch (Exception ignored) {
            }
        }
        String recent = firstRecentArchiveSource(ctx, messageRecordManager);
        if (StringUtils.isNotNullOrEmpty(recent)) {
            return recent;
        }
        return "";
    }

    @Nonnull
    private static String firstQuotedArchiveSource(@Nonnull AgentContext ctx,
                                                   @Nullable IMessageRecordManager messageRecordManager) {
        if (messageRecordManager == null
                || ctx.messageContext() == null
                || CollectionUtils.isNullOrEmpty(ctx.messageContext().originalMsg())) {
            return "";
        }
        Long replyId = BaniraUtils.getReplyId(ctx.messageContext().originalMsg());
        if (replyId == null || replyId <= 0 || replyId > Integer.MAX_VALUE) {
            return "";
        }
        MessageRecord record = null;
        try {
            if (ctx.msgType() == EnumMessageType.GROUP && ctx.scopeGroupId() > 0) {
                record = messageRecordManager.getGroupMessageRecord(ctx.scopeGroupId(), replyId.intValue());
            } else if (ctx.senderId() != null && ctx.senderId() > 0) {
                record = messageRecordManager.getPrivateMessageRecord(ctx.senderId(), replyId.intValue());
            }
        } catch (Exception ignored) {
        }
        return firstDownloadableSource(record);
    }

    @Nonnull
    private static String firstRecentArchiveSource(@Nonnull AgentContext ctx,
                                                   @Nullable IMessageRecordManager messageRecordManager) {
        if (messageRecordManager == null || ctx.msgType() != EnumMessageType.GROUP || ctx.scopeGroupId() <= 0) {
            return "";
        }
        try {
            MessageRecordQueryParam param = new MessageRecordQueryParam(true, 1, 12)
                    .setBotId(ctx.botId())
                    .setGroupId(ctx.scopeGroupId())
                    .setMsgType(EnumMessageType.GROUP.name())
                    .setRecalled(false);
            param.addOrderBy(MessageRecordQueryParam.ORDER_ID, false);
            for (MessageRecord record : messageRecordManager.getMessageRecordList(param)) {
                if (record == null || Objects.equals(record.getMsgId(), ctx.msgId())) {
                    continue;
                }
                String resolved = firstDownloadableSource(record);
                if (StringUtils.isNotNullOrEmpty(resolved) && isArchiveSource(resolved)) {
                    return resolved;
                }
            }
        } catch (Exception ignored) {
        }
        return "";
    }

    @Nonnull
    private static String firstDownloadableSource(@Nullable MessageRecord record) {
        if (record == null || record.recalled()) {
            return "";
        }
        String fromRecode = firstDownloadableSource(record.getMsgRecode());
        if (StringUtils.isNotNullOrEmpty(fromRecode)) {
            return fromRecode;
        }
        String fromRawText = firstDownloadableSource(record.getMsgRaw());
        if (StringUtils.isNotNullOrEmpty(fromRawText)) {
            return fromRawText;
        }
        String fromRecodeArray = firstDownloadableSource(safeStringToArray(record.getMsgRecode()));
        if (StringUtils.isNotNullOrEmpty(fromRecodeArray)) {
            return fromRecodeArray;
        }
        return firstDownloadableSource(safeStringToArray(record.getMsgRaw()));
    }

    @Nonnull
    private static String firstDownloadableSource(@Nullable String text) {
        String value = StringUtils.nullToEmpty(text);
        java.util.regex.Matcher matcher = java.util.regex.Pattern
                .compile("(?i)(https?://[^\\s,，)）\\]]+|file:/[^\\s,，)）\\]]+|[A-Za-z]:[/\\\\][^\\s\\]]+)")
                .matcher(value);
        return matcher.find() ? trimSource(matcher.group(1)) : "";
    }

    @Nonnull
    private static String firstDownloadableSource(@Nullable List<ArrayMsg> msgs) {
        if (CollectionUtils.isNullOrEmpty(msgs)) {
            return "";
        }
        for (ArrayMsg msg : msgs) {
            if (msg == null) {
                continue;
            }
            String url = msg.getStringData("url");
            if (StringUtils.isNotNullOrEmpty(url)) {
                return trimSource(url);
            }
            String file = msg.getStringData("file");
            if (StringUtils.isNotNullOrEmpty(file)) {
                return trimSource(file);
            }
            String fileId = msg.getStringData("file_id");
            if (StringUtils.isNotNullOrEmpty(fileId) && isArchiveSource(fileId)) {
                return trimSource(fileId);
            }
            String name = msg.getStringData("name");
            if (StringUtils.isNotNullOrEmpty(name) && isArchiveSource(name)) {
                String fallback = firstDownloadableSource(msg.toCQCode());
                if (StringUtils.isNotNullOrEmpty(fallback)) {
                    return fallback;
                }
            }
        }
        return "";
    }

    @Nonnull
    private static List<ArrayMsg> safeStringToArray(@Nullable String source) {
        if (StringUtils.isNullOrEmptyEx(source)) {
            return Collections.emptyList();
        }
        try {
            return MessageConverser.stringToArray(source);
        } catch (Exception ignored) {
            return Collections.emptyList();
        }
    }

    @Nonnull
    private static String trimSource(@Nonnull String source) {
        return source.trim().replaceAll("[,，。)）\\]]+$", "");
    }

    private static boolean isArchiveSource(@Nonnull String source) {
        return source.toLowerCase(java.util.Locale.ROOT).split("[?#]", 2)[0].endsWith(".zip");
    }

    private static boolean isImageFileName(@Nonnull String name) {
        String lower = name.toLowerCase(java.util.Locale.ROOT);
        return lower.endsWith(".png")
                || lower.endsWith(".jpg")
                || lower.endsWith(".jpeg")
                || lower.endsWith(".gif")
                || lower.endsWith(".webp");
    }

    @Nonnull
    private static byte[] readZipEntry(@Nonnull ZipInputStream zip, int maxBytes) throws java.io.IOException {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        byte[] buffer = new byte[8192];
        int total = 0;
        int read;
        while ((read = zip.read(buffer)) >= 0) {
            total += read;
            if (total > maxBytes) {
                break;
            }
            out.write(buffer, 0, read);
        }
        return out.toByteArray();
    }

    @Nonnull
    private static String safeArchiveEntryName(@Nonnull String name) {
        String normalized = name.replace("\\", "/");
        int slash = normalized.lastIndexOf('/');
        if (slash >= 0) {
            normalized = normalized.substring(slash + 1);
        }
        return AiTextLimits.truncate(normalized, 40);
    }

    record StickerMetadata(String description,
                           String scene,
                           String keywords,
                           String tagCsv) {

        @Nonnull
        StickerMetadata merge(@Nonnull StickerMetadata other) {
            LinkedHashSet<String> mergedKeywords = new LinkedHashSet<>();
            mergedKeywords.addAll(Arrays.asList(StringUtils.nullToEmpty(keywords).split(",")));
            mergedKeywords.addAll(Arrays.asList(StringUtils.nullToEmpty(other.keywords()).split(",")));
            return new StickerMetadata(
                    mergeMetadataText(other.description(), description),
                    mergeMetadataText(other.scene(), scene),
                    joinKeywords(mergedKeywords),
                    normalizeTags(tagCsv + "," + other.tagCsv())
            );
        }
    }

    private static final class StickerVisionAnalyzer {

        private final ChatModelRouter router;

        private StickerVisionAnalyzer(@Nonnull ChatModelRouter router) {
            this.router = router;
        }

        @Nullable
        static StickerVisionAnalyzer create(@Nullable ChatConfig config) {
            if (config == null || config.model() == null || !config.model().imageInputEnabled() || !ChatConfigSupport.isModelReady(config)) {
                return null;
            }
            try {
                return new StickerVisionAnalyzer(new ChatModelRouter(ChatConfigSupport.normalize(config)));
            } catch (Exception e) {
                LOGGER.debug("AI sticker vision analyzer unavailable: {}", e.getMessage());
                return null;
            }
        }

        @Nullable
        StickerMetadata analyze(@Nonnull String entryName, @Nonnull byte[] bytes, @Nonnull ImageInfo info) {
            ImageContent image = MessageConvert.imageContentFromBytes(bytes);
            if (image == null) {
                return null;
            }
            try {
                List<Content> contents = new ArrayList<>();
                contents.add(new TextContent("""
                        请分析这张将作为 QQ 群聊表情包收藏的图片。
                        只返回 JSON，不要 Markdown，不要解释。
                        字段：
                        description：一句中文描述图里主体、表情或文字，20字以内
                        scene：一句中文使用场景，说明适合表达什么情绪或回应什么话，35字以内
                        keywords：中文关键词数组，3到8个，包含情绪、动作、文字梗、同义词
                        文件名：%s
                        尺寸：%dx%d
                        """.formatted(safeArchiveEntryName(entryName), info.width(), info.height())));
                contents.add(image);
                String text = router.chat(List.of(UserMessage.userMessage("sticker_import", contents))).aiMessage().text();
                return parseVisionMetadata(text);
            } catch (Exception e) {
                LOGGER.debug("AI sticker vision analysis failed entry={}: {}", entryName, e.getMessage());
                return null;
            }
        }

        @Nullable
        private static StickerMetadata parseVisionMetadata(@Nullable String text) {
            String jsonText = StringUtils.nullToEmpty(text).trim();
            Matcher matcher = JSON_OBJECT_PATTERN.matcher(jsonText);
            if (matcher.find()) {
                jsonText = matcher.group();
            }
            JsonObject json = JsonUtils.parseJsonObject(jsonText);
            if (json == null) {
                return null;
            }
            String description = cleanMetadataSentence(JsonUtils.getString(json, "description", ""));
            String scene = cleanMetadataSentence(JsonUtils.getString(json, "scene", ""));
            LinkedHashSet<String> keywords = new LinkedHashSet<>();
            String rawKeywords = JsonUtils.getString(json, "keywords", "");
            rawKeywords = rawKeywords.replaceAll("[\\[\\]\"]", "");
            for (String part : rawKeywords.split("[,，、\\s]+")) {
                String keyword = cleanKeyword(part);
                if (isUsefulKeyword(keyword)) {
                    keywords.add(keyword);
                    keywords.addAll(stickerSynonyms(keyword));
                }
            }
            if (StringUtils.isNullOrEmptyEx(description) || StringUtils.isNullOrEmptyEx(scene) || keywords.isEmpty()) {
                return null;
            }
            String keywordText = joinKeywords(keywords);
            return new StickerMetadata(description, scene, keywordText, "vision:llm,label:" + keywordText);
        }
    }

    @Nonnull
    private static String cleanMetadataSentence(@Nullable String text) {
        return AiTextLimits.truncate(StringUtils.nullToEmpty(text)
                .replaceAll("[\\r\\n\\t]+", " ")
                .replaceAll("\\s+", " ")
                .trim(), MAX_METADATA_TEXT);
    }

    private record StickerSource(String url, String originalCq, MsgTypeEnum type) {
    }

    private record ImageInfo(int width, int height) {
        static ImageInfo invalid() {
            return new ImageInfo(0, 0);
        }

        boolean valid() {
            return width > 0 && height > 0;
        }

        double aspectRatio() {
            return height == 0 ? 0 : width / (double) height;
        }
    }

    private record StickerEntry(String path,
                                String originalCq,
                                String sourceUrl,
                                String description,
                                String scene,
                                String keywords,
                                String sourceMsgId,
                                long size) {

        static StickerEntry empty() {
            return new StickerEntry("", "", "", "", "", "", "", 0);
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
            return new StickerEntry(path, originalCq, sourceUrl, description, scene, keywords, sourceMsgId, size);
        }

        @Nonnull
        String toJson() {
            Map<String, Object> payload = new LinkedHashMap<>();
            payload.put("path", StringUtils.nullToEmpty(path));
            payload.put("originalCq", StringUtils.nullToEmpty(originalCq));
            payload.put("sourceUrl", StringUtils.nullToEmpty(sourceUrl));
            payload.put("description", StringUtils.nullToEmpty(description));
            payload.put("scene", StringUtils.nullToEmpty(scene));
            payload.put("keywords", StringUtils.nullToEmpty(keywords));
            payload.put("sourceMsgId", StringUtils.nullToEmpty(sourceMsgId));
            payload.put("size", size);
            return JsonUtils.toJsonString(payload);
        }
    }

}
