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

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.ThreadLocalRandom;
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
    private static final int MAX_ARCHIVE_BYTES = 50 * 1024 * 1024;
    private static final int MAX_ARCHIVE_ENTRIES = 120;
    private static final int MAX_AUTO_IMAGE_SIDE = 320;
    private static final int MAX_AUTO_IMAGE_PIXELS = 320 * 320;
    private static final int MAX_EXPLICIT_IMAGE_SIDE = 900;
    private static final int MAX_EXPLICIT_IMAGE_PIXELS = 900 * 900;
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
        String description = StringUtils.isNotNullOrEmpty(plainText)
                ? "带文字说明的表情包：" + plainText
                : "自动收集的表情包";
        String scene = StringUtils.isNotNullOrEmpty(plainText)
                ? "适合回应类似“" + AiTextLimits.truncate(plainText, 40) + "”的语境"
                : "适合需要用表情包简单回应的语境";
        if (!saveStickerMemory(bot.getSelfId(), groupId, event.getUserId(), event.getMessageId(),
                source.url(), message, bytes, description, scene, "source:auto_collect")) {
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
        if (!BaniraUtils.hasKanriOperatorAccess(ctx.bot(), ctx.scopeGroupId(), ctx.senderId())) {
            return ownerOnlyText("表情包压缩包导入");
        }
        String resolved = resolveArchiveSource(ctx, source);
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
        long sourceMsgId = StringUtils.toLong(ctx.msgId(), 0L);
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
                if (bytes.length == 0 || bytes.length > MAX_STICKER_BYTES
                        || !looksLikeExplicitStickerImage(bytes)) {
                    skipped++;
                    continue;
                }
                String desc = StringUtils.isNotNullOrEmpty(description)
                        ? description.trim()
                        : "从压缩包导入的表情包：" + safeArchiveEntryName(name);
                String useScene = StringUtils.isNotNullOrEmpty(scene)
                        ? scene.trim()
                        : "适合轻松接梗或简短回应时使用";
                String sourceUrl = resolved + "#" + name;
                if (stickerExists(ctx.botId(), ctx.scopeGroupId(), sourceUrl)) {
                    skipped++;
                    continue;
                }
                boolean ok = saveStickerMemory(ctx.botId(), ctx.scopeGroupId(), ctx.senderId(),
                        sourceMsgId, sourceUrl, "", bytes, desc, useScene, "source:archive");
                if (ok) {
                    saved++;
                } else {
                    skipped++;
                }
            }
        } catch (Exception e) {
            LOGGER.warn("AI collect sticker archive failed group={} user={} source={}",
                    ctx.scopeGroupId(), ctx.senderId(), resolved, e);
            return "压缩包解压失败。";
        }
        LOGGER.debug("AI collected sticker archive group={} user={} scanned={} saved={} skipped={} source={}",
                ctx.scopeGroupId(), ctx.senderId(), scanned, saved, skipped, resolved);
        return saved > 0
                ? "已导入 " + saved + " 个表情包，跳过 " + skipped + " 个。"
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
                                      @Nonnull String extraTag) {
        String cachePath = BaniraUtils.saveFileToCachePath(bytes, EnumCacheFileType.image);
        if (StringUtils.isNullOrEmptyEx(cachePath)) {
            return false;
        }
        String content = STICKER_PREFIX + JsonUtils.toJsonString(Map.of(
                "path", cachePath,
                "originalCq", originalCq,
                "sourceUrl", sourceUrl,
                "description", description,
                "scene", scene,
                "sourceSender", senderId,
                "sourceGroup", groupId,
                "sourceMsgId", sourceMsgId,
                "size", bytes.length
        ));
        long now = DateUtils.getTimestamp(new Date());
        AiMemory memory = new AiMemory()
                .setBotId(botId)
                .setGroupId(groupId)
                .setUserId(0L)
                .setContent(content)
                .setTags(STICKER_TAG + "," + extraTag)
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
    private static String resolveArchiveSource(@Nonnull AgentContext ctx, @Nullable String source) {
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
            try {
                List<ArrayMsg> replyContent = ctx.bot().getReplyContent(ctx.messageContext().originalMsg());
                String fromReply = firstDownloadableSource(replyContent);
                if (StringUtils.isNotNullOrEmpty(fromReply)) {
                    return fromReply;
                }
            } catch (Exception ignored) {
            }
        }
        return "";
    }

    @Nonnull
    private static String firstDownloadableSource(@Nullable String text) {
        String value = StringUtils.nullToEmpty(text);
        java.util.regex.Matcher matcher = java.util.regex.Pattern
                .compile("(?i)(https?://\\S+|file:/\\S+|[A-Za-z]:[/\\\\][^\\s\\]]+)")
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
        }
        return "";
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
