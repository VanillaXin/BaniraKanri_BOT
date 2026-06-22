package xin.vanilla.banira.plugin.chat;

import com.google.gson.JsonObject;
import com.mikuac.shiro.common.utils.MessageConverser;
import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.dto.action.common.ActionData;
import com.mikuac.shiro.dto.action.response.*;
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
    private static final int MAX_COLLECT_IMAGES_PER_REQUEST = 60;
    private static final int MAX_FORWARD_DEPTH = 3;
    private static final int MAX_FORWARD_NODES = 80;
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
        long groupId = BaniraUtils.isGroupIdValid(event.getGroupId()) ? event.getGroupId() : 0L;
        String plainText = plainText(event.getArrayMsg());
        List<StickerImageCandidate> candidates = collectImageCandidates(bot, groupId, event.getArrayMsg(),
                event.getMessageId(), 0, true, MAX_COLLECT_IMAGES_PER_REQUEST);
        int saved = 0;
        int skipped = 0;
        for (StickerImageCandidate candidate : candidates) {
            if (candidate == null || StringUtils.isNullOrEmptyEx(candidate.sourceUrl())
                    || !isDownloadableImageSource(candidate.sourceUrl())
                    || stickerExists(bot.getSelfId(), groupId, candidate.sourceUrl())) {
                skipped++;
                continue;
            }
            byte[] bytes = downloadStickerBytes(candidate.sourceUrl(), MAX_STICKER_BYTES);
            if (bytes == null || bytes.length == 0 || bytes.length > MAX_STICKER_BYTES) {
                skipped++;
                LOGGER.debug("AI ignored sticker group={} user={} msgId={} bytes={}",
                        groupId, event.getUserId(), event.getMessageId(), bytes != null ? bytes.length : -1);
                continue;
            }
            if (!looksLikeAutoCollectStickerImage(event.getArrayMsg(), bytes)) {
                skipped++;
                LOGGER.debug("AI ignored non-sticker image group={} user={} msgId={} sourceType={} bytes={}",
                        groupId, event.getUserId(), event.getMessageId(), candidate.sourceType(), bytes.length);
                continue;
            }
            StickerMetadata metadata = fallbackStickerMetadata(
                    StringUtils.isNotNullOrEmpty(plainText) ? plainText : candidate.name(),
                    imageInfo(bytes),
                    StringUtils.isNotNullOrEmpty(plainText) ? "带文字说明的表情包：" + plainText : "",
                    StringUtils.isNotNullOrEmpty(plainText) ? "适合回应类似“" + AiTextLimits.truncate(plainText, 40) + "”的语境" : ""
            );
            if (saveStickerMemory(bot.getSelfId(), groupId, event.getUserId(), event.getMessageId(),
                    candidate.sourceUrl(), candidate.originalCq(), bytes, metadata.description(), metadata.scene(), metadata.keywords(),
                    "source:auto_collect," + metadata.tagCsv())) {
                saved++;
            } else {
                skipped++;
            }
        }
        if (saved > 0) {
            LOGGER.debug("AI collected stickers group={} user={} msgId={} saved={} skipped={} chars={}",
                    groupId, event.getUserId(), event.getMessageId(), saved, skipped, message.length());
        }
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
        ArchiveSource archiveSource = resolveArchiveCandidate(ctx, source, messageRecordManager);
        if (archiveSource == null || StringUtils.isNullOrEmptyEx(archiveSource.downloadSource())) {
            return "没有找到可下载的压缩包链接。请直接发 zip 链接，或回复包含文件链接的消息再让我导入。";
        }
        String resolved = archiveSource.downloadSource();
        if (!isArchiveSource(resolved)) {
            return "只支持 zip 压缩包。";
        }
        byte[] archive = downloadArchiveBytes(ctx, archiveSource, MAX_ARCHIVE_BYTES);
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
    public String collectStickerImage(@Nonnull AgentContext ctx,
                                      @Nullable String source,
                                      @Nullable String description,
                                      @Nullable String scene,
                                      @Nullable ChatConfig chatConfig) {
        List<StickerImageCandidate> candidates = resolveStickerImageSources(ctx, source, messageRecordManager);
        if (candidates.isEmpty()) {
            return "没找到能收藏的表情。请直接发图、引用图片/合并转发，或在刚发过图片后再说让我收藏。";
        }
        long groupId = ctx.scopeGroupId();
        StickerVisionAnalyzer analyzer = StickerVisionAnalyzer.create(chatConfig);
        int saved = 0;
        int duplicated = 0;
        int skipped = 0;
        for (StickerImageCandidate candidate : candidates) {
            if (candidate == null || StringUtils.isNullOrEmptyEx(candidate.sourceUrl()) || !isDownloadableImageSource(candidate.sourceUrl())) {
                skipped++;
                continue;
            }
            byte[] bytes = downloadStickerBytes(candidate.sourceUrl(), MAX_STICKER_BYTES);
            ImageInfo info = bytes != null ? imageInfo(bytes) : ImageInfo.invalid();
            if (bytes == null || bytes.length == 0 || bytes.length > MAX_STICKER_BYTES
                    || !info.valid()
                    || !looksLikeExplicitStickerInfo(info)) {
                skipped++;
                continue;
            }
            String sourceUrl = candidate.sourceUrl() + (candidate.sourceMsgId() > 0 ? "#msg=" + candidate.sourceMsgId() : "");
            if (stickerExists(ctx.botId(), groupId, sourceUrl)) {
                duplicated++;
                continue;
            }
            StickerMetadata metadata = analyzeStickerMetadata(candidate.name(), bytes, info, description, scene, analyzer);
            boolean ok = saveStickerMemory(ctx.botId(), groupId, ctx.senderId() != null ? ctx.senderId() : 0L,
                    candidate.sourceMsgId(), sourceUrl, candidate.originalCq(), bytes,
                    metadata.description(), metadata.scene(), metadata.keywords(),
                    "source:requested," + metadata.tagCsv());
            if (ok) {
                saved++;
            } else {
                skipped++;
            }
        }
        LOGGER.debug("AI collected requested stickers group={} user={} candidates={} saved={} duplicated={} skipped={}",
                groupId, ctx.senderId(), candidates.size(), saved, duplicated, skipped);
        if (saved > 0) {
            return "收好了，新增 " + saved + " 张，已补描述和标签。";
        }
        if (duplicated > 0 && skipped == 0) {
            return "这些已经在收藏里了。";
        }
        return "没收进新的表情，可能是重复、太大，或不像表情包。";
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

    @Nullable
    private static byte[] downloadArchiveBytes(@Nonnull AgentContext ctx,
                                               @Nonnull ArchiveSource source,
                                               int maxBytes) {
        if (StringUtils.isNotNullOrEmpty(source.fileId()) && ctx.bot() != null && ctx.scopeGroupId() > 0) {
            byte[] fromFile = downloadGroupFileBytes(ctx.bot(), ctx.scopeGroupId(), source.fileId(), source.busId(), maxBytes);
            if (fromFile != null && fromFile.length > 0) {
                return fromFile;
            }
        }
        byte[] direct = downloadStickerBytes(source.downloadSource(), maxBytes);
        if (direct != null && direct.length > 0) {
            return direct;
        }
        return direct;
    }

    @Nullable
    private static byte[] downloadGroupFileBytes(@Nonnull BaniraBot bot,
                                                 long groupId,
                                                 @Nonnull String fileId,
                                                 int busId,
                                                 int maxBytes) {
        try {
            ActionData<GroupFilesResp> file = bot.getFile(groupId, fileId, busId);
            byte[] bytes = bytesFromGroupFileResp(file, maxBytes);
            if (bytes != null && bytes.length > 0) {
                return bytes;
            }
        } catch (Exception e) {
            LOGGER.debug("AI sticker archive getFile failed group={} fileId={} busId={}", groupId, fileId, busId, e);
        }
        String url = "";
        try {
            ActionData<UrlResp> urlResp = bot.getGroupFileUrl(groupId, fileId, busId);
            if (bot.isActionDataNotEmpty(urlResp) && urlResp.getData() != null) {
                url = StringUtils.nullToEmpty(urlResp.getData().getUrl()).trim();
            }
        } catch (Exception e) {
            LOGGER.debug("AI sticker archive getGroupFileUrl failed group={} fileId={} busId={}", groupId, fileId, busId, e);
        }
        if (StringUtils.isNullOrEmptyEx(url)) {
            return null;
        }
        try {
            ActionData<DownloadFileResp> local = bot.downloadFile(url);
            if (bot.isActionDataNotEmpty(local) && local.getData() != null) {
                byte[] bytes = bytesFromLocalPath(local.getData().getFile(), maxBytes);
                if (bytes != null && bytes.length > 0) {
                    return bytes;
                }
            }
        } catch (Exception e) {
            LOGGER.debug("AI sticker archive downloadFile failed group={} fileId={} url={}", groupId, fileId, url, e);
        }
        return downloadStickerBytes(url, maxBytes);
    }

    @Nullable
    private static byte[] bytesFromGroupFileResp(@Nullable ActionData<GroupFilesResp> resp, int maxBytes) {
        if (resp == null || resp.getData() == null) {
            return null;
        }
        GroupFilesResp data = resp.getData();
        byte[] bytes = bytesFromLocalPath(data.getFile(), maxBytes);
        if (bytes != null && bytes.length > 0) {
            return bytes;
        }
        String base64 = StringUtils.nullToEmpty(data.getBase64()).trim();
        if (StringUtils.isNotNullOrEmpty(base64)) {
            try {
                bytes = Base64.getDecoder().decode(base64);
                return bytes.length <= maxBytes ? bytes : null;
            } catch (Exception ignored) {
            }
        }
        String file = StringUtils.nullToEmpty(data.getFile()).trim();
        if (StringUtils.isNotNullOrEmpty(file) && (file.startsWith("http://") || file.startsWith("https://") || file.startsWith("file:"))) {
            return downloadStickerBytes(file, maxBytes);
        }
        return null;
    }

    @Nullable
    private static byte[] bytesFromLocalPath(@Nullable String pathText, int maxBytes) {
        if (StringUtils.isNullOrEmptyEx(pathText)) {
            return null;
        }
        try {
            Path path = pathText.trim().toLowerCase(Locale.ROOT).startsWith("file:")
                    ? Path.of(URI.create(pathText.trim()))
                    : Path.of(pathText.trim());
            if (Files.isRegularFile(path) && Files.size(path) <= maxBytes) {
                return Files.readAllBytes(path);
            }
        } catch (Exception ignored) {
        }
        return null;
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
        ArchiveSource candidate = resolveArchiveCandidate(ctx, source, messageRecordManager);
        return candidate != null ? candidate.downloadSource() : "";
    }

    @Nullable
    static StickerImageCandidate resolveStickerImageSource(@Nonnull AgentContext ctx,
                                                           @Nullable String source,
                                                           @Nullable IMessageRecordManager messageRecordManager) {
        List<StickerImageCandidate> candidates = resolveStickerImageSources(ctx, source, messageRecordManager);
        return candidates.isEmpty() ? null : candidates.getFirst();
    }

    @Nonnull
    static List<StickerImageCandidate> resolveStickerImageSources(@Nonnull AgentContext ctx,
                                                                  @Nullable String source,
                                                                  @Nullable IMessageRecordManager messageRecordManager) {
        LinkedHashMap<String, StickerImageCandidate> result = new LinkedHashMap<>();
        String explicit = firstDownloadableSource(StringUtils.nullToEmpty(source));
        if (StringUtils.isNotNullOrEmpty(explicit) && isImageSource(explicit)) {
            addImageCandidate(result, new StickerImageCandidate(explicit, explicit, "", 0L, MsgTypeEnum.image));
            return new ArrayList<>(result.values());
        }
        collectImageCandidates(ctx.bot(), ctx.scopeGroupId(), ctx.userMessage(), StringUtils.toLong(ctx.msgId(), 0L),
                true, MAX_COLLECT_IMAGES_PER_REQUEST).forEach(candidate -> addImageCandidate(result, candidate));
        if (ctx.messageContext() != null && CollectionUtils.isNotNullOrEmpty(ctx.messageContext().originalMsg())) {
            collectImageCandidates(ctx.bot(), ctx.scopeGroupId(), ctx.messageContext().originalMsg(),
                    StringUtils.toLong(ctx.msgId(), 0L), 0, true, MAX_COLLECT_IMAGES_PER_REQUEST)
                    .forEach(candidate -> addImageCandidate(result, candidate));
            firstQuotedImageSources(ctx, messageRecordManager)
                    .forEach(candidate -> addImageCandidate(result, candidate));
            try {
                List<ArrayMsg> replyContent = ctx.bot().getReplyContent(ctx.messageContext().originalMsg());
                collectImageCandidates(ctx.bot(), ctx.scopeGroupId(), replyContent, StringUtils.toLong(ctx.msgId(), 0L),
                        0, true, MAX_COLLECT_IMAGES_PER_REQUEST - result.size())
                        .forEach(candidate -> addImageCandidate(result, candidate));
            } catch (Exception ignored) {
            }
        }
        if (result.isEmpty()) {
            firstRecentImageSources(ctx, messageRecordManager).forEach(candidate -> addImageCandidate(result, candidate));
        }
        return new ArrayList<>(result.values());
    }

    @Nonnull
    private static List<StickerImageCandidate> firstQuotedImageSources(@Nonnull AgentContext ctx,
                                                                       @Nullable IMessageRecordManager messageRecordManager) {
        if (messageRecordManager == null
                || ctx.messageContext() == null
                || CollectionUtils.isNullOrEmpty(ctx.messageContext().originalMsg())) {
            return List.of();
        }
        Long replyId = BaniraUtils.getReplyId(ctx.messageContext().originalMsg());
        if (replyId == null || replyId <= 0 || replyId > Integer.MAX_VALUE) {
            return List.of();
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
        return imageCandidates(record, ctx.bot(), ctx.scopeGroupId());
    }

    @Nonnull
    private static List<StickerImageCandidate> firstRecentImageSources(@Nonnull AgentContext ctx,
                                                                       @Nullable IMessageRecordManager messageRecordManager) {
        if (messageRecordManager == null || ctx.msgType() != EnumMessageType.GROUP || ctx.scopeGroupId() <= 0) {
            return List.of();
        }
        LinkedHashMap<String, StickerImageCandidate> result = new LinkedHashMap<>();
        try {
            MessageRecordQueryParam param = new MessageRecordQueryParam(true, 1, 16)
                    .setBotId(ctx.botId())
                    .setGroupId(ctx.scopeGroupId())
                    .setMsgType(EnumMessageType.GROUP.name())
                    .setRecalled(false);
            param.addOrderBy(MessageRecordQueryParam.ORDER_ID, false);
            for (MessageRecord record : messageRecordManager.getMessageRecordList(param)) {
                if (record == null || Objects.equals(record.getMsgId(), ctx.msgId())) {
                    continue;
                }
                for (StickerImageCandidate candidate : imageCandidates(record, ctx.bot(), ctx.scopeGroupId())) {
                    addImageCandidate(result, candidate);
                    if (result.size() >= MAX_COLLECT_IMAGES_PER_REQUEST) {
                        return new ArrayList<>(result.values());
                    }
                }
            }
        } catch (Exception ignored) {
        }
        return new ArrayList<>(result.values());
    }

    @Nonnull
    private static String firstQuotedArchiveSource(@Nonnull AgentContext ctx,
                                                   @Nullable IMessageRecordManager messageRecordManager) {
        ArchiveSource source = firstQuotedArchiveCandidate(ctx, messageRecordManager);
        return source != null ? source.downloadSource() : "";
    }

    @Nonnull
    private static String firstRecentArchiveSource(@Nonnull AgentContext ctx,
                                                   @Nullable IMessageRecordManager messageRecordManager) {
        ArchiveSource source = firstRecentArchiveCandidate(ctx, messageRecordManager);
        return source != null ? source.downloadSource() : "";
    }

    @Nullable
    private static ArchiveSource resolveArchiveCandidate(@Nonnull AgentContext ctx,
                                                         @Nullable String source,
                                                         @Nullable IMessageRecordManager messageRecordManager) {
        ArchiveSource explicit = firstArchiveCandidate(StringUtils.nullToEmpty(source), 0L);
        if (explicit != null) {
            return explicit;
        }
        ArchiveSource current = firstArchiveCandidate(ctx.userMessage(), StringUtils.toLong(ctx.msgId(), 0L));
        if (current != null) {
            return current;
        }
        if (ctx.messageContext() != null && CollectionUtils.isNotNullOrEmpty(ctx.messageContext().originalMsg())) {
            ArchiveSource fromCurrent = firstArchiveCandidate(ctx.messageContext().originalMsg(), StringUtils.toLong(ctx.msgId(), 0L));
            if (fromCurrent != null) {
                return fromCurrent;
            }
            ArchiveSource fromQuotedRecord = firstQuotedArchiveCandidate(ctx, messageRecordManager);
            if (fromQuotedRecord != null) {
                return fromQuotedRecord;
            }
            try {
                List<ArrayMsg> replyContent = ctx.bot().getReplyContent(ctx.messageContext().originalMsg());
                ArchiveSource fromReply = firstArchiveCandidate(replyContent, StringUtils.toLong(ctx.msgId(), 0L));
                if (fromReply != null) {
                    return fromReply;
                }
            } catch (Exception ignored) {
            }
        }
        return firstRecentArchiveCandidate(ctx, messageRecordManager);
    }

    @Nullable
    private static ArchiveSource firstQuotedArchiveCandidate(@Nonnull AgentContext ctx,
                                                             @Nullable IMessageRecordManager messageRecordManager) {
        if (messageRecordManager == null
                || ctx.messageContext() == null
                || CollectionUtils.isNullOrEmpty(ctx.messageContext().originalMsg())) {
            return null;
        }
        Long replyId = BaniraUtils.getReplyId(ctx.messageContext().originalMsg());
        if (replyId == null || replyId <= 0 || replyId > Integer.MAX_VALUE) {
            return null;
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
        return firstArchiveCandidate(record);
    }

    @Nullable
    private static ArchiveSource firstRecentArchiveCandidate(@Nonnull AgentContext ctx,
                                                             @Nullable IMessageRecordManager messageRecordManager) {
        if (messageRecordManager == null || ctx.msgType() != EnumMessageType.GROUP || ctx.scopeGroupId() <= 0) {
            return null;
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
                ArchiveSource resolved = firstArchiveCandidate(record);
                if (resolved != null) {
                    return resolved;
                }
            }
        } catch (Exception ignored) {
        }
        return null;
    }

    @Nullable
    private static ArchiveSource firstArchiveCandidate(@Nullable MessageRecord record) {
        if (record == null || record.recalled()) {
            return null;
        }
        long sourceMsgId = StringUtils.toLong(record.getMsgId(), 0L);
        ArchiveSource fromRecode = firstArchiveCandidate(record.getMsgRecode(), sourceMsgId);
        if (fromRecode != null) {
            return fromRecode;
        }
        return firstArchiveCandidate(record.getMsgRaw(), sourceMsgId);
    }

    @Nullable
    private static ArchiveSource firstArchiveCandidate(@Nullable String text, long sourceMsgId) {
        String value = StringUtils.nullToEmpty(text);
        ArchiveSource fromArray = firstArchiveCandidate(safeStringToArray(value), sourceMsgId);
        if (fromArray != null) {
            return fromArray;
        }
        String source = firstDownloadableSource(value);
        return StringUtils.isNotNullOrEmpty(source) && isArchiveSource(source)
                ? ArchiveSource.downloadable(source, safeArchiveEntryName(source), sourceMsgId)
                : null;
    }

    @Nullable
    private static ArchiveSource firstArchiveCandidate(@Nullable List<ArrayMsg> msgs, long sourceMsgId) {
        if (CollectionUtils.isNullOrEmpty(msgs)) {
            return null;
        }
        for (ArrayMsg msg : msgs) {
            if (msg == null) {
                continue;
            }
            String file = msg.getStringData("file");
            String name = msg.getStringData("name");
            String fileId = msg.getStringData("file_id");
            int busId = (int) msg.getLongData("busid");
            if (StringUtils.isNullOrEmptyEx(name)) {
                name = file;
            }
            if (StringUtils.isNotNullOrEmpty(name) && isArchiveSource(name) && StringUtils.isNotNullOrEmpty(fileId)) {
                return ArchiveSource.groupFile(fileId, busId, name, sourceMsgId);
            }
            String url = msg.getStringData("url");
            if (StringUtils.isNotNullOrEmpty(url) && isArchiveSource(url)) {
                return ArchiveSource.downloadable(trimSource(url), safeArchiveEntryName(url), sourceMsgId);
            }
            if (StringUtils.isNotNullOrEmpty(file) && isArchiveSource(file)) {
                if (StringUtils.isNotNullOrEmpty(fileId)) {
                    return ArchiveSource.groupFile(fileId, busId, file, sourceMsgId);
                }
                return ArchiveSource.downloadable(trimSource(file), safeArchiveEntryName(file), sourceMsgId);
            }
        }
        return null;
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

    @Nullable
    private static StickerImageCandidate firstImageCandidate(@Nullable MessageRecord record) {
        List<StickerImageCandidate> candidates = imageCandidates(record, null, 0L);
        return candidates.isEmpty() ? null : candidates.getFirst();
    }

    @Nullable
    private static StickerImageCandidate firstImageCandidate(@Nullable String text, long sourceMsgId) {
        List<StickerImageCandidate> candidates = collectImageCandidates(null, 0L, text, sourceMsgId, false, MAX_COLLECT_IMAGES_PER_REQUEST);
        return candidates.isEmpty() ? null : candidates.getFirst();
    }

    @Nullable
    private static StickerImageCandidate firstImageCandidate(@Nullable List<ArrayMsg> msgs, long sourceMsgId) {
        List<StickerImageCandidate> candidates = collectImageCandidates(null, 0L, msgs, sourceMsgId, 0, false, MAX_COLLECT_IMAGES_PER_REQUEST);
        return candidates.isEmpty() ? null : candidates.getFirst();
    }

    @Nonnull
    private static List<StickerImageCandidate> imageCandidates(@Nullable MessageRecord record,
                                                               @Nullable BaniraBot bot,
                                                               long groupId) {
        if (record == null || record.recalled()) {
            return List.of();
        }
        long sourceMsgId = StringUtils.toLong(record.getMsgId(), 0L);
        LinkedHashMap<String, StickerImageCandidate> result = new LinkedHashMap<>();
        collectImageCandidates(bot, groupId, record.getMsgRecode(), sourceMsgId, true, MAX_COLLECT_IMAGES_PER_REQUEST)
                .forEach(candidate -> addImageCandidate(result, candidate));
        collectImageCandidates(bot, groupId, record.getMsgRaw(), sourceMsgId, true, MAX_COLLECT_IMAGES_PER_REQUEST - result.size())
                .forEach(candidate -> addImageCandidate(result, candidate));
        return new ArrayList<>(result.values());
    }

    @Nonnull
    private static List<StickerImageCandidate> collectImageCandidates(@Nullable BaniraBot bot,
                                                                      long groupId,
                                                                      @Nullable String text,
                                                                      long sourceMsgId,
                                                                      boolean includeForward,
                                                                      int limit) {
        String value = StringUtils.nullToEmpty(text);
        LinkedHashMap<String, StickerImageCandidate> result = new LinkedHashMap<>();
        collectImageCandidates(bot, groupId, safeStringToArray(value), sourceMsgId, 0, includeForward, limit)
                .forEach(candidate -> addImageCandidate(result, candidate));
        String source = firstDownloadableSource(value);
        if (result.isEmpty() && StringUtils.isNotNullOrEmpty(source) && isImageSource(source)) {
            addImageCandidate(result, new StickerImageCandidate(source, safeArchiveEntryName(source), value, sourceMsgId, MsgTypeEnum.image));
        }
        return new ArrayList<>(result.values());
    }

    @Nonnull
    private static List<StickerImageCandidate> collectImageCandidates(@Nullable BaniraBot bot,
                                                                      long groupId,
                                                                      @Nullable List<ArrayMsg> msgs,
                                                                      long sourceMsgId,
                                                                      int depth,
                                                                      boolean includeForward,
                                                                      int limit) {
        if (limit <= 0 || CollectionUtils.isNullOrEmpty(msgs) || depth > MAX_FORWARD_DEPTH) {
            return List.of();
        }
        LinkedHashMap<String, StickerImageCandidate> result = new LinkedHashMap<>();
        for (ArrayMsg msg : msgs) {
            if (msg == null || msg.getType() == null || result.size() >= limit) {
                continue;
            }
            MsgTypeEnum type = msg.getType();
            if (type == MsgTypeEnum.image || type == MsgTypeEnum.mface || type == MsgTypeEnum.marketface) {
                StickerImageCandidate candidate = imageCandidateFromMsg(msg, sourceMsgId);
                if (candidate != null) {
                    addImageCandidate(result, candidate);
                }
                continue;
            }
            if (includeForward && type == MsgTypeEnum.forward && bot != null) {
                for (MsgResp node : resolveForwardNodes(bot, msg)) {
                    if (node == null || result.size() >= limit) {
                        continue;
                    }
                    List<ArrayMsg> nodeMsgs = nodeArrayMsg(node);
                    collectImageCandidates(bot, groupId, nodeMsgs, sourceMsgId, depth + 1, true, limit - result.size())
                            .forEach(candidate -> addImageCandidate(result, candidate));
                }
            }
        }
        return new ArrayList<>(result.values());
    }

    @Nullable
    private static StickerImageCandidate imageCandidateFromMsg(@Nonnull ArrayMsg msg, long sourceMsgId) {
        String url = msg.getStringData("url");
        if (StringUtils.isNullOrEmptyEx(url)) {
            url = msg.getStringData("file");
        }
        if (StringUtils.isNullOrEmptyEx(url) || !isImageSource(url)) {
            return null;
        }
        String name = msg.getStringData("summary");
        if (StringUtils.isNullOrEmptyEx(name)) {
            name = msg.getStringData("file");
        }
        if (StringUtils.isNullOrEmptyEx(name)) {
            name = safeArchiveEntryName(url);
        }
        return new StickerImageCandidate(trimSource(url), name, msg.toCQCode(), sourceMsgId, msg.getType());
    }

    private static void addImageCandidate(@Nonnull LinkedHashMap<String, StickerImageCandidate> candidates,
                                          @Nullable StickerImageCandidate candidate) {
        if (candidate == null || StringUtils.isNullOrEmptyEx(candidate.sourceUrl())) {
            return;
        }
        candidates.putIfAbsent(candidate.sourceUrl() + "#" + candidate.sourceMsgId(), candidate);
    }

    @Nonnull
    private static List<MsgResp> resolveForwardNodes(@Nonnull BaniraBot bot, @Nonnull ArrayMsg arrayMsg) {
        List<MsgResp> nodes = BaniraUtils.getForwardContentFirst(arrayMsg);
        if (!nodes.isEmpty()) {
            return nodes.size() > MAX_FORWARD_NODES ? nodes.subList(0, MAX_FORWARD_NODES) : nodes;
        }
        long forwardId = arrayMsg.getLongData("id");
        if (forwardId <= 0 || forwardId > Integer.MAX_VALUE) {
            return List.of();
        }
        try {
            ActionData<GetForwardMsgResp> resp = bot.getForwardMsg((int) forwardId);
            if (bot.isActionDataNotEmpty(resp) && resp.getData() != null && resp.getData().getMessages() != null) {
                List<MsgResp> messages = resp.getData().getMessages();
                return messages.size() > MAX_FORWARD_NODES ? messages.subList(0, MAX_FORWARD_NODES) : messages;
            }
        } catch (Exception ignored) {
        }
        return List.of();
    }

    @Nonnull
    private static List<ArrayMsg> nodeArrayMsg(@Nonnull MsgResp node) {
        if (node.getArrayMsg() != null && !node.getArrayMsg().isEmpty()) {
            return node.getArrayMsg();
        }
        if (StringUtils.isNotNullOrEmpty(node.getMessage())) {
            return MessageConverser.stringToArray(node.getMessage());
        }
        return List.of();
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

    private static boolean isImageSource(@Nonnull String source) {
        String lower = source.toLowerCase(java.util.Locale.ROOT).split("[?#]", 2)[0];
        return lower.startsWith("data:image/")
                || lower.endsWith(".png")
                || lower.endsWith(".jpg")
                || lower.endsWith(".jpeg")
                || lower.endsWith(".gif")
                || lower.endsWith(".webp");
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

    record StickerImageCandidate(String sourceUrl,
                                 String name,
                                 String originalCq,
                                 long sourceMsgId,
                                 MsgTypeEnum sourceType) {
    }

    private record ArchiveSource(String downloadSource,
                                 String fileId,
                                 int busId,
                                 String name,
                                 long sourceMsgId) {
        static ArchiveSource downloadable(@Nonnull String source, @Nonnull String name, long sourceMsgId) {
            return new ArchiveSource(source, "", 0, name, sourceMsgId);
        }

        static ArchiveSource groupFile(@Nonnull String fileId, int busId, @Nonnull String name, long sourceMsgId) {
            return new ArchiveSource(name, fileId, busId, name, sourceMsgId);
        }
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
