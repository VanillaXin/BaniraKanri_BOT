package xin.vanilla.banira.plugin.mcmod;

import cn.hutool.core.io.FileUtil;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.mikuac.shiro.common.utils.MsgUtils;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import tools.jackson.core.type.TypeReference;
import xin.vanilla.banira.config.entity.extended.McModCommentConfig;
import xin.vanilla.banira.config.entity.extended.ModWatchInfo;
import xin.vanilla.banira.config.entity.group.McModGroupConfig;
import xin.vanilla.banira.config.other.OtherConfigRegistry;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.util.*;
import xin.vanilla.banira.util.mcmod.*;

import java.io.File;
import java.time.Duration;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * MCMod评论监控服务
 */
@Slf4j
@Service
public class McModCommentService {

    @Resource
    private OtherConfigRegistry otherConfigRegistry;

    /**
     * 缓存历史评论ID, cacheKey (type:containerId) -> Set<McModCommentRow>
     */
    public static final Map<String, Set<McModCommentRow>> COMMENT_CACHE = new ConcurrentHashMap<>();

    private static final String CACHE_DIR = "data/mcmod/comment";

    /**
     * 评论元数据标记，用于回复/删除指令解析
     */
    public static final Pattern COMMENT_METADATA_PATTERN = Pattern.compile("#MCMOD:([^:\\s]+):([^:\\s]+):(\\d+)");

    /**
     * 生成缓存key
     *
     * @param commentType 评论类型
     * @param containerId 容器ID
     * @return 缓存key
     */
    public static String getCacheKey(EnumContentType commentType, String containerId) {
        return commentType.value() + ":" + containerId;
    }

    /**
     * 拉取评论列表
     *
     * @param commentType 评论类型
     * @param containerId 容器ID
     */
    public static Set<McModCommentRow> fetchComments(EnumContentType commentType, String containerId) {
        String cacheKey = getCacheKey(commentType, containerId);
        Set<McModCommentRow> result = new HashSet<>();
        try {
            McModCommentResult comments = McModUtils.getComments(commentType, null, containerId, 1, false);
            if (comments != null) {
                result.addAll(comments.getRow());
                // 获取所有评论的回复
                for (McModCommentRow comment : comments.getRow()) {
                    if (comment.isReply()) continue;
                    McModCommentResult replies = McModUtils.getCommentReplies(comment.getId(), 1);
                    if (replies != null) {
                        result.addAll(replies.getRow());
                    }
                }
            }
            // 获取缓存中的最近俩天的评论的回复
            Instant minus = Instant.now().minus(Duration.ofDays(2));
            for (McModCommentRow comment : COMMENT_CACHE.getOrDefault(cacheKey, Set.of())) {
                if (comment.isReply()) continue;
                Instant replyTime = parseReplyTime(comment.getTime());
                if (replyTime == null || replyTime.isBefore(minus)) continue;
                McModCommentResult replies = McModUtils.getCommentReplies(comment.getId(), 1);
                if (replies != null) {
                    result.addAll(replies.getRow());
                    McModCommentResult curent = replies;
                    while (curent != null && curent.getPage() != null && curent.getPage().getNext() != null) {
                        curent = McModUtils.getComments(commentType, containerId, curent.getPage().getNext());
                        if (curent != null) {
                            result.addAll(curent.getRow());
                        }
                    }
                }
            }
        } catch (Exception e) {
            LOGGER.error("Error fetching comments for type {} container {}", commentType, containerId, e);
        }
        return result;
    }

    /**
     * 刷新缓存并返回新评论列表
     *
     * @param commentType 评论类型
     * @param containerId 容器ID
     * @param comments    最新拉取的评论列表
     */
    public static @Nonnull Set<McModCommentRow> getNewComments(EnumContentType commentType, String containerId, Set<McModCommentRow> comments) {
        String cacheKey = getCacheKey(commentType, containerId);
        Set<McModCommentRow> commentCache = COMMENT_CACHE.computeIfAbsent(cacheKey, k -> new HashSet<>());

        // 组装评论对象
        Set<McModCommentRow> latestComments = new HashSet<>();
        for (McModCommentRow comment : comments) {
            comment.setCommentType(commentType);
            comment.setContainerId(containerId);
            if (comment.isReply()) {
                McModCommentRow parent = commentCache.stream().filter(c -> c.getId().equals(comment.getParentId())).findFirst()
                        .orElseGet(() -> comments.stream().filter(c -> c.getId().equals(comment.getParentId())).findFirst().orElse(null));
                comment.setParentComment(parent);
            } else {
                List<McModCommentRow> children;
                children = commentCache.stream().filter(c -> c.getParentId() != null && c.getParentId().equals(comment.getId())).toList();
                if (CollectionUtils.isNullOrEmpty(children)) {
                    children = comments.stream().filter(c -> c.getParentId() != null && c.getParentId().equals(comment.getId())).toList();
                }
                comment.setReplies(children);
            }
            latestComments.add(comment);
        }

        Set<McModCommentRow> newComments = new HashSet<>();
        // 检查新评论
        for (McModCommentRow comment : latestComments) {
            if (commentCache.stream().noneMatch(cache -> cache.getId().equals(comment.getId()))) {
                newComments.add(comment);
            }
        }

        // 过滤掉1小时前的回复
        Instant oneHourAgo = Instant.now().minus(Duration.ofHours(1));
        newComments = newComments.stream().filter(comment -> {
            Instant replyTime = parseReplyTime(comment.getTime());
            return replyTime != null && replyTime.isAfter(oneHourAgo);
        }).collect(Collectors.toSet());

        // 使用 Map 确保新内容覆盖旧内容
        Map<String, McModCommentRow> cacheMap = new HashMap<>();
        // 先将现有缓存放入 Map
        for (McModCommentRow cached : commentCache) {
            String key = getCommentKey(cached.getCommentType(), cached.getContainerId(), cached.getId());
            cacheMap.put(key, cached);
        }
        // 用新内容覆盖旧内容
        for (McModCommentRow comment : comments) {
            String key = getCommentKey(comment.getCommentType(), comment.getContainerId(), comment.getId());
            cacheMap.put(key, comment);
        }
        // 更新缓存 Set
        commentCache.clear();
        commentCache.addAll(cacheMap.values());
        saveCacheToFile(cacheKey, commentCache);

        return newComments;
    }

    /**
     * 解析回复时间
     *
     * @param time 时间对象
     */
    private static Instant parseReplyTime(McModCommentTime time) {
        if (time == null) {
            return null;
        }

        String source = time.getSource();
        if (source != null && !source.isEmpty()) {
            java.util.Date date = DateUtils.format(source);
            if (date != null) {
                return date.toInstant();
            }
        }

        String range = time.getRange();
        if (range != null && !range.isEmpty()) {
            return parseRelativeTime(range);
        }

        return null;
    }

    /**
     * 解析相对时间字符串
     *
     * @param range 相对时间字符串
     */
    private static Instant parseRelativeTime(String range) {
        if (range == null || range.isEmpty()) {
            return null;
        }
        Pattern pattern = Pattern.compile("(\\d+)([分时天月年])前");
        Matcher matcher = pattern.matcher(range);
        if (matcher.find()) {
            try {
                int value = Integer.parseInt(matcher.group(1));
                String unit = matcher.group(2);

                Instant now = Instant.now();
                return switch (unit) {
                    case "分" -> now.minus(Duration.ofMinutes(value));
                    case "时" -> now.minus(Duration.ofHours(value));
                    case "天" -> now.minus(Duration.ofDays(value));
                    case "月" -> now.minus(Duration.ofDays(value * 30L));
                    case "年" -> now.minus(Duration.ofDays(value * 365L));
                    default -> null;
                };
            } catch (NumberFormatException e) {
                LOGGER.warn("Failed to parse relative time: {}", range, e);
                return null;
            }
        }
        return null;
    }

    /**
     * 从文件加载缓存
     *
     * @param cacheKey 缓存key (type:containerId)
     */
    public static void loadCacheFromFile(String cacheKey) {
        File cacheFile = getCacheFile(cacheKey);
        if (!cacheFile.exists()) {
            LOGGER.debug("Cache file not found for key {}, will initialize on first check", cacheKey);
            return;
        }

        try {
            String content = FileUtil.readUtf8String(cacheFile);
            if (StringUtils.isNullOrEmptyEx(content)) {
                return;
            }

            JsonArray jsonArray = JsonUtils.parseJsonArray(content);
            if (jsonArray == null) {
                return;
            }

            Set<McModCommentRow> cachedComments = new HashSet<>();
            for (JsonElement element : jsonArray) {
                McModCommentRow comment = com.mikuac.shiro.common.utils.JsonUtils.readValue(element.toString(), new TypeReference<>() {
                });
                if (comment != null) {
                    cachedComments.add(comment);
                }
            }

            COMMENT_CACHE.put(cacheKey, cachedComments);
            LOGGER.info("Loaded {} cached comment IDs for key {} from file", cachedComments.size(), cacheKey);
        } catch (Exception e) {
            LOGGER.error("Error loading cache from file for key {}", cacheKey, e);
        }
    }

    /**
     * 保存缓存到文件
     * 按时间倒序排序后保存
     *
     * @param cacheKey       缓存key (type:containerId)
     * @param cachedComments 缓存的评论
     */
    public static void saveCacheToFile(String cacheKey, Set<McModCommentRow> cachedComments) {
        try {
            File cacheFile = getCacheFile(cacheKey);
            File cacheDir = cacheFile.getParentFile();
            if (cacheDir != null && !cacheDir.exists()) {
                cacheDir.mkdirs();
            }

            // 按时间倒序排序
            List<McModCommentRow> sortedComments = new ArrayList<>(cachedComments);
            sortedComments.sort((a, b) -> {
                Instant timeA = parseReplyTime(a.getTime());
                Instant timeB = parseReplyTime(b.getTime());
                if (timeA == null && timeB == null) return 0;
                if (timeA == null) return 1;
                if (timeB == null) return -1;
                return timeB.compareTo(timeA);
            });

            JsonArray jsonArray = new JsonArray();
            for (McModCommentRow comment : sortedComments) {
                jsonArray.add(JsonUtils.parseJson(comment));
            }

            String content = JsonUtils.PRETTY_GSON.toJson(jsonArray);
            FileUtil.writeUtf8String(content, cacheFile);
            LOGGER.debug("Saved {} cached comment IDs for mod {} to file", cachedComments.size(), cacheKey);
        } catch (Exception e) {
            LOGGER.error("Error saving cache to file for mod {}", cacheKey, e);
        }
    }

    /**
     * 生成评论的唯一键
     *
     * @param commentType 评论类型
     * @param containerId 容器ID
     * @param id          评论ID
     */
    public static String getCommentKey(EnumContentType commentType, String containerId, String id) {
        return (commentType != null ? commentType.value() : "") + ":" +
                (containerId != null ? containerId : "") + ":" +
                (id != null ? id : "");
    }

    /**
     * 获取缓存文件路径
     *
     * @param cacheKey 缓存key (type:containerId)
     */
    private static File getCacheFile(String cacheKey) {
        // 将冒号替换为下划线，避免文件名问题
        String safeFileName = cacheKey.replace(":", "_");
        return new File(CACHE_DIR, safeFileName + ".json");
    }

    /**
     * 格式化评论元数据（供回复/删除指令解析）
     */
    @Nonnull
    public static String formatCommentMetadata(@Nonnull EnumContentType commentType, @Nonnull String containerId,
                                               @Nonnull McModCommentRow comment) {
        return String.format("#MCMOD:%s:%s:%s", commentType.name(), containerId, comment.getId());
    }

    /**
     * 格式化评论消息（文本兜底）
     */
    public static String formatCommentMessage(EnumContentType commentType, String containerId, McModCommentRow comment) {
        String containerLabel = resolveContainerLabel(commentType, containerId);
        boolean isReply = comment.getParentComment() != null;
        String header = isReply ? "新回复" : "新评论";
        String user = comment.getUser() != null ? comment.getUser().getName() : "未知用户";
        String content = getPlainCommentText(comment);

        StringBuilder sb = new StringBuilder();
        sb.append("【MC百科·").append(header).append("】")
                .append(getCommentTypeName(commentType)).append(" · ").append(containerLabel).append('\n')
                .append(user);
        if (!isReply && StringUtils.isNotNullOrEmpty(comment.getFloor())) {
            sb.append(" · #").append(comment.getFloor());
        }
        if (comment.getTime() != null && StringUtils.isNotNullOrEmpty(comment.getTime().getSource())) {
            sb.append(" · ").append(comment.getTime().getSource());
        }
        sb.append('\n').append(content);

        if (isReply && comment.getParentComment() != null && comment.getParentComment().getUser() != null) {
            sb.append("\n↩ @").append(comment.getParentComment().getUser().getName());
        }
        sb.append('\n').append(formatCommentMetadata(commentType, containerId, comment));
        return sb.toString();
    }

    /**
     * 获取评论类型名称
     */
    @Nonnull
    public static String getCommentTypeName(@Nonnull EnumContentType commentType) {
        return switch (commentType) {
            case MOD -> "模组";
            case MODPACK -> "整合包";
            case AUTHOR -> "作者";
            case USER_CENTER -> "用户中心";
        };
    }

    /**
     * 获取纯文本评论内容
     */
    @Nonnull
    public static String getPlainCommentText(@Nullable McModCommentRow comment) {
        if (comment == null) {
            return "";
        }
        if (StringUtils.isNotNullOrEmpty(comment.getContent())) {
            return getCommentSummary(org.jsoup.Jsoup.parse(comment.getContent()).text());
        }
        return getCommentSummary(comment.getFormattedContent());
    }

    /**
     * 获取评论内容摘要
     */
    @Nonnull
    public static String getCommentSummary(@Nullable String content) {
        if (content == null) {
            return "";
        }
        String text = content.replaceAll("<[^>]+>", "").trim();
        if (text.length() > 120) {
            return text.substring(0, 120) + "...";
        }
        return text;
    }

    @Nullable
    private static String resolveContainerLabel(@Nonnull EnumContentType commentType, @Nonnull String containerId) {
        McModContent content = switch (commentType) {
            case MOD -> McModUtils.getModName(containerId);
            case MODPACK -> McModUtils.getModpackName(containerId);
            case AUTHOR -> McModUtils.getAuthorName(containerId);
            case USER_CENTER -> null;
        };
        if (content != null) {
            return content.getFormattedName();
        }
        return containerId;
    }

    /**
     * 发送新评论到指定群
     *
     * @param bot         机器人实例
     * @param groupId     群ID
     * @param commentType 评论类型
     * @param containerId 容器ID
     * @param comment     评论信息
     */
    public static void sendCommentToGroup(BaniraBot bot, Long groupId, EnumContentType commentType, String containerId, McModCommentRow comment) {
        try {
            McModGroupConfig othersConfig = BaniraUtils.getGroupConfigOrGlobal(McModGroupConfig.class, groupId);
            if (othersConfig == null || othersConfig.mcModCommentConfig() == null || !othersConfig.mcModCommentConfig().enable()) {
                return;
            }
            // 若为当前登录账号的评论则不发送通知
            String currentUserId = McModUtils.getLoginUserId(groupId);
            if (currentUserId != null) {
                String commentUserId = comment.getUser().getId();
                if (currentUserId.equals(commentUserId)) {
                    LOGGER.debug("Skipping notification for own comment/reply, type: {}, containerId: {}, commentId: {}", commentType, containerId, comment.getId());
                    return;
                }
            }

            String containerLabel = resolveContainerLabel(commentType, containerId);
            String metadata = formatCommentMetadata(commentType, containerId, comment);
            String imageMsg = McModRenderHelper.renderComment(commentType, containerId, comment, containerLabel);
            String message = StringUtils.isNotNullOrEmpty(imageMsg)
                    ? MsgUtils.builder().text(imageMsg).text("\n").text(metadata).build()
                    : formatCommentMessage(commentType, containerId, comment);
            bot.sendGroupMsg(groupId, message, false);
        } catch (Exception e) {
            LOGGER.error("Error sending comment to group {} for type {} container {}", groupId, commentType, containerId, e);
        }
    }

    public void initializeCacheIfAbsent(EnumContentType commentType, String containerId) {
        String cacheKey = getCacheKey(commentType, containerId);
        if (COMMENT_CACHE.containsKey(cacheKey)) {
            return;
        }
        Map<String, McModCommentRow> commentMap = new HashMap<>();
        McModCommentResult comments = McModUtils.getComments(commentType, containerId, 1);
        if (comments != null) {
            for (McModCommentRow comment : comments.getRow()) {
                comment.setCommentType(commentType);
                comment.setContainerId(containerId);
                String key = getCommentKey(commentType, containerId, comment.getId());
                commentMap.put(key, comment);
            }
            McModCommentResult current = comments;
            while (current != null && current.getPage() != null && current.getPage().getNext() != null) {
                current = McModUtils.getComments(commentType, containerId, current.getPage().getNext());
                if (current != null) {
                    for (McModCommentRow comment : current.getRow()) {
                        comment.setCommentType(commentType);
                        comment.setContainerId(containerId);
                        String key = getCommentKey(commentType, containerId, comment.getId());
                        commentMap.put(key, comment);
                    }
                }
            }
        }
        List<McModCommentRow> commentList = new ArrayList<>(commentMap.values());
        for (McModCommentRow commentRow : commentList) {
            McModCommentResult replies = McModUtils.getCommentReplies(commentRow.getId(), 1);
            if (replies == null) {
                continue;
            }
            for (McModCommentRow reply : replies.getRow()) {
                reply.setCommentType(commentType);
                reply.setContainerId(containerId);
                reply.setParentId(commentRow.getId());
                String key = getCommentKey(commentType, containerId, reply.getId());
                commentMap.put(key, reply);
            }
            McModCommentResult current = replies;
            while (current != null && current.getPage() != null && current.getPage().getNext() != null) {
                current = McModUtils.getCommentReplies(commentRow.getId(), current.getPage().getNext());
                if (current != null) {
                    for (McModCommentRow reply : current.getRow()) {
                        reply.setCommentType(commentType);
                        reply.setContainerId(containerId);
                        reply.setParentId(commentRow.getId());
                        String key = getCommentKey(commentType, containerId, reply.getId());
                        commentMap.put(key, reply);
                    }
                }
            }
        }
        Set<McModCommentRow> cachedComment = new HashSet<>(commentMap.values());
        COMMENT_CACHE.put(cacheKey, cachedComment);
        saveCacheToFile(cacheKey, cachedComment);
    }

    /**
     * 加载所有已配置容器的缓存
     */
    public void loadAllCaches() {
        Map<String, List<ModWatchInfo>> monitoredContainers = getAllMonitoredContainers();
        for (String cacheKey : monitoredContainers.keySet()) {
            loadCacheFromFile(cacheKey);
        }
        LOGGER.info("Loaded caches for {} containers", monitoredContainers.size());
    }

    /**
     * 获取所有需要检测的容器列表
     *
     * @return cacheKey -> watchInfos
     */
    public Map<String, List<ModWatchInfo>> getAllMonitoredContainers() {
        Map<String, List<ModWatchInfo>> cacheKeyToWatchInfos = new HashMap<>();
        Map<Long, McModGroupConfig> otherConfigMap = otherConfigRegistry.getGroupedSnapshot(McModGroupConfig.class);

        for (Map.Entry<Long, McModGroupConfig> entry : otherConfigMap.entrySet()) {
            Long groupId = entry.getKey();
            if (groupId == null || groupId == 0L) {
                continue; // 跳过全局配置
            }

            McModCommentConfig config = entry.getValue().mcModCommentConfig();
            if (config == null || config.modWatchMap() == null) {
                continue;
            }

            // 遍历该群的所有监控配置
            for (Map.Entry<String, List<ModWatchInfo>> containerEntry : config.modWatchMap().entrySet()) {
                String containerId = containerEntry.getKey();
                List<ModWatchInfo> watchInfos = containerEntry.getValue();

                // 只添加属于当前群的监控信息
                for (ModWatchInfo watchInfo : watchInfos) {
                    if (watchInfo.groupId().equals(groupId)) {
                        String cacheKey = getCacheKey(watchInfo.commentType(), containerId);
                        cacheKeyToWatchInfos.computeIfAbsent(cacheKey, k -> new ArrayList<>()).add(watchInfo);
                    }
                }
            }
        }
        return cacheKeyToWatchInfos;
    }

}
