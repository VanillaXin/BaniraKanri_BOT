package xin.vanilla.banira.plugin.mcmodcomment;

import cn.hutool.core.io.FileUtil;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import xin.vanilla.banira.config.entity.GroupConfig;
import xin.vanilla.banira.config.entity.basic.OtherConfig;
import xin.vanilla.banira.config.entity.extended.ModWatchInfo;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.util.JsonUtils;
import xin.vanilla.banira.util.StringUtils;

import java.io.File;
import java.net.URI;
import java.net.URLEncoder;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Supplier;

/**
 * MCMod评论监控服务
 */
@Slf4j
@Service
public class McModCommentService {

    @Resource
    private Supplier<GroupConfig> groupConfig;

    /**
     * 缓存历史评论ID，格式：modId -> Set<commentId>
     */
    private final Map<String, Set<String>> commentCache = new ConcurrentHashMap<>();

    /**
     * 记录每个mod是否已经初始化过缓存（从文件加载或首次检测）
     */
    private final Map<String, Boolean> cacheInitialized = new ConcurrentHashMap<>();

    private static final String API_URL = "https://www.mcmod.cn/frame/comment/CommentRow/";
    private static final String REFERER_PREFIX = "https://www.mcmod.cn/class/";
    private static final String REFERER_SUFFIX = ".html";
    private static final String CACHE_DIR = "data/mcmodcomment";

    private static final HttpClient httpClient = HttpClient.newBuilder()
            .followRedirects(HttpClient.Redirect.NORMAL)
            .build();

    /**
     * 获取mod的评论列表
     *
     * @param modId mod编号
     * @return 评论列表，如果失败返回null
     */
    public List<CommentInfo> fetchComments(String modId) {
        try {
            // 构建请求参数
            JsonObject data = new JsonObject();
            data.addProperty("type", "class");
            data.addProperty("channel", "1");
            data.addProperty("doid", modId);
            data.addProperty("page", 1);
            data.addProperty("selfonly", 0);

            String dataStr = JsonUtils.toJsonString(data);
            String encodedData = URLEncoder.encode(dataStr, StandardCharsets.UTF_8);

            // 构建请求体
            String body = "data=" + encodedData;

            // 构建请求
            HttpRequest request = HttpRequest.newBuilder()
                    .uri(URI.create(API_URL))
                    .header("Content-Type", "application/x-www-form-urlencoded")
                    .header("Referer", REFERER_PREFIX + modId + REFERER_SUFFIX)
                    .header("User-Agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36")
                    .POST(HttpRequest.BodyPublishers.ofString(body, StandardCharsets.UTF_8))
                    .build();

            // 发送POST请求
            HttpResponse<String> httpResponse = httpClient.send(request, HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8));
            String response = httpResponse.body();

            if (StringUtils.isNullOrEmptyEx(response)) {
                LOGGER.warn("Failed to fetch comments for mod {}: empty response", modId);
                return null;
            }

            // 解析响应
            JsonObject jsonResponse = JsonUtils.parseJsonObject(response);
            if (jsonResponse == null) {
                LOGGER.warn("Failed to parse response for mod {}: invalid JSON", modId);
                return null;
            }

            // 检查状态
            int state = JsonUtils.getInt(jsonResponse, "state", -1);
            if (state != 0) {
                LOGGER.warn("API returned error state {} for mod {}", state, modId);
                return null;
            }

            // 获取评论列表
            JsonArray rowArray = JsonUtils.getJsonArray(jsonResponse, "data.row", null);
            if (rowArray == null || rowArray.isEmpty()) {
                return new ArrayList<>();
            }

            List<CommentInfo> comments = new ArrayList<>();
            for (JsonElement element : rowArray) {
                if (element.isJsonObject()) {
                    CommentInfo comment = parseComment(element.getAsJsonObject());
                    if (comment != null) {
                        comments.add(comment);
                    }
                }
            }

            return comments;
        } catch (Exception e) {
            LOGGER.error("Error fetching comments for mod {}", modId, e);
            return null;
        }
    }

    /**
     * 解析评论信息
     */
    private CommentInfo parseComment(JsonObject commentObj) {
        try {
            CommentInfo comment = new CommentInfo();
            comment.setId(JsonUtils.getString(commentObj, "id", ""));
            comment.setFloor(JsonUtils.getString(commentObj, "floor", ""));
            comment.setContent(JsonUtils.getString(commentObj, "content", ""));

            // 解析用户信息
            JsonObject userObj = JsonUtils.getJsonObject(commentObj, "user", null);
            if (userObj != null) {
                comment.setUserId(JsonUtils.getString(userObj, "id", ""));
                comment.setUserName(JsonUtils.getString(userObj, "name", ""));
            }

            // 解析时间信息
            JsonObject timeObj = JsonUtils.getJsonObject(commentObj, "time", null);
            if (timeObj != null) {
                comment.setTime(JsonUtils.getString(timeObj, "source", ""));
            }

            return comment;
        } catch (Exception e) {
            LOGGER.error("Error parsing comment", e);
            return null;
        }
    }

    /**
     * 检查是否有新评论
     *
     * @param modId    mod编号
     * @param comments 当前评论列表
     * @return 新评论结果，包含新评论列表和是否首次加载标志
     */
    public NewCommentsResult getNewComments(String modId, List<CommentInfo> comments) {
        Set<String> cachedIds = commentCache.computeIfAbsent(modId, k -> new HashSet<>());
        List<CommentInfo> newComments = new ArrayList<>();

        // 检查是否是首次加载（缓存文件不存在且未初始化）
        boolean cacheFileExists = getCacheFile(modId).exists();
        boolean isInitialized = cacheInitialized.getOrDefault(modId, false);
        // 如果是首次检测且缓存文件不存在，不应该发送通知
        boolean isFirstLoad = !cacheFileExists && !isInitialized;

        for (CommentInfo comment : comments) {
            if (!cachedIds.contains(comment.getId())) {
                newComments.add(comment);
                cachedIds.add(comment.getId());
            }
        }

        // 如果有新评论或需要初始化，保存到文件
        if (!newComments.isEmpty() || !isInitialized) {
            saveCacheToFile(modId, cachedIds);
            cacheInitialized.put(modId, true);
        }

        return new NewCommentsResult(newComments, isFirstLoad);
    }

    /**
     * 新评论结果
     */
    public record NewCommentsResult(List<CommentInfo> newComments, boolean isFirstLoad) {

    }

    /**
     * 初始化缓存（首次加载时缓存所有现有评论）
     *
     * @param modId    mod编号
     * @param comments 评论列表
     */
    public void initCache(String modId, List<CommentInfo> comments) {
        Set<String> cachedIds = commentCache.computeIfAbsent(modId, k -> new HashSet<>());
        for (CommentInfo comment : comments) {
            cachedIds.add(comment.getId());
        }
        // 保存到文件
        saveCacheToFile(modId, cachedIds);
        cacheInitialized.put(modId, true);
    }

    /**
     * 从文件加载缓存
     *
     * @param modId mod编号
     */
    public void loadCacheFromFile(String modId) {
        File cacheFile = getCacheFile(modId);
        if (!cacheFile.exists()) {
            LOGGER.debug("Cache file not found for mod {}, will initialize on first check", modId);
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

            Set<String> cachedIds = new HashSet<>();
            for (JsonElement element : jsonArray) {
                if (element.isJsonPrimitive()) {
                    cachedIds.add(element.getAsString());
                }
            }

            commentCache.put(modId, cachedIds);
            cacheInitialized.put(modId, true);
            LOGGER.info("Loaded {} cached comment IDs for mod {} from file", cachedIds.size(), modId);
        } catch (Exception e) {
            LOGGER.error("Error loading cache from file for mod {}", modId, e);
        }
    }

    /**
     * 保存缓存到文件
     *
     * @param modId     mod编号
     * @param cachedIds 缓存的评论ID集合
     */
    private void saveCacheToFile(String modId, Set<String> cachedIds) {
        try {
            File cacheFile = getCacheFile(modId);
            File cacheDir = cacheFile.getParentFile();
            if (cacheDir != null && !cacheDir.exists()) {
                cacheDir.mkdirs();
            }

            JsonArray jsonArray = new JsonArray();
            for (String commentId : cachedIds) {
                jsonArray.add(commentId);
            }

            String content = JsonUtils.PRETTY_GSON.toJson(jsonArray);
            FileUtil.writeUtf8String(content, cacheFile);
            LOGGER.debug("Saved {} cached comment IDs for mod {} to file", cachedIds.size(), modId);
        } catch (Exception e) {
            LOGGER.error("Error saving cache to file for mod {}", modId, e);
        }
    }

    /**
     * 获取缓存文件路径
     *
     * @param modId mod编号
     * @return 缓存文件
     */
    private File getCacheFile(String modId) {
        return new File(CACHE_DIR, modId + ".json");
    }

    /**
     * 加载所有已配置mod的缓存
     */
    public void loadAllCaches() {
        Map<String, List<ModWatchInfo>> monitoredMods = getAllMonitoredMods();
        for (String modId : monitoredMods.keySet()) {
            loadCacheFromFile(modId);
        }
        LOGGER.info("Loaded caches for {} mods", monitoredMods.size());
    }

    /**
     * 检查指定mod的缓存是否已初始化
     *
     * @param modId mod编号
     * @return 是否已初始化
     */
    public boolean isCacheInitialized(String modId) {
        return cacheInitialized.getOrDefault(modId, false);
    }

    /**
     * 格式化评论消息
     *
     * @param modId   mod编号
     * @param comment 评论信息
     * @return 格式化后的消息
     */
    public String formatCommentMessage(String modId, CommentInfo comment) {
        return "【MC百科新评论提醒】\n" +
                "Mod编号: " + modId + "\n" +
                "链接: " + REFERER_PREFIX + modId + REFERER_SUFFIX + "\n" +
                "楼层: " + comment.getFloor() + "\n" +
                "用户: " + comment.getUserName() + " (" + comment.getUserId() + ")\n" +
                "时间: " + comment.getTime() + "\n" +
                "内容: " + comment.getContent();
    }

    /**
     * 获取所有需要检测的mod列表（从所有群的配置中收集）
     *
     * @return mod编号到监控信息列表的映射
     */
    public Map<String, List<ModWatchInfo>> getAllMonitoredMods() {
        Map<String, List<ModWatchInfo>> modToWatchInfos = new HashMap<>();
        Map<Long, OtherConfig> otherConfigMap = groupConfig.get().otherConfig();

        for (Map.Entry<Long, OtherConfig> entry : otherConfigMap.entrySet()) {
            Long groupId = entry.getKey();
            if (groupId == null || groupId == 0L) {
                continue; // 跳过全局配置
            }

            xin.vanilla.banira.config.entity.extended.McModCommentConfig config = entry.getValue().mcModCommentConfig();
            if (config == null || config.modWatchMap() == null) {
                continue;
            }

            // 遍历该群的所有监控配置
            for (Map.Entry<String, List<xin.vanilla.banira.config.entity.extended.ModWatchInfo>> modEntry : config.modWatchMap().entrySet()) {
                String modId = modEntry.getKey();
                List<xin.vanilla.banira.config.entity.extended.ModWatchInfo> watchInfos = modEntry.getValue();
                
                // 只添加属于当前群的监控信息
                for (xin.vanilla.banira.config.entity.extended.ModWatchInfo watchInfo : watchInfos) {
                    if (watchInfo.groupId().equals(groupId)) {
                        modToWatchInfos.computeIfAbsent(modId, k -> new ArrayList<>()).add(watchInfo);
                    }
                }
            }
        }

        return modToWatchInfos;
    }

    /**
     * 发送新评论到指定群
     *
     * @param bot     机器人实例
     * @param groupId 群ID
     * @param modId   mod编号
     * @param comment 评论信息
     */
    public void sendCommentToGroup(BaniraBot bot, Long groupId, String modId, CommentInfo comment) {
        try {
            String message = formatCommentMessage(modId, comment);
            bot.sendGroupMsg(groupId, message, false);
        } catch (Exception e) {
            LOGGER.error("Error sending comment to group {} for mod {}", groupId, modId, e);
        }
    }

}

