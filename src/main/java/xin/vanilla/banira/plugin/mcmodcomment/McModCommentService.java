package xin.vanilla.banira.plugin.mcmodcomment;

import cn.hutool.core.io.FileUtil;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import xin.vanilla.banira.config.entity.GroupConfig;
import xin.vanilla.banira.config.entity.basic.OtherConfig;
import xin.vanilla.banira.config.entity.extended.McModCommentConfig;
import xin.vanilla.banira.config.entity.extended.ModWatchInfo;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.util.JsonUtils;
import xin.vanilla.banira.util.McModUtils;
import xin.vanilla.banira.util.StringUtils;
import xin.vanilla.banira.util.mcmod.EnumCommentType;
import xin.vanilla.banira.util.mcmod.McModCommentResult;
import xin.vanilla.banira.util.mcmod.McModCommentRow;
import xin.vanilla.banira.util.mcmod.McModSearchResult;

import java.io.File;
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

    private static final String CACHE_DIR = "data/mcmodcomment";

    /**
     * 获取mod的评论列表
     *
     * @param modId mod编号
     * @return 评论列表，如果失败返回null
     */
    public List<McModCommentRow> fetchComments(String modId) {
        try {
            McModCommentResult comments = McModUtils.getComments(EnumCommentType.MOD, null, modId, 1, false);
            if (comments != null) return comments.getRow();
        } catch (Exception e) {
            LOGGER.error("Error fetching comments for mod {}", modId, e);
        }
        return null;
    }

    /**
     * 检查是否有新评论
     *
     * @param modId    mod编号
     * @param comments 当前评论列表
     * @return 新评论结果，包含新评论列表和是否首次加载标志
     */
    public NewCommentsResult getNewComments(String modId, List<McModCommentRow> comments) {
        Set<String> cachedIds = commentCache.computeIfAbsent(modId, k -> new HashSet<>());
        List<McModCommentRow> newComments = new ArrayList<>();

        // 检查是否是首次加载（缓存文件不存在且未初始化）
        boolean cacheFileExists = getCacheFile(modId).exists();
        boolean isInitialized = cacheInitialized.getOrDefault(modId, false);
        // 如果是首次检测且缓存文件不存在，不应该发送通知
        boolean isFirstLoad = !cacheFileExists && !isInitialized;

        for (McModCommentRow comment : comments) {
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
    public record NewCommentsResult(List<McModCommentRow> newComments, boolean isFirstLoad) {
    }

    /**
     * 初始化缓存（首次加载时缓存所有现有评论）
     *
     * @param modId    mod编号
     * @param comments 评论列表
     */
    public void initCache(String modId, List<McModCommentRow> comments) {
        Set<String> cachedIds = commentCache.computeIfAbsent(modId, k -> new HashSet<>());
        for (McModCommentRow comment : comments) {
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
    public String formatCommentMessage(String modId, McModCommentRow comment) {
        McModSearchResult modName = McModUtils.getModName(modId);
        String result = "【MC百科新评论提醒】\n" +
                "模组链接: " + McModUtils.getModUrl(modId) + "\n";
        if (modName != null) {
            result += "模组名称: " + modName.toFormatString() + "\n";
        }
        result += "模组编号: " + modId + "\n" +
                "评论编号: " + comment.getId() + "\n" +
                "楼层: " + comment.getFloor() + "\n" +
                "用户: " + comment.getUser().getName() + " (" + comment.getUser().getId() + ")\n" +
                "时间: " + comment.getTime() + "\n" +
                "内容: " + comment.getContent();
        return result;
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

            McModCommentConfig config = entry.getValue().mcModCommentConfig();
            if (config == null || config.modWatchMap() == null) {
                continue;
            }

            // 遍历该群的所有监控配置
            for (Map.Entry<String, List<ModWatchInfo>> modEntry : config.modWatchMap().entrySet()) {
                String modId = modEntry.getKey();
                List<ModWatchInfo> watchInfos = modEntry.getValue();

                // 只添加属于当前群的监控信息
                for (ModWatchInfo watchInfo : watchInfos) {
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
    public void sendCommentToGroup(BaniraBot bot, Long groupId, String modId, McModCommentRow comment) {
        try {
            String message = formatCommentMessage(modId, comment);
            bot.sendGroupMsg(groupId, message, false);
        } catch (Exception e) {
            LOGGER.error("Error sending comment to group {} for mod {}", groupId, modId, e);
        }
    }

}

