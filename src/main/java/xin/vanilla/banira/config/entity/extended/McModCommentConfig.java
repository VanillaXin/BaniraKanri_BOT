package xin.vanilla.banira.config.entity.extended;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import xin.vanilla.banira.util.mcmod.EnumCommentType;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * MCMod评论监控配置
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class McModCommentConfig {

    /**
     * 检测目标列表（容器ID -> 监控信息列表）
     * 每个容器可以对应多个群，每个群有自己的botId和评论类型
     */
    private Map<String, List<ModWatchInfo>> modWatchMap;

    /**
     * 是否启用
     */
    private boolean enable;

    {
        this.modWatchMap = new LinkedHashMap<>();
        this.enable = true;
    }

    /**
     * 添加监控目标
     *
     * @param commentType 评论类型
     * @param containerId 容器ID
     * @param groupId     群号
     * @param botId       Bot ID
     */
    public void addModWatch(EnumCommentType commentType, String containerId, Long groupId, Long botId) {
        modWatchMap.computeIfAbsent(containerId, k -> new ArrayList<>())
                .add(new ModWatchInfo().groupId(groupId).botId(botId).commentType(commentType).containerId(containerId));
    }

    /**
     * 删除监控目标
     *
     * @param commentType 评论类型
     * @param containerId 容器ID
     * @param groupId     群号
     */
    public void removeModWatch(EnumCommentType commentType, String containerId, Long groupId) {
        List<ModWatchInfo> watchList = modWatchMap.get(containerId);
        if (watchList != null) {
            watchList.removeIf(info -> info.groupId().equals(groupId)
                    && info.commentType() == commentType);
            if (watchList.isEmpty()) {
                modWatchMap.remove(containerId);
            }
        }
    }

    /**
     * 检查指定群是否监控指定容器
     *
     * @param commentType 评论类型
     * @param containerId 容器ID
     * @param groupId     群号
     * @return 是否监控
     */
    public boolean isWatching(EnumCommentType commentType, String containerId, Long groupId) {
        List<ModWatchInfo> watchList = modWatchMap.get(containerId);
        if (watchList == null) {
            return false;
        }
        return watchList.stream().anyMatch(info -> info.groupId().equals(groupId)
                && info.commentType() == commentType);
    }

}

