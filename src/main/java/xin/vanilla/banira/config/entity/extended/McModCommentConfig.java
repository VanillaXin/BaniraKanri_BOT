package xin.vanilla.banira.config.entity.extended;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

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
     * 检测目标列表（mod编号 -> 监控信息列表）
     * 每个mod可以对应多个群，每个群有自己的botId
     */
    private Map<String, List<ModWatchInfo>> modWatchMap;

    {
        this.modWatchMap = new LinkedHashMap<>();
    }

    /**
     * 添加监控目标
     *
     * @param modId   mod编号
     * @param groupId 群号
     * @param botId   Bot ID
     */
    public void addModWatch(String modId, Long groupId, Long botId) {
        modWatchMap.computeIfAbsent(modId, k -> new ArrayList<>())
                .add(new ModWatchInfo().groupId(groupId).botId(botId));
    }

    /**
     * 删除监控目标
     *
     * @param modId   mod编号
     * @param groupId 群号
     */
    public void removeModWatch(String modId, Long groupId) {
        List<ModWatchInfo> watchList = modWatchMap.get(modId);
        if (watchList != null) {
            watchList.removeIf(info -> info.groupId().equals(groupId));
            if (watchList.isEmpty()) {
                modWatchMap.remove(modId);
            }
        }
    }

    /**
     * 检查指定群是否监控指定mod
     *
     * @param modId   mod编号
     * @param groupId 群号
     * @return 是否监控
     */
    public boolean isWatching(String modId, Long groupId) {
        List<ModWatchInfo> watchList = modWatchMap.get(modId);
        if (watchList == null) {
            return false;
        }
        return watchList.stream().anyMatch(info -> info.groupId().equals(groupId));
    }

}

