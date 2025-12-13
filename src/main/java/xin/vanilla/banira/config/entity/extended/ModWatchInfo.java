package xin.vanilla.banira.config.entity.extended;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import xin.vanilla.banira.util.mcmod.EnumContentType;

/**
 * Mod监控信息
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class ModWatchInfo {

    /**
     * 群号
     */
    private Long groupId;

    /**
     * Bot ID
     */
    private Long botId;

    /**
     * 评论类型
     */
    private EnumContentType commentType;

    /**
     * 容器ID（mod编号、作者ID、用户ID等）
     */
    private String containerId;

}
