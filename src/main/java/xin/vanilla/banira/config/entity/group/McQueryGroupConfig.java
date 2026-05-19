package xin.vanilla.banira.config.entity.group;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import xin.vanilla.banira.config.contract.GroupConfig;
import xin.vanilla.banira.config.entity.extended.McConfig;

/**
 * MC查询群配置。
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class McQueryGroupConfig implements GroupConfig {

    /**
     * 我的世界服务器状态查询配置。
     */
    private McConfig mcConfig;

    {
        this.mcConfig = new McConfig();
    }
}
