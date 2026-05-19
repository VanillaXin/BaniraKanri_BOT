package xin.vanilla.banira.config.entity;


import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import xin.vanilla.banira.config.entity.basic.PermissionConfig;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * 群配置
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class GroupConfig {
    /**
     * 女仆： group -> maid，0为管家
     */
    private Map<Long, List<PermissionConfig>> maid;
    {
        this.maid = new LinkedHashMap<>();
        this.maid.put(0L, new ArrayList<>());
    }

}
