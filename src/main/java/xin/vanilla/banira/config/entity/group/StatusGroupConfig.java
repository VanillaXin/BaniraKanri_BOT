package xin.vanilla.banira.config.entity.group;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import xin.vanilla.banira.config.contract.GroupConfig;

import java.util.ArrayList;
import java.util.List;

/**
 * 状态插件群配置。
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class StatusGroupConfig implements GroupConfig {

    /**
     * 随机图片路径。
     */
    private List<String> randomImgPath;

    /**
     * 状态查询背景图地址。
     */
    private String statusBgUrl;

    {
        this.randomImgPath = new ArrayList<>();
        this.statusBgUrl = "bg.png";
    }
}
