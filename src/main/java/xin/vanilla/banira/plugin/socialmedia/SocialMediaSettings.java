package xin.vanilla.banira.plugin.socialmedia;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.List;

/**
 * 社交媒体插件配置
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class SocialMediaSettings {

    /**
     * 外部自定义接口列表
     */
    private List<SocialMediaApiSettings> apis;

    {
        this.apis = BaniraUtils.mutableListOf();
    }
}
