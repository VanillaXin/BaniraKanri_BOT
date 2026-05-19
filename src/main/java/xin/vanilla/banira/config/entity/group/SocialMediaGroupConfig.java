package xin.vanilla.banira.config.entity.group;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import xin.vanilla.banira.config.contract.GroupConfig;
import xin.vanilla.banira.plugin.socialmedia.SocialMediaGroupSettings;

/**
 * 社交媒体群配置。
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class SocialMediaGroupConfig implements GroupConfig {

    /**
     * 社交媒体解析启用开关。
     */
    private boolean socialMedia;

    /**
     * 社交媒体触发与回复配置。
     */
    private SocialMediaGroupSettings socialMediaSettings;

    {
        this.socialMedia = false;
        this.socialMediaSettings = new SocialMediaGroupSettings();
    }
}
