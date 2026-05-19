package xin.vanilla.banira.config.entity.group;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import xin.vanilla.banira.config.contract.GroupConfig;
import xin.vanilla.banira.config.entity.extended.McModCommentConfig;
import xin.vanilla.banira.config.entity.extended.McModCookieConfig;

/**
 * MCMod群配置。
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class McModGroupConfig implements GroupConfig {

    /**
     * MCMod评论监控配置。
     */
    private McModCommentConfig mcModCommentConfig;

    /**
     * MCMod Cookie 配置。
     */
    private McModCookieConfig mcModCookieConfig;

    {
        this.mcModCommentConfig = new McModCommentConfig();
        this.mcModCookieConfig = new McModCookieConfig();
    }
}
