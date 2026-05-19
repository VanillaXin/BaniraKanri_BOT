package xin.vanilla.banira.config.entity.group;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import xin.vanilla.banira.config.contract.GroupConfig;
import xin.vanilla.banira.config.entity.extended.WifeConfig;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.List;

/**
 * 抽老婆群配置。
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class WifeGroupConfig implements GroupConfig {

    /**
     * 抽老婆规则。
     */
    private List<WifeConfig> wifeConfig;

    {
        this.wifeConfig = BaniraUtils.mutableListOf(new WifeConfig("^抽(?<nick>.{2,5})$"
                , "$nick"
                , "$atUser 今天你的群友$wifeNick是\n$wifeHead『$wifeName』($wifeId) 喵！"
                , "$atUser 今天你已经有$wifeNick了喵！")
        );
    }
}
