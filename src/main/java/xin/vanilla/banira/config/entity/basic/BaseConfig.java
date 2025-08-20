package xin.vanilla.banira.config.entity.basic;

import lombok.experimental.Accessors;
import xin.vanilla.banira.plugin.*;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.Map;
import java.util.Set;

/**
 * 基础配置
 *
 * @param backGroup  后台群
 * @param capability 插件启用状态
 */
@Accessors(chain = true)
public record BaseConfig(
        Set<Long> backGroup,
        Map<String, Integer> capability
) {

    public static BaseConfig empty() {
        return new BaseConfig(
                BaniraUtils.mutableSetOf(),
                BaniraUtils.mutableMapOf()
        );
    }

    public static BaseConfig preset() {
        return new BaseConfig(
                BaniraUtils.mutableSetOf(),
                BaniraUtils.mutableMapOf(
                        KanriPlugin.class.getName(), 1
                        , HelpPlugin.class.getName(), 2
                        , ExamplePlugin.class.getName(), 99
                        , ImageFaceToImagePlugin.class.getName(), 99
                        , WifePlugin.class.getName(), 99
                        , StatusPlugin.class.getName(), 99
                )
        );
    }

}
