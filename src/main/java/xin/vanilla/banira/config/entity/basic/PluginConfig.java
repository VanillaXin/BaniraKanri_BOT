package xin.vanilla.banira.config.entity.basic;

import lombok.experimental.Accessors;
import xin.vanilla.banira.plugin.*;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.Map;
import java.util.Set;

/**
 * 基础配置
 *
 * @param capability 插件启用状态
 */
@Accessors(chain = true)
public record PluginConfig(
        Map<String, Integer> capability
) {

    public static PluginConfig empty() {
        return new PluginConfig(
                BaniraUtils.mutableMapOf()
        );
    }

    public static PluginConfig preset() {
        return new PluginConfig(
                BaniraUtils.mutableMapOf(
                        KanriPlugin.class.getName(), 1
                        , HelpPlugin.class.getName(), 2
                        , KeywordPlugin.class.getName(), 2
                        , TimerPlugin.class.getName(), 2
                        , ExamplePlugin.class.getName(), 99
                        , ImageFaceToImagePlugin.class.getName(), 99
                        , WifePlugin.class.getName(), 99
                        , StatusPlugin.class.getName(), 99
                        , McQueryPlugin.class.getName(), 99
                )
        );
    }

}
