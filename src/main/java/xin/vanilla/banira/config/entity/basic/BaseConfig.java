package xin.vanilla.banira.config.entity.basic;

import lombok.experimental.Accessors;
import xin.vanilla.banira.plugin.ExamplePlugin;
import xin.vanilla.banira.plugin.KanriPlugin;
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

    public static BaseConfig preset() {
        return new BaseConfig(
                BaniraUtils.mutableSetOf(),
                BaniraUtils.mutableMapOf(
                        KanriPlugin.class.getName(), 1
                        , ExamplePlugin.class.getName(), 99
                )
        );
    }

}
