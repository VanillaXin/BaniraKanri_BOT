package xin.vanilla.banira.config.entity.basic;

import lombok.experimental.Accessors;
import xin.vanilla.banira.plugin.*;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * 基础配置
 *
 * @param capability 插件启用状态
 */
@Accessors(chain = true)
public record PluginConfig(
        Map<String, Integer> capability
) {

    public static PluginConfig preset() {
        return new PluginConfig(
                new LinkedHashMap<>() {{
                    put(KanriPlugin.class.getName(), 1);
                    put(HelpPlugin.class.getName(), 2);
                    put(TimerPlugin.class.getName(), 2);
                    put(StatusPlugin.class.getName(), 2);
                    put(KeywordPlugin.class.getName(), 2);

                    put(ExamplePlugin.class.getName(), 0);
                    put(WifePlugin.class.getName(), 99);
                    put(McQueryPlugin.class.getName(), 99);
                    put(ImageFaceToImagePlugin.class.getName(), 99);
                }}
        );
    }

}
