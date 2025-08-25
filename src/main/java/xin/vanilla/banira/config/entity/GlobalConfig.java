package xin.vanilla.banira.config.entity;

import jakarta.annotation.Nonnull;
import lombok.experimental.Accessors;
import xin.vanilla.banira.config.entity.basic.PluginConfig;

import java.util.Arrays;
import java.util.List;

/**
 * 全局配置
 *
 * @param botNick      机器人昵称
 * @param owner        主人
 * @param backGroup    后台群
 * @param pluginConfig 插件配置
 */
@Accessors(chain = true)
public record GlobalConfig(
        @Nonnull String token,
        @Nonnull String wsUrl,
        @Nonnull Long owner,
        @Nonnull String botNick,
        @Nonnull List<Long> backGroup,
        @Nonnull PluginConfig pluginConfig
) {

    public static GlobalConfig preset() {
        return new GlobalConfig(
                "",
                "ws://127.0.0.1:8080",
                0L,
                "香草酱",
                Arrays.asList(),
                PluginConfig.preset()
        );
    }

}
