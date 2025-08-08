package xin.vanilla.banira.config.entity;

import jakarta.annotation.Nonnull;
import lombok.experimental.Accessors;
import xin.vanilla.banira.config.entity.basic.BaseConfig;
import xin.vanilla.banira.config.entity.basic.InstructionsConfig;
import xin.vanilla.banira.config.entity.basic.OtherConfig;
import xin.vanilla.banira.config.entity.basic.PermissionConfig;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.Set;

/**
 * 全局配置
 *
 * @param owner       主人
 * @param butler      管家
 * @param baseConfig  基础配置
 * @param instConfig  指令配置
 * @param otherConfig 其他配置
 */
@Accessors(chain = true)
public record GlobalConfig(
        @Nonnull String token,
        @Nonnull String wsUrl,
        @Nonnull Long owner,
        @Nonnull Set<PermissionConfig> butler,
        @Nonnull BaseConfig baseConfig,
        @Nonnull InstructionsConfig instConfig,
        @Nonnull OtherConfig otherConfig
) {

    public static GlobalConfig preset() {
        return new GlobalConfig(
                "",
                "ws://127.0.0.1:8080",
                0L,
                BaniraUtils.mutableSetOf(PermissionConfig.preset()),
                BaseConfig.preset(),
                InstructionsConfig.preset(),
                OtherConfig.preset()
        );
    }

}
