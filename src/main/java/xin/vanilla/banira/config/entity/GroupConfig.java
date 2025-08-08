package xin.vanilla.banira.config.entity;


import jakarta.annotation.Nonnull;
import lombok.experimental.Accessors;
import xin.vanilla.banira.config.entity.basic.BaseConfig;
import xin.vanilla.banira.config.entity.basic.OtherConfig;
import xin.vanilla.banira.config.entity.basic.PermissionConfig;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * 群配置
 *
 * @param servant     仆人： group -> servant
 * @param baseConfig  基础配置： group -> baseConfig
 * @param otherConfig 其他配置： group -> otherConfig
 */
@Accessors(chain = true)
public record GroupConfig(
        @Nonnull Map<Long, Set<PermissionConfig>> servant,
        @Nonnull Map<Long, BaseConfig> baseConfig,
        @Nonnull Map<Long, OtherConfig> otherConfig
) {

    public static GroupConfig preset() {
        return new GroupConfig(
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>()
        );
    }

}
