package xin.vanilla.banira.config.entity;


import jakarta.annotation.Nonnull;
import lombok.experimental.Accessors;
import xin.vanilla.banira.config.entity.basic.OtherConfig;
import xin.vanilla.banira.config.entity.basic.PermissionConfig;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.List;
import java.util.Map;

/**
 * 群配置
 *
 * @param maid        女仆： group -> maid，0为管家
 * @param otherConfig 其他配置： group -> otherConfig，0为全局
 */
@Accessors(chain = true)
public record GroupConfig(
        @Nonnull Map<Long, List<PermissionConfig>> maid,
        @Nonnull Map<Long, OtherConfig> otherConfig
) {

    public static GroupConfig preset() {
        return new GroupConfig(
                BaniraUtils.mutableMapOf(),
                BaniraUtils.mutableMapOf(
                        0L, OtherConfig.preset()
                )
        );
    }

}
