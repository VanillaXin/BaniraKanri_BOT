package xin.vanilla.banira.config.entity.basic;

import lombok.experimental.Accessors;
import xin.vanilla.banira.enums.EnumPermission;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.Set;

/**
 * 权限配置
 *
 * @param id          身份标识
 * @param permissions 权限
 */
@Accessors(chain = true)
public record PermissionConfig(
        Long id,
        Set<EnumPermission> permissions
) {

    public static PermissionConfig preset() {
        return new PermissionConfig(
                null,
                BaniraUtils.mutableSetOf()
        );
    }

}
