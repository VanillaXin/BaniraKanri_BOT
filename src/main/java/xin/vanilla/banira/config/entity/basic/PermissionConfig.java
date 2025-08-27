package xin.vanilla.banira.config.entity.basic;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;
import xin.vanilla.banira.enums.EnumPermission;

import java.util.List;

/**
 * 权限配置
 */
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class PermissionConfig {

    /**
     * 身份标识
     */
    private Long id;
    /**
     * 权限
     */
    private List<EnumPermission> permissions;

}
