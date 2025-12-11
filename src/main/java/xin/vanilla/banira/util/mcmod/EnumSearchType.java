package xin.vanilla.banira.util.mcmod;

import lombok.Getter;
import lombok.experimental.Accessors;

/**
 * 搜索类型
 */
@Getter
@Accessors(fluent = true)
public enum EnumSearchType {
    MOD("post_relation_mod"),
    MODPACK("post_relation_modpack"),
    AUTHOR("author"),
    ;

    private final String value;

    EnumSearchType(String value) {
        this.value = value;
    }
}
