package xin.vanilla.banira.util.mcmod;

import lombok.Getter;
import lombok.experimental.Accessors;

/**
 * 搜索类型
 */
@Getter
@Accessors(fluent = true)
public enum EnumSearchType {
    MOD("post_relation_mod", 1),
    MODPACK("post_relation_modpack", 2),
    ITEM(3),
    TUTORIAL(4),
    AUTHOR("author", 5),
    USER(6),
    ;

    private final String value;
    private final int filter;

    EnumSearchType(int filter) {
        this.filter = filter;
        this.value = null;
    }

    EnumSearchType(String value, int filter) {
        this.value = value;
        this.filter = filter;
    }

    public EnumContentType toContentType() {
        return EnumContentType.valueOfEx(this.name());
    }
}
