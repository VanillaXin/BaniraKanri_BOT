package xin.vanilla.banira.util.mcmod;

import lombok.Getter;
import lombok.experimental.Accessors;

@Getter
@Accessors(fluent = true)
public enum EnumContentType {
    MOD("class"),
    MODPACK("modpack"),
    AUTHOR("author"),
    USER_CENTER("center"),
    ;

    private final String value;

    EnumContentType(String value) {
        this.value = value;
    }

    public static EnumContentType valueOfEx(String value) {
        for (EnumContentType type : values()) {
            if (type.value.equalsIgnoreCase(value) || type.name().equalsIgnoreCase(value)) {
                return type;
            }
        }
        return null;
    }
}
