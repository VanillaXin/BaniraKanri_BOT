package xin.vanilla.banira.util.mcmod;

import lombok.Getter;
import lombok.experimental.Accessors;

@Getter
@Accessors(fluent = true)
public enum EnumCommentType {
    MOD("class"),
    MODPACK("modpack"),
    AUTHOR("author"),
    USER_CENTER("center"),
    ;

    private final String value;

    EnumCommentType(String value) {
        this.value = value;
    }
}
