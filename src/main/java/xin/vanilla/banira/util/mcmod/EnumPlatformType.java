package xin.vanilla.banira.util.mcmod;

import lombok.Getter;
import lombok.experimental.Accessors;

@Getter
@Accessors(fluent = true)
public enum EnumPlatformType {
    Java(1),
    Bedrock(2),
    ;

    private final int value;

    EnumPlatformType(int value) {
        this.value = value;
    }
}
