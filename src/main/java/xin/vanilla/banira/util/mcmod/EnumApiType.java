package xin.vanilla.banira.util.mcmod;

import lombok.Getter;
import lombok.experimental.Accessors;

/**
 * mod运作方式
 */
@Getter
@Accessors(fluent = true)
public enum EnumApiType {
    Forge(1),
    Fabric(2),
    Rift(3),
    LiteLoader(4),
    DataPack(5),
    CommandBlock(6),
    FileOverwrite(7),
    BehaviorPack(8),
    Sandbox(9),
    Other(10),
    Quilt(11),
    ResourcePack(12),
    Neoforge(13),
    ;

    private final int value;

    EnumApiType(int value) {
        this.value = value;
    }
}
