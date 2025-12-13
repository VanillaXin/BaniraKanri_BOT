package xin.vanilla.banira.util.mcmod;


import lombok.Getter;
import lombok.experimental.Accessors;

/**
 * 模组分类
 */
@Getter
@Accessors(fluent = true)
public enum EnumModCategory {
    科技(1, "核心元素"),
    魔法(2, "核心元素"),
    冒险(3, "核心元素"),
    农业(4, "核心元素"),
    装饰(5, "核心元素"),
    实用(23, "核心元素"),
    辅助(24, "核心元素"),
    魔改(21, "核心元素"),
    LIB(7, "核心元素"),

    资源(8, "世界生成器"),
    世界(9, "世界生成器"),
    群系(10, "世界生成器"),
    结构(35, "世界生成器"),
    生物(11, "世界生成器"),

    存储(13, "杂项"),
    物流(14, "杂项"),
    道具(15, "杂项"),
    安全(6, "杂项"),
    红石(16, "杂项"),
    食物(17, "杂项"),
    模型(18, "杂项"),
    关卡(34, "杂项"),
    指南(19, "杂项"),
    破坏(20, "杂项"),
    Meme(22, "杂项"),
    中式(25, "杂项"),
    日式(26, "杂项"),
    西式(27, "杂项"),
    恐怖(28, "杂项"),
    建材(29, "杂项"),
    生存(30, "杂项"),
    指令(31, "杂项"),
    优化(32, "杂项"),
    国创(33, "杂项"),
    ;

    private final int value;
    private final String group;

    EnumModCategory(int value, String group) {
        this.value = value;
        this.group = group;
    }

    public static EnumModCategory valueOfEx(String name) {
        for (EnumModCategory value : values()) {
            if (value.name().equals(name) || ("c_" + value.value()).equalsIgnoreCase(name)) {
                return value;
            }
        }
        return null;
    }
}
