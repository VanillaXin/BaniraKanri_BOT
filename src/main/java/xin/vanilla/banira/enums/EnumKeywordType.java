package xin.vanilla.banira.enums;

import lombok.Getter;
import xin.vanilla.banira.config.entity.basic.KeyInstructionsConfig;
import xin.vanilla.banira.util.BaniraUtils;

@Getter
public enum EnumKeywordType {
    EXACT("完全匹配"),
    CONTAIN("包含匹配"),
    PINYIN("拼音匹配"),
    REGEX("正则匹配"),
    ;

    private final String desc;

    EnumKeywordType(String desc) {
        this.desc = desc;
    }

    public static EnumKeywordType valueFrom(String name) {
        for (EnumKeywordType value : EnumKeywordType.values()) {
            if (value.name().equalsIgnoreCase(name)
                    || value.desc.equalsIgnoreCase(name)
            ) {
                return value;
            }
        }
        KeyInstructionsConfig keyIns = BaniraUtils.getKeyIns();
        if (keyIns.exactly().contains(name)) {
            return EXACT;
        } else if (keyIns.contain().contains(name)) {
            return CONTAIN;
        } else if (keyIns.pinyin().contains(name)) {
            return PINYIN;
        } else if (keyIns.regex().contains(name)) {
            return REGEX;
        }
        throw new IllegalArgumentException("Invalid keyword type: " + name);
    }
}
