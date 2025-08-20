package xin.vanilla.banira.plugin.keyword;

import org.springframework.stereotype.Component;
import xin.vanilla.banira.enums.EnumKeywordType;

/**
 * 包含匹配
 */
@Component
public class ContainsMatchStrategy implements MatchStrategy {

    @Override
    public boolean match(String input, String keyword) {
        return input.contains(keyword);
    }

    @Override
    public EnumKeywordType getMatchType() {
        return EnumKeywordType.CONTAIN;
    }
}
