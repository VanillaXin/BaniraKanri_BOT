package xin.vanilla.banira.plugin.keyword;

import org.springframework.stereotype.Component;
import xin.vanilla.banira.enums.EnumKeywordType;

/**
 * 完全匹配
 */
@Component
public class ExactMatchStrategy implements MatchStrategy {

    @Override
    public boolean match(String input, String keyword) {
        return input.equals(keyword);
    }

    @Override
    public EnumKeywordType getMatchType() {
        return EnumKeywordType.EXACT;
    }

}
