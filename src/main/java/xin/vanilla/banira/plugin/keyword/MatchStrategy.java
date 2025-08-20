package xin.vanilla.banira.plugin.keyword;

import xin.vanilla.banira.enums.EnumKeywordType;

/**
 * 匹配策略
 */
public interface MatchStrategy {

    boolean match(String input, String keyword);

    EnumKeywordType getMatchType();

}
