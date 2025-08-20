package xin.vanilla.banira.plugin.keyword;

import org.springframework.stereotype.Component;
import xin.vanilla.banira.enums.EnumKeywordType;

import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

/**
 * 正则匹配
 */
@Component
public class RegexMatchStrategy implements MatchStrategy {
    private static final Map<String, Pattern> patternCache = new HashMap<>();

    @Override
    public boolean match(String input, String regex) {
        return patternCache.computeIfAbsent(regex, Pattern::compile).matcher(input).matches();
    }

    @Override
    public EnumKeywordType getMatchType() {
        return EnumKeywordType.REGEX;
    }

}
