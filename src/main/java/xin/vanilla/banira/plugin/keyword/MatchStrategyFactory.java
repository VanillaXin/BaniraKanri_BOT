package xin.vanilla.banira.plugin.keyword;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.enums.EnumKeywordType;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 匹配策略工厂
 */
@Component
public class MatchStrategyFactory {

    private final Map<EnumKeywordType, MatchStrategy> strategyMap = new HashMap<>();

    @Autowired
    public MatchStrategyFactory(List<MatchStrategy> strategies) {
        for (MatchStrategy strategy : strategies) {
            strategyMap.put(strategy.getMatchType(), strategy);
        }
    }

    public MatchStrategy getStrategy(EnumKeywordType matchType) {
        return strategyMap.get(matchType);
    }

}
