package xin.vanilla.banira.service.impl;

import jakarta.annotation.Resource;
import org.springframework.stereotype.Service;
import xin.vanilla.banira.domain.KeywordRecord;
import xin.vanilla.banira.enums.EnumKeywordType;
import xin.vanilla.banira.plugin.keyword.MatchStrategy;
import xin.vanilla.banira.plugin.keyword.MatchStrategyFactory;
import xin.vanilla.banira.service.IKeywordCacheManager;
import xin.vanilla.banira.service.IKeywordManager;
import xin.vanilla.banira.util.AhoCorasick;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Random;

@Service("keywordManager")
public class KeywordManager implements IKeywordManager {

    @Resource
    private IKeywordCacheManager keywordCacheService;
    @Resource
    private MatchStrategyFactory strategyFactory;

    private final Random random = new Random();

    @Override
    public KeywordRecord findMatchReply(String input, Long botId, Long groupId) {
        List<KeywordCacheManager.CachedKeyword> matches = new ArrayList<>();

        // 处理每种匹配类型
        for (EnumKeywordType type : EnumKeywordType.values()) {
            switch (type) {
                // 完全匹配
                case EXACT: {
                    if (keywordCacheService.getExactMatchKeywords().contains(input)) {
                        matches.addAll(findExactMatches(input));
                    }
                }
                break;
                // 包含匹配
                case CONTAIN: {
                    AhoCorasick<KeywordCacheManager.CachedKeyword> ahoCorasick = keywordCacheService.getAhoCorasick(EnumKeywordType.CONTAIN);
                    if (ahoCorasick != null) {
                        Collection<KeywordCacheManager.CachedKeyword> results = ahoCorasick.search(input);
                        for (KeywordCacheManager.CachedKeyword result : results) {
                            if (result != null) {
                                matches.add(result);
                            }
                        }
                    }
                }
                break;
                // 拼音匹配、正则匹配
                case PINYIN:
                case REGEX: {
                    List<KeywordCacheManager.CachedKeyword> keywords = keywordCacheService.getKeywordsByType(type);
                    MatchStrategy strategy = strategyFactory.getStrategy(type);
                    for (KeywordCacheManager.CachedKeyword keyword : keywords) {
                        if (strategy.match(input, keyword.getKeyword())) {
                            matches.add(keyword);
                        }
                    }
                }
                break;
            }
        }
        matches.removeIf(keyword -> (BaniraUtils.isFriendIdValid(botId) && !keyword.getBotId().equals(botId))
                || (BaniraUtils.isGroupIdValid(groupId) && !keyword.getGroupId().equals(groupId))
        );
        if (matches.isEmpty()) return null;

        // 随机返回一条匹配记录
        return matches.get(this.random.nextInt(matches.size())).toKeywordRecord();
    }

    private List<KeywordCacheManager.CachedKeyword> findExactMatches(String input) {
        List<KeywordCacheManager.CachedKeyword> result = new ArrayList<>();
        List<KeywordCacheManager.CachedKeyword> exactKeywords =
                keywordCacheService.getKeywordsByType(EnumKeywordType.EXACT);

        for (KeywordCacheManager.CachedKeyword keyword : exactKeywords) {
            if (input.equals(keyword.getKeyword())) {
                result.add(keyword);
            }
        }

        return result;
    }
}
