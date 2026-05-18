package xin.vanilla.banira.service.impl;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;
import xin.vanilla.banira.domain.KeywordRecord;
import xin.vanilla.banira.enums.EnumKeywordType;
import xin.vanilla.banira.plugin.keyword.MatchStrategyFactory;
import xin.vanilla.banira.service.IKeywordCacheManager;
import xin.vanilla.banira.service.model.CachedKeyword;

import java.util.Collections;
import java.util.List;
import java.util.Set;

class KeywordManagerTest {

    @Test
    void shouldFindExactMatchReply() {
        IKeywordCacheManager cacheManager = Mockito.mock(IKeywordCacheManager.class);
        MatchStrategyFactory strategyFactory = Mockito.mock(MatchStrategyFactory.class);
        KeywordManager keywordManager = new KeywordManager();
        ReflectionTestUtils.setField(keywordManager, "keywordCacheService", cacheManager);
        ReflectionTestUtils.setField(keywordManager, "strategyFactory", strategyFactory);

        KeywordRecord source = new KeywordRecord()
                .setId(1L)
                .setBotId(1000L)
                .setGroupId(2000L)
                .setKeywordType(EnumKeywordType.EXACT)
                .setKeyword("hello")
                .setReplyMsg("world")
                .setPriority(1);
        CachedKeyword cachedKeyword = new CachedKeyword(source);

        Mockito.when(cacheManager.getExactMatchKeywords()).thenReturn(Set.of("hello"));
        Mockito.when(cacheManager.getKeywordsByType(EnumKeywordType.EXACT)).thenReturn(List.of(cachedKeyword));
        Mockito.when(cacheManager.getKeywordsByType(EnumKeywordType.PINYIN)).thenReturn(Collections.emptyList());
        Mockito.when(cacheManager.getKeywordsByType(EnumKeywordType.REGEX)).thenReturn(Collections.emptyList());
        Mockito.when(cacheManager.getAhoCorasick(EnumKeywordType.CONTAIN)).thenReturn(null);

        KeywordRecord result = keywordManager.findMatchReply("hello", 1000L, 2000L);
        Assertions.assertNotNull(result);
        Assertions.assertEquals("world", result.getReplyMsg());
    }
}
