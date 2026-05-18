package xin.vanilla.banira.service;

import xin.vanilla.banira.enums.EnumKeywordType;
import xin.vanilla.banira.service.model.CachedKeyword;
import xin.vanilla.banira.util.AhoCorasick;

import java.util.List;
import java.util.Set;

/**
 * 关键词缓存管理服务
 */
@SuppressWarnings("unused")
public interface IKeywordCacheManager {

    void loadKeywords();

    List<CachedKeyword> getKeywordsByType(EnumKeywordType matchType);

    Set<String> getExactMatchKeywords();

    AhoCorasick<CachedKeyword> getAhoCorasick(EnumKeywordType matchType);

}
