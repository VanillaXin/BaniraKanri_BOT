package xin.vanilla.banira.service.impl;

import jakarta.annotation.Resource;
import lombok.Getter;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;
import xin.vanilla.banira.domain.KeywordRecord;
import xin.vanilla.banira.enums.EnumDataOperateType;
import xin.vanilla.banira.enums.EnumKeywordType;
import xin.vanilla.banira.event.DatabaseInitializedEvent;
import xin.vanilla.banira.event.KeywordChangedEvent;
import xin.vanilla.banira.mapper.param.KeywordRecordQueryParam;
import xin.vanilla.banira.service.IKeywordCacheManager;
import xin.vanilla.banira.service.IKeywordRecordManager;
import xin.vanilla.banira.util.AhoCorasick;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

@Service("keywordCacheManager")
public class KeywordCacheManager implements IKeywordCacheManager {
    private final Map<EnumKeywordType, List<CachedKeyword>> keywordsByType = new ConcurrentHashMap<>();
    private final Map<EnumKeywordType, Set<String>> exactMatchKeywords = new ConcurrentHashMap<>();
    private final Map<EnumKeywordType, AhoCorasick<CachedKeyword>> ahoCorasickMap = new ConcurrentHashMap<>();
    @Getter
    private volatile long lastUpdateTime = 0;

    @Resource
    private IKeywordRecordManager keywordRecordManager;

    @EventListener
    public void init(DatabaseInitializedEvent event) {
        this.loadKeywords();
    }

    @Override
    public void loadKeywords() {
        List<KeywordRecord> allKeywords = keywordRecordManager.getKeywordRecordList(
                new KeywordRecordQueryParam().setEnable(true).setAudited(true)
        );

        // 清空现有缓存
        this.keywordsByType.clear();
        this.exactMatchKeywords.clear();
        this.ahoCorasickMap.clear();

        // 按匹配类型分组
        for (KeywordRecord keyword : allKeywords) {
            CachedKeyword cached = new CachedKeyword(keyword);
            this.keywordsByType
                    .computeIfAbsent(keyword.getKeywordType(), k -> new ArrayList<>())
                    .add(cached);

            // 为完全匹配创建快速查找集合
            if (keyword.getKeywordType() == EnumKeywordType.EXACT) {
                this.exactMatchKeywords
                        .computeIfAbsent(EnumKeywordType.EXACT, k -> new HashSet<>())
                        .add(keyword.getKeyword());
            }
        }

        // 为包含匹配创建Aho-Corasick自动机
        this.buildAhoCorasickAutomaton();

        this.lastUpdateTime = System.currentTimeMillis();
    }

    @Override
    public List<CachedKeyword> getKeywordsByType(EnumKeywordType matchType) {
        return this.keywordsByType.getOrDefault(matchType, Collections.emptyList());
    }

    @Override
    public Set<String> getExactMatchKeywords() {
        return this.exactMatchKeywords.getOrDefault(EnumKeywordType.EXACT, Collections.emptySet());
    }

    @Override
    public AhoCorasick<CachedKeyword> getAhoCorasick(EnumKeywordType matchType) {
        return this.ahoCorasickMap.get(matchType);
    }

    @Getter
    public static class CachedKeyword {
        /**
         * 关键词记录ID
         */
        private final Long id;
        /**
         * 机器人ID
         */
        private final Long botId;
        /**
         * 群组ID
         */
        private final Long groupId;
        /**
         * 添加人
         */
        private final Long creatorId;
        /**
         * 添加时间
         */
        private final Long time;
        /**
         * 关键词类型
         */
        private final EnumKeywordType keywordType;
        /**
         * 关键词
         */
        private final String keyword;
        /**
         * 回复消息
         */
        private final String replyMsg;

        public CachedKeyword(KeywordRecord keywordRecord) {
            this.id = keywordRecord.getId();
            this.botId = keywordRecord.getBotId();
            this.groupId = keywordRecord.getGroupId();
            this.creatorId = keywordRecord.getCreatorId();
            this.time = keywordRecord.getTime();
            this.keywordType = keywordRecord.getKeywordType();
            this.keyword = keywordRecord.getKeyword();
            this.replyMsg = keywordRecord.getReplyMsg();
        }

        public KeywordRecord toKeywordRecord() {
            return new KeywordRecord()
                    .setId(id)
                    .setBotId(botId)
                    .setGroupId(groupId)
                    .setCreatorId(creatorId)
                    .setTime(time)
                    .setKeywordType(keywordType)
                    .setKeyword(keyword)
                    .setReplyMsg(replyMsg);
        }

        public Long getGroupId() {
            return this.groupId != null ? this.groupId : 0L;
        }
    }

    private void buildAhoCorasickAutomaton() {
        List<CachedKeyword> containsKeywords = this.keywordsByType.getOrDefault(EnumKeywordType.CONTAIN, Collections.emptyList());
        if (!containsKeywords.isEmpty()) {
            AhoCorasick<CachedKeyword> ahoCorasick = new AhoCorasick<>();
            for (CachedKeyword keyword : containsKeywords) {
                ahoCorasick.addKeyword(keyword.getKeyword(), keyword);
            }
            ahoCorasick.buildFailureLinks();
            this.ahoCorasickMap.put(EnumKeywordType.CONTAIN, ahoCorasick);
        }
    }

    /**
     * 监听关键词变化事件并刷新缓存
     */
    @EventListener
    public void onKeywordChanged(KeywordChangedEvent event) {
        if (event.getOperate() != null && event.getOperate() != EnumDataOperateType.GET) {
            this.loadKeywords();
        }
    }

}
