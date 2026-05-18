package xin.vanilla.banira.service.model;

import lombok.Getter;
import xin.vanilla.banira.domain.KeywordRecord;
import xin.vanilla.banira.enums.EnumKeywordType;

public class CachedKeyword {
    @Getter
    private final Long id;
    @Getter
    private final Long botId;
    private final Long groupId;
    @Getter
    private final Long creatorId;
    @Getter
    private final Long time;
    @Getter
    private final EnumKeywordType keywordType;
    @Getter
    private final String keyword;
    @Getter
    private final String replyMsg;
    @Getter
    private final Integer priority;

    public CachedKeyword(KeywordRecord keywordRecord) {
        this.id = keywordRecord.getId();
        this.botId = keywordRecord.getBotId();
        this.groupId = keywordRecord.getGroupId();
        this.creatorId = keywordRecord.getCreatorId();
        this.time = keywordRecord.getTime();
        this.keywordType = keywordRecord.getKeywordType();
        this.keyword = keywordRecord.getKeyword();
        this.replyMsg = keywordRecord.getReplyMsg();
        this.priority = keywordRecord.getPriority();
    }

    public Long getGroupId() {
        return this.groupId != null ? this.groupId : 0L;
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
                .setReplyMsg(replyMsg)
                .setPriority(priority);
    }
}
