package xin.vanilla.banira.mapper.param;


import xin.vanilla.banira.domain.KeywordRecord;
import xin.vanilla.banira.mapper.common.BaniraQueryParam;
import xin.vanilla.banira.util.lambda.LambdaUtils;

import java.util.Collection;

/**
 * KeywordRecord 查询参数类
 */
@SuppressWarnings("unused")
public class KeywordRecordQueryParam extends BaniraQueryParam {

    // region 查询常量

    public static final String QUERY_ID = LambdaUtils.getFiledName(KeywordRecord::getId);
    public static final String QUERY_BOT_ID = LambdaUtils.getFiledName(KeywordRecord::getBotId);
    public static final String QUERY_GROUP_ID = LambdaUtils.getFiledName(KeywordRecord::getGroupId);
    public static final String QUERY_CREATOR_ID = LambdaUtils.getFiledName(KeywordRecord::getCreatorId);
    public static final String QUERY_TIME = LambdaUtils.getFiledName(KeywordRecord::getTime);
    public static final String QUERY_KEYWORD_TYPE = LambdaUtils.getFiledName(KeywordRecord::getKeywordType);
    public static final String QUERY_KEYWORD = LambdaUtils.getFiledName(KeywordRecord::getKeyword);
    public static final String QUERY_REPLY_MSG = LambdaUtils.getFiledName(KeywordRecord::getReplyMsg);
    public static final String QUERY_ENABLE = LambdaUtils.getFiledName(KeywordRecord::isEnable);

    // endregion

    // region 排序常量

    public static final String ORDER_ID = LambdaUtils.getFiledName(KeywordRecord::getId);
    public static final String ORDER_BOT_ID = LambdaUtils.getFiledName(KeywordRecord::getBotId);
    public static final String ORDER_GROUP_ID = LambdaUtils.getFiledName(KeywordRecord::getGroupId);
    public static final String ORDER_CREATOR_ID = LambdaUtils.getFiledName(KeywordRecord::getCreatorId);
    public static final String ORDER_TIME = LambdaUtils.getFiledName(KeywordRecord::getTime);
    public static final String ORDER_KEYWORD_TYPE = LambdaUtils.getFiledName(KeywordRecord::getKeywordType);
    public static final String ORDER_KEYWORD = LambdaUtils.getFiledName(KeywordRecord::getKeyword);
    public static final String ORDER_REPLY_MSG = LambdaUtils.getFiledName(KeywordRecord::getReplyMsg);
    public static final String ORDER_ENABLE = LambdaUtils.getFiledName(KeywordRecord::isEnable);

    // endregion

    // region 构造函数

    public KeywordRecordQueryParam() {
        super(false);
    }

    /**
     * @param all 是否查询所有字段
     */
    public KeywordRecordQueryParam(boolean all) {
        super(all);
    }

    public KeywordRecordQueryParam(long startIndex, long pageSize) {
        super(startIndex, pageSize);
    }

    public KeywordRecordQueryParam(boolean all, long startIndex, long pageSize) {
        super(all, startIndex, pageSize);
    }

    public KeywordRecordQueryParam(KeywordRecord data) {
        if (data != null) {
            setId(data.getId());
            setBotId(data.getBotId());
            setGroupId(data.getGroupId());
            setCreatorId(data.getCreatorId());
            setTime(data.getTime());
            setKeyword(data.getKeyword());
            setReplyMsg(data.getReplyMsg());
        }
    }

    // endregion

    public KeywordRecordQueryParam setId(Long id) {
        addParam(QUERY_ID, id);
        return this;
    }

    public KeywordRecordQueryParam setId(Long... ids) {
        addParamByArray(QUERY_ID, ids);
        return this;
    }

    public KeywordRecordQueryParam setId(Collection<Long> ids) {
        addParamByList(QUERY_ID, ids);
        return this;
    }

    public KeywordRecordQueryParam setIdByLt(Long max) {
        addParamByLt(QUERY_ID, max);
        return this;
    }

    public KeywordRecordQueryParam setIdByGt(Long min) {
        addParamByGt(QUERY_ID, min);
        return this;
    }

    public KeywordRecordQueryParam setIdByRange(Long min, Long max) {
        addParamByRangeOpen(QUERY_ID, min, max);
        return this;
    }

    public KeywordRecordQueryParam setBotId(Long botId) {
        addParam(QUERY_BOT_ID, botId);
        return this;
    }

    public KeywordRecordQueryParam setBotId(Long... botIds) {
        addParamByArray(QUERY_BOT_ID, botIds);
        return this;
    }

    public KeywordRecordQueryParam setBotIdByLt(Long max) {
        addParamByLt(QUERY_BOT_ID, max);
        return this;
    }

    public KeywordRecordQueryParam setBotIdByGt(Long min) {
        addParamByGt(QUERY_BOT_ID, min);
        return this;
    }

    public KeywordRecordQueryParam setBotIdByRange(Long min, Long max) {
        addParamByRangeOpen(QUERY_BOT_ID, min, max);
        return this;
    }

    public KeywordRecordQueryParam setGroupId(Long groupId) {
        addParam(QUERY_GROUP_ID, groupId);
        return this;
    }

    public KeywordRecordQueryParam setGroupId(Long... groupIds) {
        addParamByArray(QUERY_GROUP_ID, groupIds);
        return this;
    }

    public KeywordRecordQueryParam setGroupIdByLt(Long max) {
        addParamByLt(QUERY_GROUP_ID, max);
        return this;
    }

    public KeywordRecordQueryParam setGroupIdByGt(Long min) {
        addParamByGt(QUERY_GROUP_ID, min);
        return this;
    }

    public KeywordRecordQueryParam setGroupIdByRange(Long min, Long max) {
        addParamByRangeOpen(QUERY_GROUP_ID, min, max);
        return this;
    }

    public KeywordRecordQueryParam setCreatorId(Long creatorId) {
        addParam(QUERY_CREATOR_ID, creatorId);
        return this;
    }

    public KeywordRecordQueryParam setCreatorId(Long... creatorIds) {
        addParamByArray(QUERY_CREATOR_ID, creatorIds);
        return this;
    }

    public KeywordRecordQueryParam setCreatorIdByLt(Long max) {
        addParamByLt(QUERY_CREATOR_ID, max);
        return this;
    }

    public KeywordRecordQueryParam setCreatorIdByGt(Long min) {
        addParamByGt(QUERY_CREATOR_ID, min);
        return this;
    }

    public KeywordRecordQueryParam setCreatorIdByRange(Long min, Long max) {
        addParamByRangeOpen(QUERY_CREATOR_ID, min, max);
        return this;
    }

    public KeywordRecordQueryParam setTime(Long time) {
        addParam(QUERY_TIME, time);
        return this;
    }

    public KeywordRecordQueryParam setTime(Long... times) {
        addParamByArray(QUERY_TIME, times);
        return this;
    }

    public KeywordRecordQueryParam setTimeByLt(Long max) {
        addParamByLt(QUERY_TIME, max);
        return this;
    }

    public KeywordRecordQueryParam setTimeByGt(Long min) {
        addParamByGt(QUERY_TIME, min);
        return this;
    }

    public KeywordRecordQueryParam setTimeByRange(Long min, Long max) {
        addParamByRangeOpen(QUERY_TIME, min, max);
        return this;
    }

    public KeywordRecordQueryParam setKeyword(String keyword) {
        addParam(QUERY_KEYWORD, keyword);
        return this;
    }

    public KeywordRecordQueryParam setKeywordAllowEmpty(String keyword) {
        put(QUERY_KEYWORD, keyword);
        return this;
    }

    public KeywordRecordQueryParam setKeyword(String... keywords) {
        addParamByArray(QUERY_KEYWORD, keywords);
        return this;
    }

    public KeywordRecordQueryParam setKeywordType(String keywordType) {
        addParam(QUERY_KEYWORD_TYPE, keywordType);
        return this;
    }

    public KeywordRecordQueryParam setKeywordType(String... keywordTypes) {
        addParamByArray(QUERY_KEYWORD_TYPE, keywordTypes);
        return this;
    }

    public KeywordRecordQueryParam setReplyMsg(String replyMsg) {
        addParam(QUERY_REPLY_MSG, replyMsg);
        return this;
    }

    public KeywordRecordQueryParam setReplyMsgAllowEmpty(String replyMsg) {
        put(QUERY_REPLY_MSG, replyMsg);
        return this;
    }

    public KeywordRecordQueryParam setReplyMsg(String... replyMsgs) {
        addParamByArray(QUERY_REPLY_MSG, replyMsgs);
        return this;
    }

    public KeywordRecordQueryParam setEnable(Boolean enable) {
        addParam(QUERY_ENABLE, enable);
        return this;
    }

    public KeywordRecordQueryParam setParamByKeyWord(String keyWord) {
        addParam(QUERY_KEY_WORD, keyWord);
        return this;
    }

}
