package xin.vanilla.banira.mapper.param;


import xin.vanilla.banira.domain.TimerRecord;
import xin.vanilla.banira.mapper.common.BaniraQueryParam;
import xin.vanilla.banira.util.lambda.LambdaUtils;

import java.util.Collection;

/**
 * TimerRecord 查询参数类
 */
@SuppressWarnings("unused")
public class TimerRecordQueryParam extends BaniraQueryParam {

    // region 查询常量

    public static final String QUERY_ID = LambdaUtils.getFiledName(TimerRecord::getId);
    public static final String QUERY_BOT_ID = LambdaUtils.getFiledName(TimerRecord::getBotId);
    public static final String QUERY_GROUP_ID = LambdaUtils.getFiledName(TimerRecord::getGroupId);
    public static final String QUERY_CREATOR_ID = LambdaUtils.getFiledName(TimerRecord::getCreatorId);
    public static final String QUERY_TIME = LambdaUtils.getFiledName(TimerRecord::getTime);
    public static final String QUERY_CRON = LambdaUtils.getFiledName(TimerRecord::getCron);
    public static final String QUERY_REPLY_MSG = LambdaUtils.getFiledName(TimerRecord::getReplyMsg);
    public static final String QUERY_ENABLE = LambdaUtils.getFiledName(TimerRecord::getEnable);

    // endregion

    // region 排序常量

    public static final String ORDER_ID = LambdaUtils.getFiledName(TimerRecord::getId);
    public static final String ORDER_BOT_ID = LambdaUtils.getFiledName(TimerRecord::getBotId);
    public static final String ORDER_GROUP_ID = LambdaUtils.getFiledName(TimerRecord::getGroupId);
    public static final String ORDER_CREATOR_ID = LambdaUtils.getFiledName(TimerRecord::getCreatorId);
    public static final String ORDER_TIME = LambdaUtils.getFiledName(TimerRecord::getTime);
    public static final String ORDER_CRON = LambdaUtils.getFiledName(TimerRecord::getCron);
    public static final String ORDER_REPLY_MSG = LambdaUtils.getFiledName(TimerRecord::getReplyMsg);
    public static final String ORDER_ENABLE = LambdaUtils.getFiledName(TimerRecord::getEnable);

    // endregion

    // region 构造函数

    public TimerRecordQueryParam() {
        super(false);
    }

    /**
     * @param all 是否查询所有字段
     */
    public TimerRecordQueryParam(boolean all) {
        super(all);
    }

    public TimerRecordQueryParam(long startIndex, long pageSize) {
        super(startIndex, pageSize);
    }

    public TimerRecordQueryParam(boolean all, long startIndex, long pageSize) {
        super(all, startIndex, pageSize);
    }

    public TimerRecordQueryParam(TimerRecord data) {
        if (data != null) {
            setId(data.getId());
            setBotId(data.getBotId());
            setGroupId(data.getGroupId());
            setCreatorId(data.getCreatorId());
            setTime(data.getTime());
            setCron(data.getCron());
            setReplyMsg(data.getReplyMsg());
        }
    }

    // endregion

    public TimerRecordQueryParam setId(Long id) {
        addParam(QUERY_ID, id);
        return this;
    }

    public TimerRecordQueryParam setId(Long... ids) {
        addParamByArray(QUERY_ID, ids);
        return this;
    }

    public TimerRecordQueryParam setId(Collection<Long> ids) {
        addParamByList(QUERY_ID, ids);
        return this;
    }

    public TimerRecordQueryParam setIdByLt(Long max) {
        addParamByLt(QUERY_ID, max);
        return this;
    }

    public TimerRecordQueryParam setIdByGt(Long min) {
        addParamByGt(QUERY_ID, min);
        return this;
    }

    public TimerRecordQueryParam setIdByRange(Long min, Long max) {
        addParamByRangeOpen(QUERY_ID, min, max);
        return this;
    }

    public TimerRecordQueryParam setBotId(Long botId) {
        addParam(QUERY_BOT_ID, botId);
        return this;
    }

    public TimerRecordQueryParam setBotId(Long... botIds) {
        addParamByArray(QUERY_BOT_ID, botIds);
        return this;
    }

    public TimerRecordQueryParam setBotIdByLt(Long max) {
        addParamByLt(QUERY_BOT_ID, max);
        return this;
    }

    public TimerRecordQueryParam setBotIdByGt(Long min) {
        addParamByGt(QUERY_BOT_ID, min);
        return this;
    }

    public TimerRecordQueryParam setBotIdByRange(Long min, Long max) {
        addParamByRangeOpen(QUERY_BOT_ID, min, max);
        return this;
    }

    public TimerRecordQueryParam setGroupId(Long groupId) {
        addParam(QUERY_GROUP_ID, groupId);
        return this;
    }

    public TimerRecordQueryParam setGroupId(Long... groupIds) {
        addParamByArray(QUERY_GROUP_ID, groupIds);
        return this;
    }

    public TimerRecordQueryParam setGroupIdByLt(Long max) {
        addParamByLt(QUERY_GROUP_ID, max);
        return this;
    }

    public TimerRecordQueryParam setGroupIdByGt(Long min) {
        addParamByGt(QUERY_GROUP_ID, min);
        return this;
    }

    public TimerRecordQueryParam setGroupIdByRange(Long min, Long max) {
        addParamByRangeOpen(QUERY_GROUP_ID, min, max);
        return this;
    }

    public TimerRecordQueryParam setCreatorId(Long creatorId) {
        addParam(QUERY_CREATOR_ID, creatorId);
        return this;
    }

    public TimerRecordQueryParam setCreatorId(Long... creatorIds) {
        addParamByArray(QUERY_CREATOR_ID, creatorIds);
        return this;
    }

    public TimerRecordQueryParam setCreatorIdByLt(Long max) {
        addParamByLt(QUERY_CREATOR_ID, max);
        return this;
    }

    public TimerRecordQueryParam setCreatorIdByGt(Long min) {
        addParamByGt(QUERY_CREATOR_ID, min);
        return this;
    }

    public TimerRecordQueryParam setCreatorIdByRange(Long min, Long max) {
        addParamByRangeOpen(QUERY_CREATOR_ID, min, max);
        return this;
    }

    public TimerRecordQueryParam setTime(Long time) {
        addParam(QUERY_TIME, time);
        return this;
    }

    public TimerRecordQueryParam setTime(Long... times) {
        addParamByArray(QUERY_TIME, times);
        return this;
    }

    public TimerRecordQueryParam setTimeByLt(Long max) {
        addParamByLt(QUERY_TIME, max);
        return this;
    }

    public TimerRecordQueryParam setTimeByGt(Long min) {
        addParamByGt(QUERY_TIME, min);
        return this;
    }

    public TimerRecordQueryParam setTimeByRange(Long min, Long max) {
        addParamByRangeOpen(QUERY_TIME, min, max);
        return this;
    }

    public TimerRecordQueryParam setCron(String cron) {
        addParam(QUERY_CRON, cron);
        return this;
    }

    public TimerRecordQueryParam setCronAllowEmpty(String cron) {
        put(QUERY_CRON, cron);
        return this;
    }

    public TimerRecordQueryParam setCron(String... crons) {
        addParamByArray(QUERY_CRON, crons);
        return this;
    }

    public TimerRecordQueryParam setReplyMsg(String replyMsg) {
        addParam(QUERY_REPLY_MSG, replyMsg);
        return this;
    }

    public TimerRecordQueryParam setReplyMsgAllowEmpty(String replyMsg) {
        put(QUERY_REPLY_MSG, replyMsg);
        return this;
    }

    public TimerRecordQueryParam setReplyMsg(String... replyMsgs) {
        addParamByArray(QUERY_REPLY_MSG, replyMsgs);
        return this;
    }

    public TimerRecordQueryParam setEnable(Boolean enable) {
        addParam(QUERY_ENABLE, enable);
        return this;
    }

    public TimerRecordQueryParam setParamByTimer(String timer) {
        addParam(QUERY_KEY_WORD, timer);
        return this;
    }

    public TimerRecordQueryParam addKeyWord(String keyWord) {
        addParam(QUERY_KEY_WORD, keyWord);
        return this;
    }

    @Override
    public TimerRecordQueryParam addOrderBy(String name, boolean asc) {
        super.addOrderBy(name, asc);
        return this;
    }
}
