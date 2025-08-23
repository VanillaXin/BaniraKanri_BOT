package xin.vanilla.banira.mapper.param;


import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.mapper.common.BaniraQueryParam;
import xin.vanilla.banira.util.lambda.LambdaUtils;

import java.util.Arrays;
import java.util.Collection;

/**
 * MessageRecord 查询参数类
 */
@SuppressWarnings("unused")
public class MessageRecordQueryParam extends BaniraQueryParam {

    // region 查询常量

    public static final String QUERY_ID = LambdaUtils.getFiledName(MessageRecord::getId);
    public static final String QUERY_MSG_ID = LambdaUtils.getFiledName(MessageRecord::getMsgId);
    public static final String QUERY_BOT_ID = LambdaUtils.getFiledName(MessageRecord::getBotId);
    public static final String QUERY_SENDER_ID = LambdaUtils.getFiledName(MessageRecord::getSenderId);
    public static final String QUERY_TARGET_ID = LambdaUtils.getFiledName(MessageRecord::getTargetId);
    public static final String QUERY_GROUP_ID = LambdaUtils.getFiledName(MessageRecord::getGroupId);
    public static final String QUERY_TIME = LambdaUtils.getFiledName(MessageRecord::getTime);
    public static final String QUERY_MSG_RAW = LambdaUtils.getFiledName(MessageRecord::getMsgRaw);
    public static final String QUERY_MSG_TYPE = LambdaUtils.getFiledName(MessageRecord::getMsgType);
    public static final String QUERY_MSG_RECODE = LambdaUtils.getFiledName(MessageRecord::getMsgRecode);

    // endregion

    // region 排序常量

    public static final String ORDER_ID = LambdaUtils.getFiledName(MessageRecord::getId);
    public static final String ORDER_MSG_ID = LambdaUtils.getFiledName(MessageRecord::getMsgId);
    public static final String ORDER_BOT_ID = LambdaUtils.getFiledName(MessageRecord::getBotId);
    public static final String ORDER_SENDER_ID = LambdaUtils.getFiledName(MessageRecord::getSenderId);
    public static final String ORDER_TARGET_ID = LambdaUtils.getFiledName(MessageRecord::getTargetId);
    public static final String ORDER_GROUP_ID = LambdaUtils.getFiledName(MessageRecord::getGroupId);
    public static final String ORDER_TIME = LambdaUtils.getFiledName(MessageRecord::getTime);
    public static final String ORDER_MSG_RAW = LambdaUtils.getFiledName(MessageRecord::getMsgRaw);
    public static final String ORDER_MSG_TYPE = LambdaUtils.getFiledName(MessageRecord::getMsgType);
    public static final String ORDER_MSG_RECODE = LambdaUtils.getFiledName(MessageRecord::getMsgRecode);

    // endregion

    // region 构造函数

    public MessageRecordQueryParam() {
        super(false);
    }

    /**
     * @param all 是否查询所有字段
     */
    public MessageRecordQueryParam(boolean all) {
        super(all);
    }

    public MessageRecordQueryParam(long startIndex, long pageSize) {
        super(startIndex, pageSize);
    }

    public MessageRecordQueryParam(boolean all, long startIndex, long pageSize) {
        super(all, startIndex, pageSize);
    }

    public MessageRecordQueryParam(MessageRecord data) {
        if (data != null) {
            setId(data.getId());
            setMsgId(data.getMsgId());
            setBotId(data.getBotId());
            setSenderId(data.getSenderId());
            setTargetId(data.getTargetId());
            setGroupId(data.getGroupId());
            setTime(data.getTime());
            setMsgRecode(data.getMsgRecode());
            setMsgRaw(data.getMsgRaw());
        }
    }

    // endregion

    public MessageRecordQueryParam setId(Long id) {
        addParam(QUERY_ID, id);
        return this;
    }

    public MessageRecordQueryParam setId(Long... ids) {
        addParamByArray(QUERY_ID, ids);
        return this;
    }

    public MessageRecordQueryParam setId(Collection<Long> ids) {
        addParamByList(QUERY_ID, ids);
        return this;
    }

    public MessageRecordQueryParam setIdByLt(Long max) {
        addParamByLt(QUERY_ID, max);
        return this;
    }

    public MessageRecordQueryParam setIdByGt(Long min) {
        addParamByGt(QUERY_ID, min);
        return this;
    }

    public MessageRecordQueryParam setIdByRange(Long min, Long max) {
        addParamByRangeOpen(QUERY_ID, min, max);
        return this;
    }

    public MessageRecordQueryParam setMsgId(Number msgId) {
        addParam(QUERY_MSG_ID, String.valueOf(msgId.longValue()));
        return this;
    }

    public MessageRecordQueryParam setMsgId(String msgId) {
        addParam(QUERY_MSG_ID, msgId);
        return this;
    }

    public MessageRecordQueryParam seMsgIdAllowEmpty(String msgId) {
        addParamAllowEmpty(QUERY_MSG_ID, msgId);
        return this;
    }

    public MessageRecordQueryParam setMsgId(String... msgIds) {
        addParamByArray(QUERY_MSG_ID, msgIds);
        return this;
    }

    public MessageRecordQueryParam setMsgId(Long... msgIds) {
        addParamByArray(QUERY_MSG_ID, Arrays.stream(msgIds).map(String::valueOf).toArray(String[]::new));
        return this;
    }

    public MessageRecordQueryParam setBotId(Long botId) {
        addParam(QUERY_BOT_ID, botId);
        return this;
    }

    public MessageRecordQueryParam setBotId(Long... botIds) {
        addParamByArray(QUERY_BOT_ID, botIds);
        return this;
    }

    public MessageRecordQueryParam setBotIdByLt(Long max) {
        addParamByLt(QUERY_BOT_ID, max);
        return this;
    }

    public MessageRecordQueryParam setBotIdByGt(Long min) {
        addParamByGt(QUERY_BOT_ID, min);
        return this;
    }

    public MessageRecordQueryParam setBotIdByRange(Long min, Long max) {
        addParamByRangeOpen(QUERY_BOT_ID, min, max);
        return this;
    }

    public MessageRecordQueryParam setSenderId(Long senderId) {
        addParam(QUERY_SENDER_ID, senderId);
        return this;
    }

    public MessageRecordQueryParam setSenderId(Long... senderIds) {
        addParamByArray(QUERY_SENDER_ID, senderIds);
        return this;
    }

    public MessageRecordQueryParam setSenderIdByLt(Long max) {
        addParamByLt(QUERY_SENDER_ID, max);
        return this;
    }

    public MessageRecordQueryParam setSenderIdByGt(Long min) {
        addParamByGt(QUERY_SENDER_ID, min);
        return this;
    }

    public MessageRecordQueryParam setSenderIdByRange(Long min, Long max) {
        addParamByRangeOpen(QUERY_SENDER_ID, min, max);
        return this;
    }

    public MessageRecordQueryParam setTargetId(Long targetId) {
        addParam(QUERY_TARGET_ID, targetId);
        return this;
    }

    public MessageRecordQueryParam setTargetId(Long... targetIds) {
        addParamByArray(QUERY_TARGET_ID, targetIds);
        return this;
    }

    public MessageRecordQueryParam setTargetIdByLt(Long max) {
        addParamByLt(QUERY_TARGET_ID, max);
        return this;
    }

    public MessageRecordQueryParam setTargetIdByGt(Long min) {
        addParamByGt(QUERY_TARGET_ID, min);
        return this;
    }

    public MessageRecordQueryParam setTargetIdByRange(Long min, Long max) {
        addParamByRangeOpen(QUERY_TARGET_ID, min, max);
        return this;
    }

    public MessageRecordQueryParam setGroupId(Long groupId) {
        addParam(QUERY_GROUP_ID, groupId);
        return this;
    }

    public MessageRecordQueryParam setGroupId(Long... groupIds) {
        addParamByArray(QUERY_GROUP_ID, groupIds);
        return this;
    }

    public MessageRecordQueryParam setGroupIdByLt(Long max) {
        addParamByLt(QUERY_GROUP_ID, max);
        return this;
    }

    public MessageRecordQueryParam setGroupIdByGt(Long min) {
        addParamByGt(QUERY_GROUP_ID, min);
        return this;
    }

    public MessageRecordQueryParam setGroupIdByRange(Long min, Long max) {
        addParamByRangeOpen(QUERY_GROUP_ID, min, max);
        return this;
    }

    public MessageRecordQueryParam setTime(Long time) {
        addParam(QUERY_TIME, time);
        return this;
    }

    public MessageRecordQueryParam setTime(Long... times) {
        addParamByArray(QUERY_TIME, times);
        return this;
    }

    public MessageRecordQueryParam setTimeByLt(Long max) {
        addParamByLt(QUERY_TIME, max);
        return this;
    }

    public MessageRecordQueryParam setTimeByGt(Long min) {
        addParamByGt(QUERY_TIME, min);
        return this;
    }

    public MessageRecordQueryParam setTimeByRange(Long min, Long max) {
        addParamByRangeOpen(QUERY_TIME, min, max);
        return this;
    }

    public MessageRecordQueryParam setMsgRaw(String msgRaw) {
        addParam(QUERY_MSG_RAW, msgRaw);
        return this;
    }

    public MessageRecordQueryParam setMsgRawAllowEmpty(String msgRaw) {
        put(QUERY_MSG_RAW, msgRaw);
        return this;
    }

    public MessageRecordQueryParam setMsgRaw(String... msgRaws) {
        addParamByArray(QUERY_MSG_RAW, msgRaws);
        return this;
    }

    public MessageRecordQueryParam setMsgType(String msgType) {
        addParam(QUERY_MSG_TYPE, msgType);
        return this;
    }

    public MessageRecordQueryParam setMsgType(String... msgTypes) {
        addParamByArray(QUERY_MSG_TYPE, msgTypes);
        return this;
    }

    public MessageRecordQueryParam setMsgRecode(String msgRecode) {
        addParam(QUERY_MSG_RECODE, msgRecode);
        return this;
    }

    public MessageRecordQueryParam setMsgRecodeAllowEmpty(String msgRecode) {
        put(QUERY_MSG_RECODE, msgRecode);
        return this;
    }

    public MessageRecordQueryParam setMsgRecode(String... msgRecodes) {
        addParamByArray(QUERY_MSG_RECODE, msgRecodes);
        return this;
    }

    public MessageRecordQueryParam addKeyWord(String keyWord) {
        addParam(QUERY_KEY_WORD, keyWord);
        return this;
    }

}
