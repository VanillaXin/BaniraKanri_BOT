package xin.vanilla.banira.mapper.param;


import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.util.lambda.LambdaUtils;

import java.util.Collection;

/**
 * MessageRecord 查询参数类
 */
@SuppressWarnings("unused")
public class MessageRecordQueryParam extends BaniraQueryParam {

    // region 查询常量

    public static final String QUERY_ATTR_ID = LambdaUtils.getFiledName(MessageRecord::getId);
    public static final String QUERY_ATTR_NOS = LambdaUtils.getFiledName(MessageRecord::getNos);
    public static final String QUERY_ATTR_BOT = LambdaUtils.getFiledName(MessageRecord::getBot);
    public static final String QUERY_ATTR_SENDER = LambdaUtils.getFiledName(MessageRecord::getSender);
    public static final String QUERY_ATTR_TARGET = LambdaUtils.getFiledName(MessageRecord::getTarget);
    public static final String QUERY_ATTR_TIME = LambdaUtils.getFiledName(MessageRecord::getTime);
    public static final String QUERY_ATTR_MSG_RAW = LambdaUtils.getFiledName(MessageRecord::getMsgRaw);
    public static final String QUERY_ATTR_MSG_TYPE = LambdaUtils.getFiledName(MessageRecord::getMsgType);
    public static final String QUERY_ATTR_MSG_RECODE = LambdaUtils.getFiledName(MessageRecord::getMsgRecode);

    // endregion

    // region 排序常量

    public static final String ORDER_ATTR_ID = LambdaUtils.getFiledName(MessageRecord::getId);
    public static final String ORDER_ATTR_NOS = LambdaUtils.getFiledName(MessageRecord::getNos);
    public static final String ORDER_ATTR_BOT = LambdaUtils.getFiledName(MessageRecord::getBot);
    public static final String ORDER_ATTR_SENDER = LambdaUtils.getFiledName(MessageRecord::getSender);
    public static final String ORDER_ATTR_TARGET = LambdaUtils.getFiledName(MessageRecord::getTarget);
    public static final String ORDER_ATTR_TIME = LambdaUtils.getFiledName(MessageRecord::getTime);
    public static final String ORDER_ATTR_MSG_RAW = LambdaUtils.getFiledName(MessageRecord::getMsgRaw);
    public static final String ORDER_ATTR_MSG_TYPE = LambdaUtils.getFiledName(MessageRecord::getMsgType);
    public static final String ORDER_ATTR_MSG_RECODE = LambdaUtils.getFiledName(MessageRecord::getMsgRecode);

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

    public MessageRecordQueryParam(MessageRecord data) {
        if (data != null) {
            setId(data.getId());
            setNos(data.getNos());
            setBot(data.getBot());
            setSender(data.getSender());
            setTarget(data.getTarget());
            setTime(data.getTime());
            setMsgRecode(data.getMsgRecode());
            setMsgRaw(data.getMsgRaw());
        }
    }

    // endregion

    public MessageRecordQueryParam setId(Long id) {
        addParam(QUERY_ATTR_ID, id);
        return this;
    }

    public MessageRecordQueryParam setId(Long... ids) {
        addParamByArray(QUERY_ATTR_ID, ids);
        return this;
    }

    public MessageRecordQueryParam setId(Collection<Long> ids) {
        addParamByList(QUERY_ATTR_ID, ids);
        return this;
    }

    public MessageRecordQueryParam setIdLt(Long max) {
        addParamByLt(QUERY_ATTR_ID, max);
        return this;
    }

    public MessageRecordQueryParam setIdByGt(Long min) {
        addParamByGt(QUERY_ATTR_ID, min);
        return this;
    }

    public MessageRecordQueryParam setIdByRange(Long min, Long max) {
        addParamByRangeOpen(QUERY_ATTR_ID, min, max);
        return this;
    }

    public MessageRecordQueryParam setNos(String nos) {
        addParam(QUERY_ATTR_NOS, nos);
        return this;
    }

    public MessageRecordQueryParam seNosAllowEmpty(String nos) {
        addParamAllowEmpty(QUERY_ATTR_NOS, nos);
        return this;
    }

    public MessageRecordQueryParam setNos(String... noss) {
        addParamByArray(QUERY_ATTR_NOS, noss);
        return this;
    }

    public MessageRecordQueryParam setBot(Long bot) {
        addParam(QUERY_ATTR_BOT, bot);
        return this;
    }

    public MessageRecordQueryParam setBot(Long... bots) {
        addParamByArray(QUERY_ATTR_BOT, bots);
        return this;
    }

    public MessageRecordQueryParam setBotByLt(Long max) {
        addParamByLt(QUERY_ATTR_BOT, max);
        return this;
    }

    public MessageRecordQueryParam setBotByGt(Long min) {
        addParamByGt(QUERY_ATTR_BOT, min);
        return this;
    }

    public MessageRecordQueryParam setBotByRange(Long min, Long max) {
        addParamByRangeOpen(QUERY_ATTR_BOT, min, max);
        return this;
    }

    public MessageRecordQueryParam setSender(Long sender) {
        addParam(QUERY_ATTR_SENDER, sender);
        return this;
    }

    public MessageRecordQueryParam setSender(Long... senders) {
        addParamByArray(QUERY_ATTR_SENDER, senders);
        return this;
    }

    public MessageRecordQueryParam setSenderByLt(Long max) {
        addParamByLt(QUERY_ATTR_SENDER, max);
        return this;
    }

    public MessageRecordQueryParam setSenderByGt(Long min) {
        addParamByGt(QUERY_ATTR_SENDER, min);
        return this;
    }

    public MessageRecordQueryParam setSenderByRange(Long min, Long max) {
        addParamByRangeOpen(QUERY_ATTR_SENDER, min, max);
        return this;
    }

    public MessageRecordQueryParam setTarget(Long target) {
        addParam(QUERY_ATTR_TARGET, target);
        return this;
    }

    public MessageRecordQueryParam setTarget(Long... targets) {
        addParamByArray(QUERY_ATTR_TARGET, targets);
        return this;
    }

    public MessageRecordQueryParam setTargetByLt(Long max) {
        addParamByLt(QUERY_ATTR_TARGET, max);
        return this;
    }

    public MessageRecordQueryParam setTargetByGt(Long min) {
        addParamByGt(QUERY_ATTR_TARGET, min);
        return this;
    }

    public MessageRecordQueryParam setTargetByRange(Long min, Long max) {
        addParamByRangeOpen(QUERY_ATTR_TARGET, min, max);
        return this;
    }

    public MessageRecordQueryParam setTime(Long time) {
        addParam(QUERY_ATTR_TIME, time);
        return this;
    }

    public MessageRecordQueryParam setTime(Long... times) {
        addParamByArray(QUERY_ATTR_TIME, times);
        return this;
    }

    public MessageRecordQueryParam setTimeByLt(Long max) {
        addParamByLt(QUERY_ATTR_TIME, max);
        return this;
    }

    public MessageRecordQueryParam setTimeByGt(Long min) {
        addParamByGt(QUERY_ATTR_TIME, min);
        return this;
    }

    public MessageRecordQueryParam setTimeByRange(Long min, Long max) {
        addParamByRangeOpen(QUERY_ATTR_TIME, min, max);
        return this;
    }

    public MessageRecordQueryParam setMsgRaw(String msgRaw) {
        addParam(QUERY_ATTR_MSG_RAW, msgRaw);
        return this;
    }

    public MessageRecordQueryParam setMsgRawAllowEmpty(String msgRaw) {
        put(QUERY_ATTR_MSG_RAW, msgRaw);
        return this;
    }

    public MessageRecordQueryParam setMsgRaw(String... msgRaws) {
        addParamByArray(QUERY_ATTR_MSG_RAW, msgRaws);
        return this;
    }

    public MessageRecordQueryParam setMsgType(String msgType) {
        addParam(QUERY_ATTR_MSG_TYPE, msgType);
        return this;
    }

    public MessageRecordQueryParam setMsgType(String... msgTypes) {
        addParamByArray(QUERY_ATTR_MSG_TYPE, msgTypes);
        return this;
    }

    public MessageRecordQueryParam setMsgRecode(String msgRecode) {
        addParam(QUERY_ATTR_MSG_RECODE, msgRecode);
        return this;
    }

    public MessageRecordQueryParam setMsgRecodeAllowEmpty(String msgRecode) {
        put(QUERY_ATTR_MSG_RECODE, msgRecode);
        return this;
    }

    public MessageRecordQueryParam setMsgRecode(String... msgRecodes) {
        addParamByArray(QUERY_ATTR_MSG_RECODE, msgRecodes);
        return this;
    }

    public MessageRecordQueryParam setParamByKeyWord(String keyWord) {
        addParam(QUERY_ATTR_KEY_WORD, keyWord);
        return this;
    }

}
