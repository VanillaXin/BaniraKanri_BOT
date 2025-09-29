package xin.vanilla.banira.mapper.param;


import xin.vanilla.banira.domain.WifeRecord;
import xin.vanilla.banira.mapper.common.BaniraQueryParam;
import xin.vanilla.banira.util.lambda.LambdaUtils;

import java.util.Collection;

/**
 * WifeRecord 查询参数类
 */
@SuppressWarnings("unused")
public class WifeRecordQueryParam extends BaniraQueryParam {

    // region 查询常量

    public static final String QUERY_ID = LambdaUtils.getFiledName(WifeRecord::getId);
    public static final String QUERY_MSG_ID = LambdaUtils.getFiledName(WifeRecord::getMsgId);
    public static final String QUERY_GROUP_ID = LambdaUtils.getFiledName(WifeRecord::getGroupId);
    public static final String QUERY_SENDER_ID = LambdaUtils.getFiledName(WifeRecord::getSenderId);
    public static final String QUERY_TIME = LambdaUtils.getFiledName(WifeRecord::getTime);
    public static final String QUERY_WIFE_ID = LambdaUtils.getFiledName(WifeRecord::getWifeId);
    public static final String QUERY_WIFE_NAME = LambdaUtils.getFiledName(WifeRecord::getWifeName);
    public static final String QUERY_WIFE_NICK = LambdaUtils.getFiledName(WifeRecord::getWifeNick);

    // endregion

    // region 排序常量

    public static final String ORDER_ID = LambdaUtils.getFiledName(WifeRecord::getId);
    public static final String ORDER_MSG_ID = LambdaUtils.getFiledName(WifeRecord::getMsgId);
    public static final String ORDER_GROUP_ID = LambdaUtils.getFiledName(WifeRecord::getGroupId);
    public static final String ORDER_SENDER_ID = LambdaUtils.getFiledName(WifeRecord::getSenderId);
    public static final String ORDER_TIME = LambdaUtils.getFiledName(WifeRecord::getTime);
    public static final String ORDER_WIFE_ID = LambdaUtils.getFiledName(WifeRecord::getWifeId);
    public static final String ORDER_WIFE_NAME = LambdaUtils.getFiledName(WifeRecord::getWifeName);
    public static final String ORDER_WIFE_NICK = LambdaUtils.getFiledName(WifeRecord::getWifeNick);

    // endregion

    // region 构造函数

    public WifeRecordQueryParam() {
        super(false);
    }

    /**
     * @param all 是否查询所有字段
     */
    public WifeRecordQueryParam(boolean all) {
        super(all);
    }

    public WifeRecordQueryParam(long page, long pageSize) {
        super(page, pageSize);
    }

    public WifeRecordQueryParam(boolean all, long page, long pageSize) {
        super(all, page, pageSize);
    }

    public WifeRecordQueryParam(WifeRecord data) {
        if (data != null) {
            setId(data.getId());
            setMsgId(data.getMsgId());
            setGroupId(data.getGroupId());
            setSenderId(data.getSenderId());
            setTime(data.getTime());
            setWifeId(data.getWifeId());
            setWifeName(data.getWifeName());
            setWifeNick(data.getWifeNick());
        }
    }

    // endregion

    public WifeRecordQueryParam setId(Long id) {
        addParam(QUERY_ID, id);
        return this;
    }

    public WifeRecordQueryParam setId(Long... ids) {
        addParamByArray(QUERY_ID, ids);
        return this;
    }

    public WifeRecordQueryParam setId(Collection<Long> ids) {
        addParamByList(QUERY_ID, ids);
        return this;
    }

    public WifeRecordQueryParam setIdByLt(Long max) {
        addParamByLt(QUERY_ID, max);
        return this;
    }

    public WifeRecordQueryParam setIdByGt(Long min) {
        addParamByGt(QUERY_ID, min);
        return this;
    }

    public WifeRecordQueryParam setIdByRange(Long min, Long max) {
        addParamByRangeOpen(QUERY_ID, min, max);
        return this;
    }

    public WifeRecordQueryParam setMsgId(String msgId) {
        addParam(QUERY_MSG_ID, msgId);
        return this;
    }

    public WifeRecordQueryParam seMsgIdAllowEmpty(String msgId) {
        addParamAllowEmpty(QUERY_MSG_ID, msgId);
        return this;
    }

    public WifeRecordQueryParam setMsgId(String... msgIds) {
        addParamByArray(QUERY_MSG_ID, msgIds);
        return this;
    }

    public WifeRecordQueryParam setGroupId(Long groupId) {
        addParam(QUERY_GROUP_ID, groupId);
        return this;
    }

    public WifeRecordQueryParam setGroupId(Long... groupIds) {
        addParamByArray(QUERY_GROUP_ID, groupIds);
        return this;
    }

    public WifeRecordQueryParam setGroupIdByLt(Long max) {
        addParamByLt(QUERY_GROUP_ID, max);
        return this;
    }

    public WifeRecordQueryParam setGroupIdByGt(Long min) {
        addParamByGt(QUERY_GROUP_ID, min);
        return this;
    }

    public WifeRecordQueryParam setGroupIdByRange(Long min, Long max) {
        addParamByRangeOpen(QUERY_GROUP_ID, min, max);
        return this;
    }

    public WifeRecordQueryParam setSenderId(Long senderId) {
        addParam(QUERY_SENDER_ID, senderId);
        return this;
    }

    public WifeRecordQueryParam setSenderId(Long... senderIds) {
        addParamByArray(QUERY_SENDER_ID, senderIds);
        return this;
    }

    public WifeRecordQueryParam setSenderIdByLt(Long max) {
        addParamByLt(QUERY_SENDER_ID, max);
        return this;
    }

    public WifeRecordQueryParam setSenderIdByGt(Long min) {
        addParamByGt(QUERY_SENDER_ID, min);
        return this;
    }

    public WifeRecordQueryParam setSenderIdByRange(Long min, Long max) {
        addParamByRangeOpen(QUERY_SENDER_ID, min, max);
        return this;
    }

    public WifeRecordQueryParam setTime(Long time) {
        addParam(QUERY_TIME, time);
        return this;
    }

    public WifeRecordQueryParam setTime(Long... times) {
        addParamByArray(QUERY_TIME, times);
        return this;
    }

    public WifeRecordQueryParam setTimeByLt(Long max) {
        addParamByLt(QUERY_TIME, max);
        return this;
    }

    public WifeRecordQueryParam setTimeByGt(Long min) {
        addParamByGt(QUERY_TIME, min);
        return this;
    }

    public WifeRecordQueryParam setTimeByRange(Long min, Long max) {
        addParamByRangeOpen(QUERY_TIME, min, max);
        return this;
    }

    public WifeRecordQueryParam setWifeId(Long wifeId) {
        addParam(QUERY_WIFE_ID, wifeId);
        return this;
    }

    public WifeRecordQueryParam setWifeId(Long... wifeIds) {
        addParamByArray(QUERY_WIFE_ID, wifeIds);
        return this;
    }

    public WifeRecordQueryParam setWifeIdByLt(Long max) {
        addParamByLt(QUERY_WIFE_ID, max);
        return this;
    }

    public WifeRecordQueryParam setWifeIdByGt(Long min) {
        addParamByGt(QUERY_WIFE_ID, min);
        return this;
    }

    public WifeRecordQueryParam setWifeIdByRange(Long min, Long max) {
        addParamByRangeOpen(QUERY_WIFE_ID, min, max);
        return this;
    }

    public WifeRecordQueryParam setWifeName(String wifeName) {
        addParam(QUERY_WIFE_NAME, wifeName);
        return this;
    }

    public WifeRecordQueryParam setWifeNameAllowEmpty(String wifeName) {
        put(QUERY_WIFE_NAME, wifeName);
        return this;
    }

    public WifeRecordQueryParam setWifeName(String... wifeNames) {
        addParamByArray(QUERY_WIFE_NAME, wifeNames);
        return this;
    }

    public WifeRecordQueryParam setWifeNick(String wifeNick) {
        addParam(QUERY_WIFE_NICK, wifeNick);
        return this;
    }

    public WifeRecordQueryParam setWifeNickAllowEmpty(String wifeNick) {
        put(QUERY_WIFE_NICK, wifeNick);
        return this;
    }

    public WifeRecordQueryParam setWifeNick(String... wifeNicks) {
        addParamByArray(QUERY_WIFE_NICK, wifeNicks);
        return this;
    }

    public WifeRecordQueryParam addKeyWord(String keyWord) {
        addParam(QUERY_KEY_WORD, keyWord);
        return this;
    }

}
