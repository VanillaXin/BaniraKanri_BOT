package xin.vanilla.banira.plugin.common;

import com.mikuac.shiro.common.utils.JsonUtils;
import com.mikuac.shiro.common.utils.MessageConverser;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.action.common.ActionData;
import com.mikuac.shiro.dto.action.common.ActionList;
import com.mikuac.shiro.dto.action.common.ActionRaw;
import com.mikuac.shiro.dto.action.common.MsgId;
import com.mikuac.shiro.dto.action.response.*;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import com.mikuac.shiro.model.ArrayMsg;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.enums.EnumMessageType;
import xin.vanilla.banira.enums.EnumPermission;
import xin.vanilla.banira.mapper.param.MessageRecordQueryParam;
import xin.vanilla.banira.service.IMessageRecordManager;
import xin.vanilla.banira.start.SpringContextHolder;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.*;

/**
 * Bot大包装，实现记录发送的消息并提供部分工具方法
 */
@SuppressWarnings("unused")
public class BaniraBot extends Bot {

    public BaniraBot(Bot bot) {
        super(bot.getSelfId()
                , bot.getSession()
                , bot.getActionHandler()
                , bot.getPluginList()
                , bot.getAnnotationMethodContainer()
                , bot.getBotMessageEventInterceptor()
        );
    }


    // region override

    /**
     * 发送私聊消息
     *
     * @param userId     对方 QQ 号
     * @param msg        要发送的内容
     * @param autoEscape 消息内容是否作为纯文本发送 ( 即不解析 CQ 码 ) , 只在 message 字段是字符串时有效
     * @return result {@link ActionData} of {@link MsgId}
     */
    public ActionData<MsgId> sendPrivateMsg(long userId, String msg, boolean autoEscape) {
        ActionData<MsgId> msgId;
        if (autoEscape) {
            String censoredMsg = BaniraUtils.replaceSensitiveContent(msg);
            msgId = super.sendPrivateMsg(userId, censoredMsg, autoEscape);
        } else {
            List<ArrayMsg> censoredMsg = BaniraUtils.replaceSensitiveContent(MessageConverser.stringToArray(msg));
            msgId = super.sendPrivateMsg(userId, censoredMsg, autoEscape);
        }
        if (isActionDataMsgIdNotEmpty(msgId)) {
            MessageRecord record = new MessageRecord()
                    .setMsgId(getActionDataMsgId(msgId))
                    .setBotId(super.getSelfId())
                    .setSenderId(super.getSelfId())
                    .setTargetId(userId)
                    .setTime(System.currentTimeMillis() / 1000)
                    .setMsgRaw(msg)
                    .setMsgRecode(msg)
                    .setMsgType(EnumMessageType.FRIEND);
            getMessageRecordManager().addMessageRecord(setMsgRecordTime(record));
        }
        return msgId;
    }

    /**
     * 发送私聊消息
     *
     * @param userId     对方 QQ 号
     * @param msg        消息链
     * @param autoEscape 消息内容是否作为纯文本发送 ( 即不解析 CQ 码 ) , 只在 message 字段是字符串时有效
     * @return result {@link ActionData} of {@link MsgId}
     */
    public ActionData<MsgId> sendPrivateMsg(long userId, List<ArrayMsg> msg, boolean autoEscape) {
        List<ArrayMsg> censoredMsg = BaniraUtils.replaceSensitiveContent(msg);
        ActionData<MsgId> msgId = super.sendPrivateMsg(userId, censoredMsg, autoEscape);
        if (isActionDataMsgIdNotEmpty(msgId)) {
            MessageRecord record = new MessageRecord()
                    .setMsgId(getActionDataMsgId(msgId))
                    .setBotId(super.getSelfId())
                    .setSenderId(super.getSelfId())
                    .setTargetId(userId)
                    .setTime(System.currentTimeMillis() / 1000)
                    .setMsgRaw(MessageConverser.arraysToString(msg))
                    .setMsgRecode(MessageConverser.arraysToString(msg))
                    .setMsgType(EnumMessageType.FRIEND);
            getMessageRecordManager().addMessageRecord(setMsgRecordTime(record));
        }
        return msgId;
    }

    /**
     * 临时会话
     *
     * @param groupId    主动发起临时会话群号(机器人本身必须是管理员/群主)
     * @param userId     对方 QQ 号
     * @param msg        要发送的内容
     * @param autoEscape 消息内容是否作为纯文本发送 ( 即不解析 CQ 码 ) , 只在 message 字段是字符串时有效
     * @return result {@link ActionData} of {@link MsgId}
     */
    public ActionData<MsgId> sendPrivateMsg(long groupId, long userId, String msg, boolean autoEscape) {
        ActionData<MsgId> msgId;
        if (autoEscape) {
            String censoredMsg = BaniraUtils.replaceSensitiveContent(msg);
            msgId = super.sendPrivateMsg(groupId, userId, censoredMsg, autoEscape);
        } else {
            List<ArrayMsg> censoredMsg = BaniraUtils.replaceSensitiveContent(MessageConverser.stringToArray(msg));
            msgId = super.sendPrivateMsg(groupId, userId, censoredMsg, autoEscape);
        }
        if (isActionDataMsgIdNotEmpty(msgId)) {
            MessageRecord record = new MessageRecord()
                    .setMsgId(getActionDataMsgId(msgId))
                    .setBotId(super.getSelfId())
                    .setSenderId(super.getSelfId())
                    .setGroupId(groupId)
                    .setTargetId(userId)
                    .setTime(System.currentTimeMillis() / 1000)
                    .setMsgRaw(msg)
                    .setMsgRecode(msg)
                    .setMsgType(EnumMessageType.MEMBER);
            getMessageRecordManager().addMessageRecord(setMsgRecordTime(record));
        }
        return msgId;
    }

    /**
     * 临时会话
     *
     * @param groupId    主动发起临时会话群号(机器人本身必须是管理员/群主)
     * @param userId     对方 QQ 号
     * @param msg        消息链
     * @param autoEscape 消息内容是否作为纯文本发送 ( 即不解析 CQ 码 ) , 只在 message 字段是字符串时有效
     * @return result {@link ActionData} of {@link MsgId}
     */
    public ActionData<MsgId> sendPrivateMsg(long groupId, long userId, List<ArrayMsg> msg, boolean autoEscape) {
        List<ArrayMsg> censoredMsg = BaniraUtils.replaceSensitiveContent(msg);
        ActionData<MsgId> msgId = super.sendPrivateMsg(groupId, userId, censoredMsg, autoEscape);
        if (isActionDataMsgIdNotEmpty(msgId)) {
            MessageRecord record = new MessageRecord()
                    .setMsgId(getActionDataMsgId(msgId))
                    .setBotId(super.getSelfId())
                    .setSenderId(super.getSelfId())
                    .setGroupId(groupId)
                    .setTargetId(userId)
                    .setTime(System.currentTimeMillis() / 1000)
                    .setMsgRaw(MessageConverser.arraysToString(msg))
                    .setMsgRecode(MessageConverser.arraysToString(msg))
                    .setMsgType(EnumMessageType.MEMBER);
            getMessageRecordManager().addMessageRecord(setMsgRecordTime(record));
        }
        return msgId;
    }

    /**
     * 发送群消息
     *
     * @param groupId    群号
     * @param msg        要发送的内容
     * @param autoEscape 消息内容是否作为纯文本发送 ( 即不解析 CQ 码 ) , 只在 message 字段是字符串时有效
     * @return result {@link ActionData} of {@link MsgId}
     */
    public ActionData<MsgId> sendGroupMsg(long groupId, String msg, boolean autoEscape) {
        ActionData<MsgId> msgId;
        if (autoEscape) {
            String censoredMsg = BaniraUtils.replaceSensitiveContent(msg);
            msgId = super.sendGroupMsg(groupId, censoredMsg, autoEscape);
        } else {
            List<ArrayMsg> censoredMsg = BaniraUtils.replaceSensitiveContent(MessageConverser.stringToArray(msg));
            msgId = super.sendGroupMsg(groupId, censoredMsg, autoEscape);
        }
        if (isActionDataMsgIdNotEmpty(msgId)) {
            MessageRecord record = new MessageRecord()
                    .setMsgId(getActionDataMsgId(msgId))
                    .setBotId(super.getSelfId())
                    .setSenderId(super.getSelfId())
                    .setGroupId(groupId)
                    .setTime(System.currentTimeMillis() / 1000)
                    .setMsgRaw(msg)
                    .setMsgRecode(msg)
                    .setMsgType(EnumMessageType.GROUP);
            getMessageRecordManager().addMessageRecord(setMsgRecordTime(record));
        }
        return msgId;
    }

    /**
     * 发送群消息
     *
     * @param groupId    群号
     * @param msg        消息链
     * @param autoEscape 消息内容是否作为纯文本发送 ( 即不解析 CQ 码 ) , 只在 message 字段是字符串时有效
     * @return result {@link ActionData} of {@link MsgId}
     */
    public ActionData<MsgId> sendGroupMsg(long groupId, List<ArrayMsg> msg, boolean autoEscape) {
        List<ArrayMsg> censoredMsg = BaniraUtils.replaceSensitiveContent(msg);
        ActionData<MsgId> msgId = super.sendGroupMsg(groupId, censoredMsg, autoEscape);
        if (isActionDataMsgIdNotEmpty(msgId)) {
            MessageRecord record = new MessageRecord()
                    .setMsgId(getActionDataMsgId(msgId))
                    .setBotId(super.getSelfId())
                    .setSenderId(super.getSelfId())
                    .setGroupId(groupId)
                    .setTime(System.currentTimeMillis() / 1000)
                    .setMsgRaw(MessageConverser.arraysToString(msg))
                    .setMsgRecode(MessageConverser.arraysToString(msg))
                    .setMsgType(EnumMessageType.GROUP);
            getMessageRecordManager().addMessageRecord(setMsgRecordTime(record));
        }
        return msgId;
    }

    /**
     * 发送群消息
     *
     * @param groupId    群号
     * @param userId     调用者的QQ号 , 在QQ开放平台中用于设定@对象，如果不设置此参数会导致: 在bot返回前如果被不同用户多次调用，只会@最后一次调用的用户
     * @param msg        要发送的内容
     * @param autoEscape 消息内容是否作为纯文本发送 ( 即不解析 CQ 码 ) , 只在 message 字段是字符串时有效
     * @return result {@link ActionData} of {@link MsgId}
     */
    public ActionData<MsgId> sendGroupMsg(long groupId, long userId, String msg, boolean autoEscape) {
        ActionData<MsgId> msgId;
        if (autoEscape) {
            String censoredMsg = BaniraUtils.replaceSensitiveContent(msg);
            msgId = super.sendGroupMsg(groupId, userId, censoredMsg, autoEscape);
        } else {
            List<ArrayMsg> censoredMsg = BaniraUtils.replaceSensitiveContent(MessageConverser.stringToArray(msg));
            msgId = super.sendGroupMsg(groupId, userId, censoredMsg, autoEscape);
        }
        if (isActionDataMsgIdNotEmpty(msgId)) {
            MessageRecord record = new MessageRecord()
                    .setMsgId(getActionDataMsgId(msgId))
                    .setBotId(super.getSelfId())
                    .setSenderId(super.getSelfId())
                    .setGroupId(groupId)
                    .setTime(System.currentTimeMillis() / 1000)
                    .setMsgRaw(msg)
                    .setMsgRecode(msg)
                    .setMsgType(EnumMessageType.GROUP);
            getMessageRecordManager().addMessageRecord(setMsgRecordTime(record));
        }
        return msgId;
    }

    /**
     * 发送群消息
     *
     * @param groupId    群号
     * @param userId     调用者的QQ号 , 在QQ开放平台中用于设定@对象，如果不设置此参数会导致: 在bot返回前如果被不同用户多次调用，只会@最后一次调用的用户
     * @param msg        要发送的内容
     * @param autoEscape 消息内容是否作为纯文本发送 ( 即不解析 CQ 码 ) , 只在 message 字段是字符串时有效
     * @return result {@link ActionData} of {@link MsgId}
     */
    public ActionData<MsgId> sendGroupMsg(long groupId, long userId, List<ArrayMsg> msg, boolean autoEscape) {
        List<ArrayMsg> censoredMsg = BaniraUtils.replaceSensitiveContent(msg);
        ActionData<MsgId> msgId = super.sendGroupMsg(groupId, userId, censoredMsg, autoEscape);
        if (isActionDataMsgIdNotEmpty(msgId)) {
            MessageRecord record = new MessageRecord()
                    .setMsgId(getActionDataMsgId(msgId))
                    .setBotId(super.getSelfId())
                    .setSenderId(super.getSelfId())
                    .setGroupId(groupId)
                    .setTime(System.currentTimeMillis() / 1000)
                    .setMsgRaw(MessageConverser.arraysToString(msg))
                    .setMsgRecode(MessageConverser.arraysToString(msg))
                    .setMsgType(EnumMessageType.GROUP);
            getMessageRecordManager().addMessageRecord(setMsgRecordTime(record));
        }
        return msgId;
    }

    /**
     * 发送合并转发 (群)
     *
     * @param groupId 群号
     * @param msg     自定义转发消息 (可使用 ShiroUtils.generateForwardMsg() 方法创建)
     *                <a href="https://docs.go-cqhttp.org/cqcode/#%E5%90%88%E5%B9%B6%E8%BD%AC%E5%8F%91">参考文档</a>
     * @return result {@link ActionRaw}
     */
    public ActionData<MsgId> sendGroupForwardMsg(long groupId, List<Map<String, Object>> msg) {
        List<Map<String, Object>> censoredMsg = (List<Map<String, Object>>) BaniraUtils.replaceSensitiveContent(msg);
        ActionData<MsgId> msgIdData = super.sendGroupForwardMsg(groupId, censoredMsg);
        if (isActionDataMsgIdNotEmpty(msgIdData)) {
            Integer msgId = getActionDataMsgId(msgIdData);
            MessageRecord record = new MessageRecord()
                    .setMsgId(msgId)
                    .setBotId(super.getSelfId())
                    .setSenderId(super.getSelfId())
                    .setGroupId(groupId)
                    .setTime(System.currentTimeMillis() / 1000)
                    .setMsgType(EnumMessageType.GROUP);

            ActionData<GetForwardMsgResp> forwardMsg = getForwardMsg(msgId);
            List<MsgResp> msgRespList;
            if (isActionDataNotEmpty(forwardMsg)) {
                msgRespList = forwardMsg.getData().getMessages();
            } else {
                msgRespList = BaniraUtils.encodeSendForwardMsg(msg);
            }
            if (msgRespList.isEmpty()) {
                record.setMsgRaw(String.format("[CQ:forward,id=%s]", msgId))
                        .setMsgRecode(String.format("[CQ:forward,id=%s]", msgId));
            } else {
                ArrayMsg arrayMsg = BaniraUtils.packForwardMsg(null, msgRespList);
                record.setMsgRaw(JsonUtils.toJSONString(List.of(arrayMsg)))
                        .setMsgRecode(arrayMsg.toCQCode());
            }
            getMessageRecordManager().addMessageRecord(setMsgRecordTime(record));
        }
        return msgIdData;
    }

    /**
     * 发送合并转发 (私聊)
     *
     * @param userId 目标用户
     * @param msg    自定义转发消息 (可使用 ShiroUtils.generateForwardMsg() 方法创建)
     *               <a href="https://docs.go-cqhttp.org/cqcode/#%E5%90%88%E5%B9%B6%E8%BD%AC%E5%8F%91">参考文档</a>
     * @return result {@link ActionRaw}
     */
    public ActionData<MsgId> sendPrivateForwardMsg(long userId, List<Map<String, Object>> msg) {
        List<Map<String, Object>> censoredMsg = (List<Map<String, Object>>) BaniraUtils.replaceSensitiveContent(msg);
        ActionData<MsgId> msgIdData = super.sendPrivateForwardMsg(userId, censoredMsg);
        if (isActionDataMsgIdNotEmpty(msgIdData)) {
            Integer msgId = getActionDataMsgId(msgIdData);
            MessageRecord record = new MessageRecord()
                    .setMsgId(msgId)
                    .setBotId(super.getSelfId())
                    .setSenderId(super.getSelfId())
                    .setTargetId(userId)
                    .setTime(System.currentTimeMillis() / 1000)
                    .setMsgType(EnumMessageType.FRIEND);

            ActionData<GetForwardMsgResp> forwardMsg = getForwardMsg(msgId);
            List<MsgResp> msgRespList;
            if (isActionDataNotEmpty(forwardMsg)) {
                msgRespList = forwardMsg.getData().getMessages();
            } else {
                msgRespList = BaniraUtils.encodeSendForwardMsg(msg);
            }
            if (msgRespList.isEmpty()) {
                record.setMsgRaw(String.format("[CQ:forward,id=%s]", msgId))
                        .setMsgRecode(String.format("[CQ:forward,id=%s]", msgId));
            } else {
                ArrayMsg arrayMsg = BaniraUtils.packForwardMsg(null, msgRespList);
                record.setMsgRaw(JsonUtils.toJSONString(List.of(arrayMsg)))
                        .setMsgRecode(arrayMsg.toCQCode());
            }
            getMessageRecordManager().addMessageRecord(setMsgRecordTime(record));
        }
        return msgIdData;
    }

    /**
     * 发送合并转发
     *
     * @param event 事件
     * @param msg   自定义转发消息 (可使用 ShiroUtils.generateForwardMsg() 方法创建)
     *              <a href="https://docs.go-cqhttp.org/cqcode/#%E5%90%88%E5%B9%B6%E8%BD%AC%E5%8F%91">参考文档</a>
     * @return result {@link ActionRaw}
     */
    public ActionData<MsgId> sendForwardMsg(AnyMessageEvent event, List<Map<String, Object>> msg) {
        List<Map<String, Object>> censoredMsg = (List<Map<String, Object>>) BaniraUtils.replaceSensitiveContent(msg);
        ActionData<MsgId> msgIdData = super.sendForwardMsg(event, censoredMsg);
        if (isActionDataMsgIdNotEmpty(msgIdData)) {
            Integer msgId = getActionDataMsgId(msgIdData);
            MessageRecord record = new MessageRecord()
                    .setMsgId(msgId)
                    .setBotId(super.getSelfId())
                    .setSenderId(super.getSelfId())
                    .setGroupId(event.getGroupId())
                    .setTime(System.currentTimeMillis() / 1000)
                    .setMsgType(EnumMessageType.getType(event));

            ActionData<GetForwardMsgResp> forwardMsg = getForwardMsg(msgId);
            List<MsgResp> msgRespList;
            if (isActionDataNotEmpty(forwardMsg)) {
                msgRespList = forwardMsg.getData().getMessages();
            } else {
                msgRespList = BaniraUtils.encodeSendForwardMsg(msg);
            }
            if (msgRespList.isEmpty()) {
                record.setMsgRaw(String.format("[CQ:forward,id=%s]", msgId))
                        .setMsgRecode(String.format("[CQ:forward,id=%s]", msgId));
            } else {
                ArrayMsg arrayMsg = BaniraUtils.packForwardMsg(null, msgRespList);
                record.setMsgRaw(JsonUtils.toJSONString(List.of(arrayMsg)))
                        .setMsgRecode(arrayMsg.toCQCode());
            }
            if (record.getMsgType() != EnumMessageType.GROUP) {
                record.setTargetId(event.getUserId());
            }
            getMessageRecordManager().addMessageRecord(setMsgRecordTime(record));
        }
        return msgIdData;
    }

    /**
     * 发送群聊嵌套聊天记录
     *
     * @param groupId 为要发送的群聊
     * @param msg     为消息记录
     * @param prompt  为在外部消息列表显示的文字
     * @param source  为顶部文本
     * @param summary 为底部文本
     * @param news    为外显的摘要消息，最多三条；内容的构建参考消息节点。一般来说key为text,value为文本内容
     *                <p>参考 {@link com.mikuac.shiro.common.utils.ShiroUtils#generateSingleMsg(long, String, String)}</p>来生成单条聊天记录
     */
    public ActionData<MsgId> sendGroupForwardMsg(long groupId, List<Map<String, Object>> msg, String prompt, String source, String summary, List<Map<String, String>> news) {
        ActionData<MsgId> msgIdData = super.sendGroupForwardMsg(groupId
                , (List<Map<String, Object>>) BaniraUtils.replaceSensitiveContent(msg)
                , BaniraUtils.replaceSensitiveContent(prompt)
                , BaniraUtils.replaceSensitiveContent(source)
                , BaniraUtils.replaceSensitiveContent(summary)
                , (List<Map<String, String>>) BaniraUtils.replaceSensitiveContent(news)
        );
        if (isActionDataMsgIdNotEmpty(msgIdData)) {
            Integer msgId = getActionDataMsgId(msgIdData);
            MessageRecord record = new MessageRecord()
                    .setMsgId(msgId)
                    .setBotId(super.getSelfId())
                    .setSenderId(super.getSelfId())
                    .setGroupId(groupId)
                    .setTime(System.currentTimeMillis() / 1000)
                    .setMsgType(EnumMessageType.GROUP);

            ActionData<GetForwardMsgResp> forwardMsg = getForwardMsg(msgId);
            List<MsgResp> msgRespList;
            if (isActionDataNotEmpty(forwardMsg)) {
                msgRespList = forwardMsg.getData().getMessages();
            } else {
                msgRespList = BaniraUtils.encodeSendForwardMsg(msg);
            }
            if (msgRespList.isEmpty()) {
                record.setMsgRaw(String.format("[CQ:forward,id=%s]", msgId))
                        .setMsgRecode(String.format("[CQ:forward,id=%s]", msgId));
            } else {
                ArrayMsg arrayMsg = BaniraUtils.packForwardMsg(null, msgRespList);
                record.setMsgRaw(JsonUtils.toJSONString(List.of(arrayMsg)))
                        .setMsgRecode(arrayMsg.toCQCode());
            }
            getMessageRecordManager().addMessageRecord(setMsgRecordTime(record));
        }
        return msgIdData;
    }

    /**
     * 发送私聊嵌套聊天记录
     *
     * @param userId  为要发送的用户
     * @param msg     为消息记录
     * @param prompt  为在外部消息列表显示的文字
     * @param source  为顶部文本
     * @param summary 为底部文本
     * @param news    为外显的摘要消息，最多三条；内容的构建参考消息节点。一般来说key为text,value为文本内容
     *                <p>参考 {@link com.mikuac.shiro.common.utils.ShiroUtils#generateSingleMsg(long, String, String)}</p>来生成单条聊天记录
     */
    public ActionData<MsgId> sendPrivateForwardMsg(long userId, List<Map<String, Object>> msg, String prompt, String source, String summary, List<Map<String, String>> news) {
        ActionData<MsgId> msgIdData = super.sendPrivateForwardMsg(userId
                , (List<Map<String, Object>>) BaniraUtils.replaceSensitiveContent(msg)
                , BaniraUtils.replaceSensitiveContent(prompt)
                , BaniraUtils.replaceSensitiveContent(source)
                , BaniraUtils.replaceSensitiveContent(summary)
                , (List<Map<String, String>>) BaniraUtils.replaceSensitiveContent(news)
        );
        if (isActionDataMsgIdNotEmpty(msgIdData)) {
            Integer msgId = getActionDataMsgId(msgIdData);
            MessageRecord record = new MessageRecord()
                    .setMsgId(msgId)
                    .setBotId(super.getSelfId())
                    .setSenderId(super.getSelfId())
                    .setTargetId(userId)
                    .setTime(System.currentTimeMillis() / 1000)
                    .setMsgType(EnumMessageType.FRIEND);

            ActionData<GetForwardMsgResp> forwardMsg = getForwardMsg(msgId);
            List<MsgResp> msgRespList;
            if (isActionDataNotEmpty(forwardMsg)) {
                msgRespList = forwardMsg.getData().getMessages();
            } else {
                msgRespList = BaniraUtils.encodeSendForwardMsg(msg);
            }
            if (msgRespList.isEmpty()) {
                record.setMsgRaw(String.format("[CQ:forward,id=%s]", msgId))
                        .setMsgRecode(String.format("[CQ:forward,id=%s]", msgId));
            } else {
                ArrayMsg arrayMsg = BaniraUtils.packForwardMsg(null, msgRespList);
                record.setMsgRaw(JsonUtils.toJSONString(List.of(arrayMsg)))
                        .setMsgRecode(arrayMsg.toCQCode());
            }
            getMessageRecordManager().addMessageRecord(setMsgRecordTime(record));
        }
        return msgIdData;
    }

    /**
     * 设置群名
     *
     * @param groupId   群号
     * @param groupName 新群名
     * @return result {@link ActionRaw}
     */
    @Override
    public ActionRaw setGroupName(long groupId, String groupName) {
        return super.setGroupName(groupId, BaniraUtils.replaceSensitiveContent(groupName));
    }

    /**
     * 设置群组专属头衔
     *
     * @param groupId      群号
     * @param userId       要设置的 QQ 号
     * @param specialTitle 专属头衔，不填或空字符串表示删除专属头衔
     * @param duration     专属头衔有效期，单位秒，-1 表示永久，不过此项似乎没有效果，可能是只有某些特殊的时间长度有效，有待测试
     * @return result {@link ActionRaw}
     */
    @Override
    public ActionRaw setGroupSpecialTitle(long groupId, long userId, String specialTitle, int duration) {
        return super.setGroupSpecialTitle(groupId, userId, BaniraUtils.replaceSensitiveContent(specialTitle), duration);
    }

    /**
     * 设置群名片（群备注）
     *
     * @param groupId 群号
     * @param userId  要设置的 QQ 号
     * @param card    群名片内容，不填或空字符串表示删除群名片
     * @return result {@link ActionRaw}
     */
    @Override
    public ActionRaw setGroupCard(long groupId, long userId, String card) {
        return super.setGroupCard(groupId, userId, BaniraUtils.replaceSensitiveContent(card));
    }

    /**
     * 发送群公告
     *
     * @param groupId 群号
     * @param content 公告内容
     * @return result {@link ActionRaw}
     */
    @Override
    public ActionRaw sendGroupNotice(long groupId, String content) {
        return super.sendGroupNotice(groupId, BaniraUtils.replaceSensitiveContent(content));
    }

    /**
     * 上传群文件
     * 在不提供 folder 参数的情况下默认上传到根目录
     * 只能上传本地文件, 需要上传 http 文件的话请先下载到本地
     *
     * @param groupId 群号
     * @param file    本地文件路径
     * @param name    储存名称
     * @param folder  父目录ID
     * @return result {@link ActionRaw}
     */
    @Override
    public ActionRaw uploadGroupFile(long groupId, String file, String name, String folder) {
        return super.uploadGroupFile(groupId, file, BaniraUtils.replaceSensitiveFileName(name), folder);
    }

    /**
     * 上传群文件
     * 在不提供 folder 参数的情况下默认上传到根目录
     * 只能上传本地文件, 需要上传 http 文件的话请先下载到本地
     *
     * @param groupId 群号
     * @param file    本地文件路径
     * @param name    储存名称
     * @return result {@link ActionRaw}
     */
    @Override
    public ActionRaw uploadGroupFile(long groupId, String file, String name) {
        return super.uploadGroupFile(groupId, file, BaniraUtils.replaceSensitiveFileName(name));
    }

    // endregion override


    // region ex

    /**
     * 获取合并转发消息
     */
    public ActionData<GetForwardMsgResp> getForwardMsg(Long groupId, Long senderId, int msgId) {
        ActionData<GetForwardMsgResp> result = null;
        IMessageRecordManager messageRecordManager = SpringContextHolder.getBean(IMessageRecordManager.class);
        List<MessageRecord> list = messageRecordManager.getMessageRecordList(new MessageRecordQueryParam(true)
                .setBotId(this.getSelfId())
                .setGroupId(0L, groupId)
                .setSenderId(0L, senderId)
                .setMsgId(msgId)
        );
        if (CollectionUtils.isNotNullOrEmpty(list)) {
            MessageRecord messageRecord = list.stream()
                    .filter(recode -> BaniraUtils.hasForward(recode.getMsgRecode()))
                    .findFirst()
                    .orElse(null);
            if (messageRecord != null) {
                result = new ActionData<>();
                GetForwardMsgResp data = new GetForwardMsgResp();
                List<MsgResp> msgResps = BaniraUtils.getForwardContentFirst(messageRecord.getMsgRecode());
                data.setMessages(msgResps);
                result.setData(data);
            }
        }
        if (result == null) {
            result = super.getForwardMsg(msgId);
        }
        return result;
    }

    /**
     * 设置消息表情回复
     *
     * @param msgId 消息Id
     * @param code  表情代码 {@link com.mikuac.shiro.common.utils.FaceUtils}
     * @return 是否成功
     */
    public boolean setMsgEmojiLike(int msgId, long code) {
        ActionRaw actionRaw = super.setMsgEmojiLike(msgId, String.valueOf(code), true);
        return actionRaw != null && "ok".equalsIgnoreCase(actionRaw.getStatus());
    }

    /**
     * 设置消息表情回复 爱心❤
     *
     * @param msgId 消息Id
     * @return 是否成功
     */
    public boolean setMsgEmojiLikeHeart(int msgId) {
        return setMsgEmojiLike(msgId, 66);
    }

    /**
     * 设置消息表情回复 心碎💔
     *
     * @param msgId 消息Id
     * @return 是否成功
     */
    public boolean setMsgEmojiLikeBrokenHeart(int msgId) {
        return setMsgEmojiLike(msgId, 67);
    }

    /**
     * 设置消息表情回复 No❌
     *
     * @param msgId 消息Id
     * @return 是否成功
     */
    public boolean setMsgEmojiLikeNo(int msgId) {
        return setMsgEmojiLike(msgId, 123);
    }

    /**
     * 设置消息表情回复 收到👌
     *
     * @param msgId 消息Id
     * @return 是否成功
     */
    public boolean setMsgEmojiLikeOk(int msgId) {
        return setMsgEmojiLike(msgId, 124);
    }

    /**
     * 获取Bot信息
     */
    @Nonnull
    public LoginInfoResp getLoginInfoEx() {
        LoginInfoResp result = new LoginInfoResp();
        ActionData<LoginInfoResp> loginInfo = super.getLoginInfo();
        if (isActionDataNotEmpty(loginInfo)) {
            result = loginInfo.getData();
        } else {
            result.setUserId(this.getSelfId());
            result.setNickname(BaniraUtils.getBotNick());
        }
        return result;
    }

    /**
     * 获取群名称
     *
     * @return 未获取到时返回空字符串
     */
    @Nonnull
    public String getGroupNameEx(long groupId) {
        ActionData<GroupInfoResp> groupInfo = super.getGroupInfo(groupId, false);
        if (isActionDataNotEmpty(groupInfo)) {
            return groupInfo.getData().getGroupName();
        } else {
            return "";
        }
    }

    /**
     * 获取用户名称
     *
     * @return 未获取到时返回空字符串
     */
    @Nonnull
    public String getUserNameEx(Long groupId, Long userId) {
        if (!BaniraUtils.isUserIdValid(userId)) return "";
        String name = null;
        if (BaniraUtils.isGroupIdValid(groupId)) {
            ActionData<GroupMemberInfoResp> groupMemberInfo = super.getGroupMemberInfo(groupId, userId, false);
            if (isActionDataNotEmpty(groupMemberInfo)) {
                name = groupMemberInfo.getData().getNickname();
            }
        }
        if (StringUtils.isNullOrEmpty(name)) {
            ActionList<FriendInfoResp> friendList = super.getFriendList();
            if (isActionDataNotEmpty(friendList)) {
                name = friendList.getData().stream()
                        .filter(f -> f.getUserId().equals(userId))
                        .map(FriendInfoResp::getNickname)
                        .filter(Objects::nonNull)
                        .findFirst().orElse(null);
            }
        }
        if (StringUtils.isNullOrEmpty(name)) {
            ActionData<StrangerInfoResp> strangerInfo = super.getStrangerInfo(userId, false);
            if (isActionDataNotEmpty(strangerInfo)) {
                name = strangerInfo.getData().getNickname();
            }
        }
        return StringUtils.isNullOrEmpty(name) ? "" : name;
    }

    // endregion ex


    // region util

    public boolean isActionDataNotEmpty(ActionData<?> actionData) {
        return actionData != null && actionData.getData() != null;
    }

    public boolean isActionDataNotEmpty(ActionList<?> actionData) {
        return actionData != null && CollectionUtils.isNotNullOrEmpty(actionData.getData());
    }

    public boolean isActionDataMsgIdNotEmpty(ActionData<MsgId> msgId) {
        return isActionDataNotEmpty(msgId) && msgId.getData().getMessageId() != null;
    }

    public Integer getActionDataMsgId(ActionData<MsgId> msgId) {
        return msgId.getData().getMessageId();
    }

    public IMessageRecordManager getMessageRecordManager() {
        return SpringContextHolder.getBean(IMessageRecordManager.class);
    }

    public MessageRecord setMsgRecordTime(MessageRecord record) {
        if (StringUtils.isNotNullOrEmpty(record.getMsgId())) {
            try {
                ActionData<MsgResp> msg = super.getMsg(StringUtils.toInt(record.getMsgId()));
                if (isActionDataNotEmpty(msg)) {
                    Long time = msg.getData().getTime();
                    if (time != null) {
                        record.setTime(time);
                    }
                }
            } catch (Exception ignored) {
            }
        }
        return record;
    }

    /**
     * 判断是否群主
     */
    public boolean isGroupOwner(@Nullable Long groupId, @Nonnull Long qq) {
        return BaniraUtils.isGroupOwner(this, groupId, qq);
    }

    /**
     * 判断是否群管理
     */
    public boolean isGroupAdmin(@Nullable Long groupId, @Nonnull Long qq) {
        return BaniraUtils.isGroupAdmin(this, groupId, qq);
    }

    /**
     * 判断是否群主或管理
     */
    public boolean isGroupOwnerOrAdmin(@Nullable Long groupId, @Nonnull Long qq) {
        return this.isGroupOwner(groupId, qq) || this.isGroupAdmin(groupId, qq);
    }

    /**
     * 判断是否管理员
     * <p>
     * 主人、管家、群主、群管理员、女仆
     */
    public boolean isAdmin(@Nullable Long groupId, @Nonnull Long qq) {
        return BaniraUtils.isAdmin(this, groupId, qq);
    }

    /**
     * 判断是否群成员
     */
    public boolean isGroupMember(@Nullable Long groupId, @Nonnull Long qq) {
        return BaniraUtils.isGroupMember(this, groupId, qq);
    }

    /**
     * 判断是否在群内
     */
    public boolean isInGroup(@Nullable Long groupId, @Nonnull Long qq) {
        return BaniraUtils.isInGroup(this, groupId, qq);
    }

    /**
     * 判断机器人是否群主
     */
    public boolean isGroupOwner(@Nullable Long groupId) {
        return BaniraUtils.isGroupOwner(this, groupId, this.getSelfId());
    }

    /**
     * 判断机器人是否群管理
     */
    public boolean isGroupAdmin(@Nullable Long groupId) {
        return BaniraUtils.isGroupAdmin(this, groupId, this.getSelfId());
    }

    /**
     * 判断机器人是否群主或管理
     */
    public boolean isGroupOwnerOrAdmin(@Nullable Long groupId) {
        return this.isGroupOwner(groupId) || this.isGroupAdmin(groupId);
    }

    /**
     * 判断a是否b的上属
     */
    public boolean isUpper(@Nullable Long groupId, @Nonnull Long a, @Nonnull Long b) {
        return BaniraUtils.isUpper(this, groupId, a, b);
    }

    /**
     * 判断a是否b的上属
     */
    public boolean isUpperInGroup(@Nullable Long groupId, @Nonnull Long a, @Nonnull Long b) {
        return BaniraUtils.isUpperInGroup(this, groupId, a, b);
    }

    /**
     * 判断机器人是否b的上属
     */
    public boolean isUpperInGroup(@Nullable Long groupId, @Nonnull Long b) {
        return BaniraUtils.isUpperInGroup(this, groupId, this.getSelfId(), b);
    }

    /**
     * 获取所有拥有的权限
     */
    @Nonnull
    public Set<EnumPermission> getPermission(@Nullable Long groupId, @Nonnull Long qq) {
        return BaniraUtils.getPermission(this, groupId, qq);
    }

    /**
     * 判断是否拥有某个权限
     */
    public boolean hasPermission(@Nullable Long groupId, @Nonnull Long qq, @Nonnull EnumPermission permission) {
        return BaniraUtils.hasPermission(this, groupId, qq, permission);
    }

    /**
     * 判断是否拥有全部权限
     */
    public boolean hasAllPermissions(@Nullable Long groupId, @Nonnull Long qq, @Nonnull EnumPermission... permissions) {
        return BaniraUtils.hasAllPermissions(this, groupId, qq, permissions);
    }

    /**
     * 判断是否拥有全部权限
     */
    public boolean hasAllPermissions(@Nullable Long groupId, @Nonnull Long qq, @Nonnull Collection<EnumPermission> permissions) {
        return BaniraUtils.hasAllPermissions(this, groupId, qq, permissions);
    }

    /**
     * 判断是否拥有任意一个权限
     */
    public boolean hasAnyPermissions(@Nullable Long groupId, @Nonnull Long qq, @Nonnull EnumPermission... permission) {
        return BaniraUtils.hasAnyPermissions(this, groupId, qq, permission);
    }

    /**
     * 判断是否拥有任意一个权限
     */
    public boolean hasAnyPermissions(@Nullable Long groupId, @Nonnull Long qq, @Nonnull Collection<EnumPermission> permissions) {
        return BaniraUtils.hasAnyPermissions(this, groupId, qq, permissions);
    }

    /**
     * 判断是否被AT
     */
    public boolean isMentioned(List<ArrayMsg> arrayMsg) {
        return this.getSelfId() == BaniraUtils.getAtUserId(arrayMsg);
    }

    /**
     * 判断是否被AT
     */
    public boolean isMentioned(String msg) {
        return this.getSelfId() == BaniraUtils.getAtUserId(msg);
    }

    public List<ArrayMsg> getReplyContentById(Long replyId) {
        return BaniraUtils.getReplyContentById(this, replyId);
    }

    public List<ArrayMsg> getReplyContent(List<ArrayMsg> arrayMsg) {
        return BaniraUtils.getReplyContent(this, arrayMsg);
    }

    public String getReplyContentString(List<ArrayMsg> arrayMsg) {
        return BaniraUtils.getReplyContentString(this, arrayMsg);
    }

    public List<ArrayMsg> getReplyContent(String msg) {
        return BaniraUtils.getReplyContent(this, msg);
    }

    public String getReplyContentString(String msg) {
        return BaniraUtils.getReplyContentString(this, msg);
    }

    // endregion util

}
