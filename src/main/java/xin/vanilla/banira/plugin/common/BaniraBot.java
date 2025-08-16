package xin.vanilla.banira.plugin.common;

import com.mikuac.shiro.common.utils.MessageConverser;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.action.common.ActionData;
import com.mikuac.shiro.dto.action.common.ActionRaw;
import com.mikuac.shiro.dto.action.common.MsgId;
import com.mikuac.shiro.dto.action.response.MsgResp;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import com.mikuac.shiro.model.ArrayMsg;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.enums.EnumMessageType;
import xin.vanilla.banira.enums.EnumPermission;
import xin.vanilla.banira.service.IMessageRecordManager;
import xin.vanilla.banira.start.SpringContextHolder;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Bot大包装，实现记录发送的消息
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
        ActionData<MsgId> msgId = super.sendPrivateMsg(userId, msg, autoEscape);
        if (isActionDataMsgIdNotEmpty(msgId)) {
            MessageRecord record = new MessageRecord()
                    .setMsgId(getActionDataMsgId(msgId))
                    .setBotId(super.getSelfId())
                    .setSenderId(super.getSelfId())
                    .setTargetId(userId)
                    .setTime(System.currentTimeMillis() / 1000)
                    .setMsgRaw(msg)
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
        ActionData<MsgId> msgId = super.sendPrivateMsg(userId, msg, autoEscape);
        if (isActionDataMsgIdNotEmpty(msgId)) {
            MessageRecord record = new MessageRecord()
                    .setMsgId(getActionDataMsgId(msgId))
                    .setBotId(super.getSelfId())
                    .setSenderId(super.getSelfId())
                    .setTargetId(userId)
                    .setTime(System.currentTimeMillis() / 1000)
                    .setMsgRaw(MessageConverser.arraysToString(msg))
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
        ActionData<MsgId> msgId = super.sendPrivateMsg(groupId, userId, msg, autoEscape);
        if (isActionDataMsgIdNotEmpty(msgId)) {
            MessageRecord record = new MessageRecord()
                    .setMsgId(getActionDataMsgId(msgId))
                    .setBotId(super.getSelfId())
                    .setSenderId(super.getSelfId())
                    .setGroupId(groupId)
                    .setTargetId(userId)
                    .setTime(System.currentTimeMillis() / 1000)
                    .setMsgRaw(msg)
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
        ActionData<MsgId> msgId = super.sendPrivateMsg(groupId, userId, msg, autoEscape);
        if (isActionDataMsgIdNotEmpty(msgId)) {
            MessageRecord record = new MessageRecord()
                    .setMsgId(getActionDataMsgId(msgId))
                    .setBotId(super.getSelfId())
                    .setSenderId(super.getSelfId())
                    .setGroupId(groupId)
                    .setTargetId(userId)
                    .setTime(System.currentTimeMillis() / 1000)
                    .setMsgRaw(MessageConverser.arraysToString(msg))
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
        ActionData<MsgId> msgId = super.sendGroupMsg(groupId, msg, autoEscape);
        if (isActionDataMsgIdNotEmpty(msgId)) {
            MessageRecord record = new MessageRecord()
                    .setMsgId(getActionDataMsgId(msgId))
                    .setBotId(super.getSelfId())
                    .setSenderId(super.getSelfId())
                    .setGroupId(groupId)
                    .setTime(System.currentTimeMillis() / 1000)
                    .setMsgRaw(msg)
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
        ActionData<MsgId> msgId = super.sendGroupMsg(groupId, msg, autoEscape);
        if (isActionDataMsgIdNotEmpty(msgId)) {
            MessageRecord record = new MessageRecord()
                    .setMsgId(getActionDataMsgId(msgId))
                    .setBotId(super.getSelfId())
                    .setSenderId(super.getSelfId())
                    .setGroupId(groupId)
                    .setTime(System.currentTimeMillis() / 1000)
                    .setMsgRaw(MessageConverser.arraysToString(msg))
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
        ActionData<MsgId> msgId = super.sendGroupMsg(groupId, userId, msg, autoEscape);
        if (isActionDataMsgIdNotEmpty(msgId)) {
            MessageRecord record = new MessageRecord()
                    .setMsgId(getActionDataMsgId(msgId))
                    .setBotId(super.getSelfId())
                    .setSenderId(super.getSelfId())
                    .setGroupId(groupId)
                    .setTime(System.currentTimeMillis() / 1000)
                    .setMsgRaw(msg)
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
        ActionData<MsgId> msgId = super.sendGroupMsg(groupId, userId, msg, autoEscape);
        if (isActionDataMsgIdNotEmpty(msgId)) {
            MessageRecord record = new MessageRecord()
                    .setMsgId(getActionDataMsgId(msgId))
                    .setBotId(super.getSelfId())
                    .setSenderId(super.getSelfId())
                    .setGroupId(groupId)
                    .setTime(System.currentTimeMillis() / 1000)
                    .setMsgRaw(MessageConverser.arraysToString(msg))
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
        ActionData<MsgId> msgId = super.sendGroupForwardMsg(groupId, msg);
        if (isActionDataMsgIdNotEmpty(msgId)) {
            MessageRecord record = new MessageRecord()
                    .setMsgId(getActionDataMsgId(msgId))
                    .setBotId(super.getSelfId())
                    .setSenderId(super.getSelfId())
                    .setGroupId(groupId)
                    .setTime(System.currentTimeMillis() / 1000)
                    .setMsgRaw(String.format("[CQ:forward,id=%s]", getActionDataMsgId(msgId)))
                    .setMsgType(EnumMessageType.GROUP);
            getMessageRecordManager().addMessageRecord(setMsgRecordTime(record));
        }
        return msgId;
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
        ActionData<MsgId> msgId = super.sendPrivateForwardMsg(userId, msg);
        if (isActionDataMsgIdNotEmpty(msgId)) {
            MessageRecord record = new MessageRecord()
                    .setMsgId(getActionDataMsgId(msgId))
                    .setBotId(super.getSelfId())
                    .setSenderId(super.getSelfId())
                    .setTargetId(userId)
                    .setTime(System.currentTimeMillis() / 1000)
                    .setMsgRaw(String.format("[CQ:forward,id=%s]", getActionDataMsgId(msgId)))
                    .setMsgType(EnumMessageType.FRIEND);
            getMessageRecordManager().addMessageRecord(setMsgRecordTime(record));
        }
        return msgId;
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
        ActionData<MsgId> msgId = super.sendForwardMsg(event, msg);
        if (isActionDataMsgIdNotEmpty(msgId)) {
            MessageRecord record = new MessageRecord()
                    .setMsgId(getActionDataMsgId(msgId))
                    .setBotId(super.getSelfId())
                    .setSenderId(super.getSelfId())
                    .setGroupId(event.getGroupId())
                    .setTime(System.currentTimeMillis() / 1000)
                    .setMsgRaw(String.format("[CQ:forward,id=%s]", getActionDataMsgId(msgId)))
                    .setMsgType(EnumMessageType.getType(event));
            if (record.getMsgType() != EnumMessageType.GROUP) {
                record.setTargetId(event.getUserId());
            }
            getMessageRecordManager().addMessageRecord(setMsgRecordTime(record));
        }
        return msgId;
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
        ActionData<MsgId> msgId = super.sendGroupForwardMsg(groupId, msg, prompt, source, summary, news);
        if (isActionDataMsgIdNotEmpty(msgId)) {
            MessageRecord record = new MessageRecord()
                    .setMsgId(getActionDataMsgId(msgId))
                    .setBotId(super.getSelfId())
                    .setSenderId(super.getSelfId())
                    .setGroupId(groupId)
                    .setTime(System.currentTimeMillis() / 1000)
                    .setMsgRaw(String.format("[CQ:forward,id=%s]", getActionDataMsgId(msgId)))
                    .setMsgType(EnumMessageType.GROUP);
            getMessageRecordManager().addMessageRecord(setMsgRecordTime(record));
        }
        return msgId;
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
        ActionData<MsgId> msgId = super.sendPrivateForwardMsg(userId, msg, prompt, source, summary, news);
        if (isActionDataMsgIdNotEmpty(msgId)) {
            MessageRecord record = new MessageRecord()
                    .setMsgId(getActionDataMsgId(msgId))
                    .setBotId(super.getSelfId())
                    .setSenderId(super.getSelfId())
                    .setTargetId(userId)
                    .setTime(System.currentTimeMillis() / 1000)
                    .setMsgRaw(String.format("[CQ:forward,id=%s]", getActionDataMsgId(msgId)))
                    .setMsgType(EnumMessageType.FRIEND);
            getMessageRecordManager().addMessageRecord(setMsgRecordTime(record));
        }
        return msgId;
    }

    // endregion override


    // region util

    public boolean isActionDataNotEmpty(ActionData<?> actionData) {
        return actionData != null && actionData.getData() != null;
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

    public List<ArrayMsg> getReplayContentById(Long replayId) {
        return BaniraUtils.getReplayContentById(this, replayId);
    }

    public List<ArrayMsg> getReplayContent(List<ArrayMsg> arrayMsg) {
        return BaniraUtils.getReplayContent(this, arrayMsg);
    }

    public String getReplayContentString(List<ArrayMsg> arrayMsg) {
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
