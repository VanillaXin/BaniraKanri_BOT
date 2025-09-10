package xin.vanilla.banira.plugin;

import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.GroupMsgDeleteNoticeHandler;
import com.mikuac.shiro.annotation.PrivateMsgDeleteNoticeHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.common.utils.JsonUtils;
import com.mikuac.shiro.common.utils.MessageConverser;
import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.common.utils.ShiroUtils;
import com.mikuac.shiro.dto.action.common.ActionData;
import com.mikuac.shiro.dto.action.response.GetForwardMsgResp;
import com.mikuac.shiro.dto.action.response.MsgResp;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import com.mikuac.shiro.dto.event.notice.GroupMsgDeleteNoticeEvent;
import com.mikuac.shiro.dto.event.notice.PrivateMsgDeleteNoticeEvent;
import com.mikuac.shiro.model.ArrayMsg;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.enums.EnumMessageType;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.service.IMessageRecordManager;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.DateUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * 消息记录
 */
@Slf4j
@Shiro
@Component
public class RecorderPlugin extends BasePlugin {

    @Resource
    private IMessageRecordManager messageRecordManager;

    /**
     * 获取帮助信息
     *
     * @param groupId 群组ID
     * @param types   帮助类型
     */
    @Nonnull
    @Override
    public List<String> getHelpInfo(Long groupId, @Nonnull String... types) {
        return List.of();
    }

    @AnyMessageHandler
    public void recorder(BaniraBot bot, AnyMessageEvent event) {
        LOGGER.debug("String: {}", event.getMessage());
        LOGGER.debug("Raw: {}", event.getRawMessage());
        LOGGER.debug("Array: {}", event.getArrayMsg());
        MessageRecord record = new MessageRecord()
                .setMsgId(event.getMessageId())
                .setBotId(bot.getSelfId())
                .setSenderId(event.getUserId())
                .setGroupId(event.getGroupId())
                .setTime(event.getTime())
                .setMsgRaw(JsonUtils.toJSONString(event.getArrayMsg()))
                .setMsgRecode(event.getMessage())
                .setMsgType(EnumMessageType.getType(event));
        if (record.getMsgType() == EnumMessageType.MEMBER) {
            record.setTargetId(event.getUserId());
        }

        // 解析转发消息
        if (BaniraUtils.hasForward(event.getArrayMsg())) {
            ActionData<GetForwardMsgResp> forwardMsg = bot.getForwardMsg(event.getMessageId());
            if (bot.isActionDataNotEmpty(forwardMsg)) {
                record.setMsgRecode(
                        BaniraUtils.encodeForwardMsg(
                                BaniraUtils.getForwardId(event.getArrayMsg())
                                , forwardMsg.getData().getMessages()
                        )
                );
            }
        }

        messageRecordManager.addMessageRecord(record);
    }

    @GroupMsgDeleteNoticeHandler
    public void deleted(BaniraBot bot, GroupMsgDeleteNoticeEvent event) {
        notice(bot, event.getOperatorId(), messageRecordManager.getGroupMessageRecord(event.getGroupId(), event.getMessageId()));
    }

    @PrivateMsgDeleteNoticeHandler
    public void deleted(BaniraBot bot, PrivateMsgDeleteNoticeEvent event) {
        notice(bot, event.getUserId(), messageRecordManager.getPrivateMessageRecord(event.getUserId(), event.getMessageId()));
    }

    private void notice(BaniraBot bot, Long operatorId, MessageRecord record) {
        if (record == null || globalConfig.get().backGroup().isEmpty()) return;
        List<ArrayMsg> arrayMsgList = MessageConverser.stringToArray(record.getMsgRecode());
        String head;
        if (record.getMsgType() == EnumMessageType.GROUP) {
            String groupName = bot.getGroupNameEx(record.getGroupId());
            String operatorName = bot.getUserNameEx(record.getGroupId(), operatorId);
            String senderName;
            if (record.getSenderId().equals(operatorId)) senderName = operatorName;
            else senderName = bot.getUserNameEx(record.getGroupId(), record.getSenderId());
            head = MsgUtils.builder()
                    .text(String.format("ID：%s\n", record.getId()))
                    .text(String.format("群聊：『%s』(%s)\n", groupName, record.getGroupId()))
                    .text(String.format("操作者：『%s』(%s)\n", operatorName, operatorId))
                    .text(String.format("发送者：『%s』(%s)\n", senderName, record.getSenderId()))
                    .text(String.format("消息发送时间：『%s』(%s)", DateUtils.toString(new Date(record.getTime() * 1000), "MM/dd HH:mm:ss"), record.getTime()))
                    .build();
        } else {
            String senderName = bot.getUserNameEx(record.getGroupId(), record.getSenderId());
            head = MsgUtils.builder()
                    .text(String.format("ID：%s\n", record.getId()))
                    .text(String.format("私聊：『%s』(%s)\n", senderName, record.getSenderId()))
                    .text(String.format("消息发送时间：『%s』(%s)", DateUtils.toString(new Date(record.getTime() * 1000), "MM/dd HH:mm:ss"), record.getTime()))
                    .build();
        }
        for (Long backGroup : globalConfig.get().backGroup()) {
            if (BaniraUtils.isGroupIdValid(backGroup) && !backGroup.equals(record.getGroupId())) {
                if (BaniraUtils.hasForward(arrayMsgList)) {
                    // 方式1：失败
                    // LoginInfoResp loginInfoEx = bot.getLoginInfoEx();
                    // String senderName = bot.getUserNameEx(record.getGroupId(), record.getSenderId());
                    // List<Map<String, Object>> forwardMsg = new ArrayList<>();
                    // forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                    //         , head
                    // ));
                    // String jsonString = JsonUtils.toJSONString(arrayMsgList);
                    // for (String s : jsonString.split("(?<=\\G.{2000})")) {
                    //     forwardMsg.add(ShiroUtils.generateSingleMsg(record.getSenderId(), senderName
                    //             , s
                    //     ));
                    // }
                    // bot.sendGroupForwardMsg(backGroup, forwardMsg);

                    // 方式2：失败
                    // bot.sendGroupMsg(backGroup, head, false);
                    // bot.sendGroupMsg(backGroup, JsonUtils.readValue(record.getMsgRaw(), new TypeReference<List<ArrayMsg>>() {
                    // }), false);

                    // 方式3
                    List<Map<String, Object>> forwardMsg = new ArrayList<>();
                    List<List<MsgResp>> forwardContent = BaniraUtils.getForwardContent(record.getMsgRecode());
                    if (CollectionUtils.isNullOrEmpty(forwardContent)) {
                        bot.sendGroupMsg(backGroup, head + "\n" + JsonUtils.toJSONString(arrayMsgList), false);
                    } else {
                        for (MsgResp msgResp : forwardContent.getFirst()) {
                            forwardMsg.add(ShiroUtils.generateSingleMsg(StringUtils.toLong(msgResp.getSender().getUserId()), msgResp.getSender().getNickname()
                                    , msgResp.getMessage()
                            ));
                        }
                        bot.sendGroupMsg(backGroup, head, false);
                        bot.sendGroupForwardMsg(backGroup, forwardMsg);
                    }

                    // 方式CNM
                    // bot.sendGroupMsg(backGroup, head + "\n" + JsonUtils.toJSONString(arrayMsgList), false);
                } else if (BaniraUtils.hasComplexMsg(arrayMsgList)) {
                    bot.sendGroupMsg(backGroup, head, false);
                    bot.sendGroupMsg(backGroup, record.getMsgRecode(), false);
                } else {
                    bot.sendGroupMsg(backGroup, head + "\n" + record.getMsgRecode(), false);
                }
            }
        }
    }

}
