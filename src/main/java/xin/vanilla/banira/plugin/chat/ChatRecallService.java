package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.enums.EnumMessageType;
import xin.vanilla.banira.mapper.param.MessageRecordQueryParam;
import xin.vanilla.banira.plugin.chat.agent.AgentContext;
import xin.vanilla.banira.plugin.chat.capability.AiCapabilityArgs;
import xin.vanilla.banira.service.IMessageRecordManager;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

@Component
public class ChatRecallService {

    private static final int MAX_RECALL_COUNT = 5;
    private static final int FALLBACK_SCAN_LIMIT = 50;

    private final IMessageRecordManager messageRecordManager;

    public ChatRecallService(IMessageRecordManager messageRecordManager) {
        this.messageRecordManager = messageRecordManager;
    }

    @Nonnull
    public String recallLastAiReply(@Nonnull AgentContext ctx, @Nonnull Map<String, String> args) {
        int count = Math.min(MAX_RECALL_COUNT, Math.max(0, AiCapabilityArgs.parseInt(args, "count", 0)));
        List<Integer> targetIds = RecentAiReplyTracker.takeLast(ctx, count);
        if (targetIds.isEmpty()) {
            targetIds = findRecentBotMessageIds(ctx, count <= 0 ? 1 : count);
        }
        if (targetIds.isEmpty()) {
            return "没有找到可撤回的上一条回复。";
        }

        int recalled = 0;
        for (Integer msgId : targetIds) {
            if (msgId == null || msgId <= 0) {
                continue;
            }
            ctx.bot().deleteMsg(msgId);
            markRecalled(ctx, msgId);
            recalled++;
        }
        return recalled > 0 ? "已撤回。" : "没有找到可撤回的上一条回复。";
    }

    @Nonnull
    private List<Integer> findRecentBotMessageIds(@Nonnull AgentContext ctx, int count) {
        if (ctx.msgType() == EnumMessageType.GROUP && (ctx.groupId() == null || ctx.groupId() <= 0)) {
            return List.of();
        }
        if (ctx.msgType() != EnumMessageType.GROUP && (ctx.senderId() == null || ctx.senderId() <= 0)) {
            return List.of();
        }
        MessageRecordQueryParam param = new MessageRecordQueryParam(true);
        param.setBotId(ctx.botId());
        param.setSenderId(ctx.botId());
        param.setRecalled(false);
        if (ctx.msgType() == EnumMessageType.GROUP) {
            param.setGroupId(ctx.groupId());
            param.setMsgType(EnumMessageType.GROUP.name());
        } else {
            param.setTargetId(ctx.senderId());
        }
        param.addOrderBy(MessageRecordQueryParam.ORDER_ID, false);
        param.setLimit(FALLBACK_SCAN_LIMIT);
        param.setOffset(0);

        List<Integer> result = new ArrayList<>();
        List<MessageRecord> records = messageRecordManager.getMessageRecordList(param);
        int scanned = 0;
        for (MessageRecord record : records) {
            if (scanned++ >= FALLBACK_SCAN_LIMIT) {
                break;
            }
            if (record.recalled()) {
                continue;
            }
            if (result.size() >= count || result.size() >= MAX_RECALL_COUNT) {
                break;
            }
            int msgId = StringUtils.toInt(record.getMsgId());
            if (msgId > 0) {
                result.add(msgId);
            }
        }
        return result;
    }

    private void markRecalled(@Nonnull AgentContext ctx, int msgId) {
        if (ctx.msgType() == EnumMessageType.GROUP && ctx.groupId() != null && ctx.groupId() > 0) {
            messageRecordManager.markGroupMessageRecalled(ctx.groupId(), msgId);
        } else if (ctx.senderId() != null && ctx.senderId() > 0) {
            messageRecordManager.markPrivateMessageRecalled(ctx.senderId(), msgId);
        }
    }
}
