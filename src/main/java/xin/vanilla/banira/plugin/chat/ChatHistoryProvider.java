package xin.vanilla.banira.plugin.chat;

import com.mikuac.shiro.model.ArrayMsg;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.enums.EnumMessageType;
import xin.vanilla.banira.mapper.param.MessageRecordQueryParam;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.service.IMessageRecordManager;
import xin.vanilla.banira.util.StringUtils;

import java.util.*;

public class ChatHistoryProvider {

    private static final int QUOTED_CONTEXT_BEFORE = 5;
    private static final int QUOTED_CONTEXT_AFTER = 5;

    private final ChatConfig cfg;
    private final IMessageRecordManager messageRecordManager;

    public ChatHistoryProvider(@Nonnull ChatConfig cfg, @Nonnull IMessageRecordManager messageRecordManager) {
        this.cfg = cfg;
        this.messageRecordManager = messageRecordManager;
    }

    @Nonnull
    public List<MessageRecord> history(@Nonnull BaniraBot bot, @Nonnull BaniraCodeContext ctx) {
        return normalize(getHistoryRecords(bot, ctx), ctx);
    }

    @Nonnull
    private List<MessageRecord> getHistoryRecords(@Nonnull BaniraBot bot, @Nonnull BaniraCodeContext ctx) {
        List<MessageRecord> records = new ArrayList<>(getRecentHistoryRecords(bot, ctx));
        addQuotedNeighborhood(records, bot, ctx);
        return records;
    }

    @Nonnull
    private List<MessageRecord> getRecentHistoryRecords(@Nonnull BaniraBot bot, @Nonnull BaniraCodeContext ctx) {
        MessageRecordQueryParam msgRecordParam = new MessageRecordQueryParam(true, 1, cfg.reply().historyLimit());
        applyScope(msgRecordParam, bot, ctx);
        msgRecordParam.addOrderBy(MessageRecordQueryParam.ORDER_ID, false);
        return messageRecordManager.getMessageRecordList(msgRecordParam);
    }

    private void addQuotedNeighborhood(@Nonnull List<MessageRecord> records
            , @Nonnull BaniraBot bot
            , @Nonnull BaniraCodeContext ctx
    ) {
        Integer replyId = quotedMessageId(ctx);
        if (replyId == null || replyId <= 0) {
            return;
        }
        MessageRecord anchor = quotedRecord(ctx, replyId);
        if (anchor == null || anchor.getId() == null || anchor.getId() <= 0) {
            return;
        }
        if (anchor.recalled()) {
            return;
        }
        records.add(anchor);
        records.addAll(neighborRecords(bot, ctx, anchor.getId(), true, QUOTED_CONTEXT_BEFORE));
        records.addAll(neighborRecords(bot, ctx, anchor.getId(), false, QUOTED_CONTEXT_AFTER));
    }

    @Nonnull
    private List<MessageRecord> neighborRecords(@Nonnull BaniraBot bot
            , @Nonnull BaniraCodeContext ctx
            , long anchorId
            , boolean before
            , int limit
    ) {
        MessageRecordQueryParam param = new MessageRecordQueryParam(true, 1, limit);
        applyScope(param, bot, ctx);
        if (before) {
            param.setIdByLt(anchorId);
            param.addOrderBy(MessageRecordQueryParam.ORDER_ID, false);
        } else {
            param.setIdByGt(anchorId);
            param.addOrderBy(MessageRecordQueryParam.ORDER_ID, true);
        }
        return messageRecordManager.getMessageRecordList(param);
    }

    private void applyScope(@Nonnull MessageRecordQueryParam param, @Nonnull BaniraBot bot, @Nonnull BaniraCodeContext ctx) {
        param.setBotId(bot.getSelfId());
        param.setMsgType(ctx.msgType().name());
        param.setRecalled(false);
        if (ctx.msgType() == EnumMessageType.GROUP) {
            param.setGroupId(ctx.group());
        } else if (ctx.msgType() == EnumMessageType.MEMBER) {
            param.setGroupId(ctx.group());
            param.setTargetId(ctx.sender());
        } else {
            param.setTargetId(ctx.sender());
        }
    }

    @Nullable
    private MessageRecord quotedRecord(@Nonnull BaniraCodeContext ctx, int replyId) {
        if (ctx.msgType() == EnumMessageType.GROUP && ctx.group() != null && ctx.group() > 0) {
            MessageRecord record = messageRecordManager.getGroupMessageRecord(ctx.group(), replyId);
            return record != null && !record.recalled() ? record : null;
        }
        MessageRecord record = messageRecordManager.getPrivateMessageRecord(ctx.sender() != null ? ctx.sender() : 0L, replyId);
        return record != null && !record.recalled() ? record : null;
    }

    @Nullable
    private static Integer quotedMessageId(@Nonnull BaniraCodeContext ctx) {
        if (ctx.originalMsg() == null) {
            return null;
        }
        for (ArrayMsg msg : ctx.originalMsg()) {
            if (msg != null && msg.getType() == com.mikuac.shiro.enums.MsgTypeEnum.reply) {
                long id = msg.getLongData("id");
                if (id > 0 && id <= Integer.MAX_VALUE) {
                    return (int) id;
                }
            }
        }
        return null;
    }

    @Nonnull
    private List<MessageRecord> normalize(List<MessageRecord> records, BaniraCodeContext ctx) {
        if (records == null || records.isEmpty()) {
            return Collections.emptyList();
        }
        List<MessageRecord> cleaned = new ArrayList<>();
        for (MessageRecord record : records) {
            if (record == null || record.recalled() || StringUtils.isNullOrEmptyEx(recordTextSource(record))) {
                continue;
            }
            if (isCurrentMessageRecord(record, ctx)) {
                continue;
            }
            cleaned.add(record);
        }
        cleaned.sort(Comparator
                .comparing(MessageRecord::getTime, Comparator.nullsLast(Long::compareTo))
                .thenComparing(MessageRecord::getId, Comparator.nullsLast(Long::compareTo)));
        cleaned = deduplicate(cleaned);
        int limit = Math.max(cfg.reply().historyLimit(),
                Math.min(cleaned.size(), cfg.reply().historyLimit() + QUOTED_CONTEXT_BEFORE + QUOTED_CONTEXT_AFTER + 1));
        if (cleaned.size() > limit) {
            return cleaned.subList(cleaned.size() - limit, cleaned.size());
        }
        return cleaned;
    }

    @Nonnull
    private static List<MessageRecord> deduplicate(@Nonnull List<MessageRecord> records) {
        Map<String, MessageRecord> unique = new LinkedHashMap<>();
        for (MessageRecord record : records) {
            String key = record.getId() != null && record.getId() > 0
                    ? "id:" + record.getId()
                    : "msg:" + StringUtils.nullToEmpty(record.getMsgId());
            unique.putIfAbsent(key, record);
        }
        return new ArrayList<>(unique.values());
    }

    @Nonnull
    static String recordTextSource(@Nonnull MessageRecord record) {
        String recode = StringUtils.nullToEmpty(record.getMsgRecode()).trim();
        if (StringUtils.isNotNullOrEmpty(recode)) {
            return recode;
        }
        return StringUtils.nullToEmpty(record.getMsgRaw()).trim();
    }

    private static boolean isCurrentMessageRecord(MessageRecord record, BaniraCodeContext ctx) {
        if (record.getMsgId() == null || ctx.msgId() == null) {
            return false;
        }
        return Objects.equals(record.getMsgId(), String.valueOf(ctx.msgId()));
    }
}
