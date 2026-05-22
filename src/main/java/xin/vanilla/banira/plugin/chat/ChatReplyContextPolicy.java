package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;
import xin.vanilla.banira.config.entity.extended.ChatReplySettings;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.enums.EnumMessageType;
import xin.vanilla.banira.util.StringUtils;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Small delivery-context decisions that should not depend on model behavior.
 */
public final class ChatReplyContextPolicy {

    private static final Pattern CQ_AT = Pattern.compile("\\[CQ:at,qq=(\\d+)]");

    private ChatReplyContextPolicy() {
    }

    @Nonnull
    public static StructuredReply applyBusyGroupQuote(@Nonnull BaniraCodeContext ctx
            , @Nonnull List<MessageRecord> history
            , @Nonnull StructuredReply reply
            , @Nonnull ChatReplySettings settings
    ) {
        if (!shouldQuoteCurrentMessage(ctx, history, reply, settings)) {
            return reply;
        }
        return new StructuredReply(
                reply.speech(),
                reply.references(),
                ctx.msgId(),
                reply.atTargets(),
                reply.directHandled()
        );
    }

    public static boolean shouldQuoteCurrentMessage(@Nonnull BaniraCodeContext ctx
            , @Nonnull List<MessageRecord> history
            , @Nonnull StructuredReply reply
            , @Nonnull ChatReplySettings settings
    ) {
        if (!settings.allowStructuredReply()
                || !settings.autoReplyInBusyGroup()
                || settings.busyGroupMessageThreshold() <= 0
                || ctx.msgType() != EnumMessageType.GROUP
                || ctx.msgId() == null
                || ctx.msgId() <= 0
                || reply.replyToMessageId() != null && reply.replyToMessageId() > 0
                || history.isEmpty()) {
            return false;
        }

        long currentTime = ctx.time() != null && ctx.time() > 0
                ? ctx.time()
                : System.currentTimeMillis() / 1000;
        long since = currentTime - Math.max(5L, settings.busyGroupWindowSeconds());
        int recentMessages = 0;
        Set<Long> senders = new HashSet<>();
        for (MessageRecord record : history) {
            if (record == null || record.getTime() == null || record.getTime() < since) {
                continue;
            }
            recentMessages++;
            if (record.getSenderId() != null && record.getSenderId() > 0) {
                senders.add(record.getSenderId());
            }
        }
        if (recentMessages < settings.busyGroupMessageThreshold()) {
            return false;
        }
        int distinctThreshold = settings.busyGroupDistinctSenderThreshold();
        return distinctThreshold <= 0 || senders.size() >= distinctThreshold;
    }

    public static boolean isDuplicateRecentBotReply(@Nonnull BaniraCodeContext ctx
            , @Nonnull List<MessageRecord> history
            , @Nonnull StructuredReply reply
            , long botId
    ) {
        if (StringUtils.isNullOrEmptyEx(reply.speech())
                || !reply.references().isEmpty()
                || asksForRepeatOrOldTopic(ctx.msg())) {
            return false;
        }
        String current = normalize(reply.speech());
        if (current.length() < 10) {
            return false;
        }
        for (MessageRecord record : history) {
            if (record == null
                    || record.getSenderId() == null
                    || record.getSenderId() != botId
                    || StringUtils.isNullOrEmptyEx(record.getMsgRecode())) {
                continue;
            }
            String previous = normalize(record.getMsgRecode());
            if (previous.length() < 10) {
                continue;
            }
            String shorter = current.length() <= previous.length() ? current : previous;
            String longer = current.length() > previous.length() ? current : previous;
            if (current.equals(previous) || shorter.length() >= 14 && longer.contains(shorter)) {
                return true;
            }
        }
        return false;
    }

    public static boolean isStaleIdentityAnswer(@Nonnull BaniraCodeContext ctx, @Nonnull StructuredReply reply) {
        if (StringUtils.isNullOrEmptyEx(reply.speech()) || asksIdentityQuestion(ctx.msg())) {
            return false;
        }
        String speech = normalize(reply.speech());
        if (speech.length() < 4) {
            return false;
        }
        return speech.matches(".*你是.{0,24}(主人|owner).*")
                || speech.matches(".*你就是.{0,24}(主人|owner).*")
                || speech.matches(".*你叫.{1,16}.*")
                || speech.matches(".*认得你.{0,16}.*")
                || speech.matches(".*认识你.{0,16}.*");
    }

    public static boolean isVacuousReplyToOtherAddressedMessage(@Nonnull BaniraCodeContext ctx
            , @Nonnull StructuredReply reply
            , long botId
    ) {
        if (StringUtils.isNullOrEmptyEx(ctx.msg())
                || StringUtils.isNullOrEmptyEx(reply.speech())
                || !reply.references().isEmpty()) {
            return false;
        }
        boolean mentionsOther = false;
        Matcher matcher = CQ_AT.matcher(ctx.msg());
        while (matcher.find()) {
            long target = Long.parseLong(matcher.group(1));
            if (target == botId) {
                return false;
            }
            mentionsOther = true;
        }
        if (!mentionsOther) {
            return false;
        }
        String speech = normalize(reply.speech());
        return speech.matches("^(收到|行|好|可以|嗯|知道了|明白|按这个来|那就按这个来|就这样|没问题)$")
                || speech.matches("^(收到|行|好|可以|嗯|知道了|明白).{0,8}(按这个来|就这样)$");
    }

    private static boolean asksForRepeatOrOldTopic(String text) {
        if (StringUtils.isNullOrEmptyEx(text)) {
            return false;
        }
        return text.contains("再说")
                || text.contains("再讲")
                || text.contains("再搜")
                || text.contains("再查")
                || text.contains("重搜")
                || text.contains("重新搜")
                || text.contains("重新查")
                || text.contains("核实")
                || text.contains("确认一下")
                || text.contains("重复")
                || text.contains("刚才")
                || text.contains("刚刚")
                || text.contains("之前")
                || text.contains("上面")
                || text.contains("那个")
                || text.contains("这个");
    }

    private static boolean asksIdentityQuestion(String text) {
        if (StringUtils.isNullOrEmptyEx(text)) {
            return false;
        }
        String normalized = normalize(text);
        return normalized.contains("我是谁")
                || normalized.contains("我叫什么")
                || normalized.contains("你知道我是谁")
                || normalized.contains("你认识我")
                || normalized.contains("你认得我")
                || normalized.contains("主人是谁")
                || normalized.contains("谁是主人");
    }

    @Nonnull
    private static String normalize(String text) {
        if (StringUtils.isNullOrEmptyEx(text)) {
            return "";
        }
        return text.replaceAll("\\[CQ:[^]]+]", " ")
                .replaceAll("[\\p{Punct}！？。。，、~～…·\\s]", "")
                .toLowerCase()
                .trim();
    }
}
