package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.plugin.chat.capability.PendingAiActionStore;
import xin.vanilla.banira.util.StringUtils;

import java.util.List;
import java.util.Locale;

/**
 * 从群聊历史推断「是否在等用户接话」等会话信号。
 */
public final class ChatConversationSignals {

    private ChatConversationSignals() {
    }

    public static boolean shouldBoostFollowInterest(@Nullable String speech) {
        return awaitsUserReply(speech);
    }

    public static boolean awaitsUserReply(@Nullable String speech) {
        if (StringUtils.isNullOrEmptyEx(speech)) {
            return false;
        }
        String normalized = normalizeComparable(speech);
        return normalized.contains("行吗")
                || normalized.contains("可以吗")
                || normalized.contains("行不")
                || normalized.contains("好吗")
                || normalized.contains("同意吗")
                || normalized.contains("要不要")
                || normalized.contains("禁言")
                && (normalized.contains("吗") || normalized.endsWith("?") || normalized.endsWith("？"));
    }

    public static boolean botAskedMuteDuration(@Nonnull List<MessageRecord> records, long botId) {
        String lastBotSpeech = findLastBotSpeech(records, botId);
        if (StringUtils.isNullOrEmptyEx(lastBotSpeech)) {
            return false;
        }
        String normalized = normalizeComparable(lastBotSpeech);
        return normalized.contains("禁多久")
                || normalized.contains("说个时长")
                || (normalized.contains("多久") && normalized.contains("禁"));
    }

    public static boolean isShortReplyToBotQuestion(@Nonnull List<MessageRecord> records
            , long botId
            , @Nullable String currentMessage
    ) {
        if (StringUtils.isNullOrEmptyEx(currentMessage)) {
            return false;
        }
        String compact = normalizeComparable(currentMessage);
        if (compact.length() > 48 && !PendingAiActionStore.isKanriProceedIntent(currentMessage)) {
            return false;
        }
        if (!PendingAiActionStore.isKanriProceedIntent(currentMessage) && !looksLikeShortAffirmation(compact)) {
            return false;
        }
        String lastBotSpeech = findLastBotSpeech(records, botId);
        return awaitsUserReply(lastBotSpeech);
    }

    @Nullable
    private static String findLastBotSpeech(@Nonnull List<MessageRecord> records, long botId) {
        for (int i = records.size() - 1; i >= 0; i--) {
            MessageRecord record = records.get(i);
            if (record == null || record.getSenderId() == null || record.getSenderId() != botId) {
                continue;
            }
            if (StringUtils.isNotNullOrEmpty(record.getMsgRecode())) {
                return record.getMsgRecode();
            }
        }
        return null;
    }

    private static boolean looksLikeShortAffirmation(@Nonnull String compact) {
        return compact.matches("^(yes|y|ok|sure|好|行|要|嗯|对|是|可以|同意|来吧|go)$");
    }

    @Nonnull
    private static String normalizeComparable(@Nullable String value) {
        if (value == null) {
            return "";
        }
        return value.replaceAll("\\[CQ:[^]]+]", " ")
                .replaceAll("[\\p{Punct}！？。。，、~～…·\\s]", "")
                .toLowerCase(Locale.ROOT)
                .trim();
    }
}
