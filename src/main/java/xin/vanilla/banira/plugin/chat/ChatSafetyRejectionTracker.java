package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.util.StringUtils;

import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Tracks messages that recently caused provider-side safety rejection so they do not poison later prompts.
 */
public final class ChatSafetyRejectionTracker {

    private static final long DEFAULT_TTL_MILLIS = 10 * 60 * 1000L;
    private static final Map<String, Long> REJECTED_MESSAGE_KEYS = new ConcurrentHashMap<>();

    private ChatSafetyRejectionTracker() {
    }

    public static boolean looksLikeProviderSafetyRejection(@Nullable Throwable throwable) {
        if (throwable == null) {
            return false;
        }
        Throwable current = throwable;
        int depth = 0;
        while (current != null && depth++ < 8) {
            if (looksLikeProviderSafetyText(current.getMessage())
                    || looksLikeProviderSafetyText(current.toString())) {
                return true;
            }
            current = current.getCause();
        }
        return false;
    }

    public static boolean looksLikeProviderSafetyText(@Nullable String text) {
        if (StringUtils.isNullOrEmptyEx(text)) {
            return false;
        }
        String lower = text.toLowerCase(Locale.ROOT);
        return lower.contains("the request was rejected because it was considered high risk")
                || lower.contains("considered high risk")
                || lower.contains("request was rejected")
                || lower.contains("content safety")
                || lower.contains("safety filter")
                || lower.contains("safety policy")
                || lower.contains("content policy")
                || lower.contains("内容安全过滤")
                || lower.contains("高风险")
                || lower.contains("安全策略")
                || lower.contains("安全审核");
    }

    public static void markPromptMessages(@Nonnull BaniraCodeContext ctx, @Nonnull List<MessageRecord> records) {
        long botId = ctx.bot() != null ? ctx.bot().getSelfId() : 0L;
        Long groupId = ctx.group();
        long expiresAt = System.currentTimeMillis() + DEFAULT_TTL_MILLIS;
        if (ctx.msgId() != null && ctx.msgId() > 0) {
            REJECTED_MESSAGE_KEYS.put(key(botId, groupId, String.valueOf(ctx.msgId())), expiresAt);
        }
        for (MessageRecord record : records) {
            if (record == null || StringUtils.isNullOrEmptyEx(record.getMsgId())) {
                continue;
            }
            long recordBotId = record.getBotId() != null ? record.getBotId() : botId;
            Long recordGroupId = record.getGroupId() != null ? record.getGroupId() : groupId;
            REJECTED_MESSAGE_KEYS.put(key(recordBotId, recordGroupId, record.getMsgId()), expiresAt);
        }
        cleanup();
    }

    public static boolean isRejectedHistoryMessage(long botId, @Nullable Long groupId, @Nullable String msgId) {
        if (StringUtils.isNullOrEmptyEx(msgId)) {
            return false;
        }
        cleanup();
        Long expiresAt = REJECTED_MESSAGE_KEYS.get(key(botId, groupId, msgId));
        if (expiresAt == null) {
            return false;
        }
        if (expiresAt < System.currentTimeMillis()) {
            REJECTED_MESSAGE_KEYS.remove(key(botId, groupId, msgId));
            return false;
        }
        return true;
    }

    static void clearForTest() {
        REJECTED_MESSAGE_KEYS.clear();
    }

    private static void cleanup() {
        long now = System.currentTimeMillis();
        Set<String> keys = REJECTED_MESSAGE_KEYS.keySet();
        for (String key : keys) {
            Long expiresAt = REJECTED_MESSAGE_KEYS.get(key);
            if (expiresAt == null || expiresAt < now) {
                REJECTED_MESSAGE_KEYS.remove(key);
            }
        }
    }

    @Nonnull
    private static String key(long botId, @Nullable Long groupId, @Nonnull String msgId) {
        return botId + ":" + (groupId != null ? groupId : 0L) + ":" + msgId.trim();
    }
}
