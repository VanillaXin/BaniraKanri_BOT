package xin.vanilla.banira.plugin.chat.capability;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import xin.vanilla.banira.enums.EnumMessageType;
import xin.vanilla.banira.plugin.chat.agent.AgentContext;
import xin.vanilla.banira.util.StringUtils;

import java.time.Duration;
import java.time.Instant;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Short-lived confirmation state for sensitive AI actions.
 */
public final class PendingAiActionStore {

    private static final Duration TTL = Duration.ofSeconds(120);
    private static final ConcurrentHashMap<Key, PendingAction> ACTIONS = new ConcurrentHashMap<>();

    private PendingAiActionStore() {
    }

    public static void put(@Nonnull AgentContext ctx, @Nonnull String capabilityName, @Nonnull Map<String, String> args) {
        cleanup();
        ACTIONS.put(Key.from(ctx), new PendingAction(normalize(capabilityName), Map.copyOf(args), ctx.msgId(), Instant.now()));
    }

    @Nullable
    public static PendingAction consumeMatching(@Nonnull AgentContext ctx, @Nonnull String capabilityName) {
        cleanup();
        Key key = Key.from(ctx);
        PendingAction action = ACTIONS.get(key);
        if (action == null || action.expired()) {
            ACTIONS.remove(key);
            return null;
        }
        if (!action.capabilityName().equals(normalize(capabilityName))) {
            return null;
        }
        ACTIONS.remove(key);
        return action;
    }

    public static boolean isConfirmationText(@Nullable String text) {
        if (StringUtils.isNullOrEmptyEx(text)) {
            return false;
        }
        String normalized = text.replaceAll("\\[CQ:[^]]+]", " ")
                .replaceAll("[\\p{Punct}！？。。，、~～…·\\s]", "")
                .trim()
                .toLowerCase(Locale.ROOT);
        return normalized.matches("^(确认|确定|可以|执行|就这样|是的|对|嗯|好|行|要|ok|yes|doit|do)$")
                || normalized.contains("确认执行")
                || normalized.contains("可以执行")
                || normalized.contains("就这样执行")
                || normalized.contains("你看着")
                || normalized.contains("看着来")
                || normalized.contains("看着办");
    }

    public static boolean isKanriAffirmationOrDelegation(@Nullable String text) {
        return isConfirmationText(text) || isKanriProceedIntent(text);
    }

    public static boolean isKanriProceedIntent(@Nullable String text) {
        if (StringUtils.isNullOrEmptyEx(text)) {
            return false;
        }
        if (isConfirmationText(text)) {
            return true;
        }
        return isKanriRetryIntent(text) || containsProceedVerb(normalizeComparable(text));
    }

    public static boolean isKanriRetryIntent(@Nullable String text) {
        if (StringUtils.isNullOrEmptyEx(text)) {
            return false;
        }
        String normalized = normalizeComparable(text);
        return normalized.contains("再试试")
                || normalized.contains("再试")
                || normalized.contains("试试")
                || normalized.contains("重来")
                || normalized.contains("再禁");
    }

    private static boolean containsProceedVerb(@Nonnull String normalized) {
        return normalized.contains("动手")
                || normalized.contains("赶紧去")
                || normalized.contains("快去")
                || normalized.contains("赶紧")
                || normalized.contains("快点")
                || normalized.contains("执行吧")
                || normalized.contains("去办")
                || normalized.contains("为什么不帮")
                || normalized.contains("怎么不帮")
                || normalized.contains("快禁")
                || normalized.contains("去禁")
                || normalized.matches(".*\\bdoit\\b.*")
                || normalized.contains("doit")
                || (normalized.contains("帮") && normalized.contains("禁"));
    }

    @Nonnull
    private static String normalizeComparable(@Nullable String text) {
        if (text == null) {
            return "";
        }
        return text.replaceAll("\\[CQ:[^]]+]", " ")
                .replaceAll("[\\p{Punct}！？。。，、~～…·\\s]", "")
                .trim()
                .toLowerCase(Locale.ROOT);
    }

    private static void cleanup() {
        ACTIONS.entrySet().removeIf(entry -> entry.getValue().expired());
    }

    @Nonnull
    private static String normalize(@Nullable String value) {
        return value == null ? "" : value.trim().toLowerCase(Locale.ROOT);
    }

    private record Key(long botId, long groupId, long userId) {
        @Nonnull
        static Key from(@Nonnull AgentContext ctx) {
            long botId = ctx.bot() != null ? ctx.botId() : 0L;
            long groupId = ctx.msgType() == EnumMessageType.GROUP && ctx.groupId() != null ? ctx.groupId() : 0L;
            long userId = ctx.senderId() != null ? ctx.senderId() : 0L;
            return new Key(botId, groupId, userId);
        }
    }

    public record PendingAction(@Nonnull String capabilityName
            , @Nonnull Map<String, String> args
            , @Nonnull String sourceMsgId
            , @Nonnull Instant createdAt
    ) {
        boolean expired() {
            return createdAt.plus(TTL).isBefore(Instant.now());
        }
    }
}
