package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.enums.EnumMessageType;
import xin.vanilla.banira.plugin.chat.agent.AgentContext;
import xin.vanilla.banira.plugin.common.BaniraBot;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Tracks the concrete QQ message ids produced by the latest AI reply per chat scope.
 */
public final class RecentAiReplyTracker {

    private static final ConcurrentHashMap<Key, List<Integer>> LAST_REPLY_IDS = new ConcurrentHashMap<>();

    private RecentAiReplyTracker() {
    }

    public static void remember(@Nonnull BaniraBot bot, @Nonnull BaniraCodeContext ctx, @Nonnull List<Integer> messageIds) {
        remember(bot.getSelfId(), ctx.msgType(), ctx.group(), ctx.sender(), messageIds);
    }

    static void remember(long botId, EnumMessageType msgType, Long groupId, Long targetId, @Nonnull List<Integer> messageIds) {
        List<Integer> normalized = messageIds.stream()
                .filter(id -> id != null && id > 0)
                .toList();
        if (normalized.isEmpty()) {
            return;
        }
        LAST_REPLY_IDS.put(Key.from(botId, msgType, groupId, targetId), normalized);
    }

    @Nonnull
    public static List<Integer> takeLast(@Nonnull AgentContext ctx, int count) {
        Key key = Key.from(ctx.botId(), ctx.msgType(), ctx.groupId(), ctx.senderId());
        List<Integer> ids = LAST_REPLY_IDS.remove(key);
        if (ids == null || ids.isEmpty()) {
            return Collections.emptyList();
        }
        if (count <= 0 || count >= ids.size()) {
            return ids;
        }
        return new ArrayList<>(ids.subList(ids.size() - count, ids.size()));
    }

    private record Key(long botId, EnumMessageType msgType, long groupId, long targetId) {

        @Nonnull
        static Key from(long botId, EnumMessageType msgType, Long groupId, Long targetId) {
            EnumMessageType type = msgType != null ? msgType : EnumMessageType.FRIEND;
            long scopeGroupId = type == EnumMessageType.GROUP && groupId != null ? groupId : 0L;
            long scopeTargetId = type == EnumMessageType.GROUP ? 0L : (targetId != null ? targetId : 0L);
            return new Key(botId, type, scopeGroupId, scopeTargetId);
        }
    }
}
