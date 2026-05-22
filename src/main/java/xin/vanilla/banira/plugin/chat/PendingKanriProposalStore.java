package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import xin.vanilla.banira.plugin.chat.agent.AgentContext;

import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 机器人口头提出、等待用户确认的群管提议（按群隔离）。
 */
public final class PendingKanriProposalStore {

    private static final Duration TTL = Duration.ofSeconds(300);
    private static final ConcurrentHashMap<Key, KanriProposal> PROPOSALS = new ConcurrentHashMap<>();

    private PendingKanriProposalStore() {
    }

    public static void putMute(long botId, long groupId, @Nonnull List<String> targetKeywords, int minutes) {
        if (groupId <= 0 || targetKeywords.isEmpty()) {
            return;
        }
        cleanup();
        PROPOSALS.put(new Key(botId, groupId), new KanriProposal(
                List.copyOf(targetKeywords),
                Math.max(1, minutes),
                Instant.now()
        ));
    }

    public static void putMute(@Nonnull AgentContext ctx, @Nonnull List<String> targetKeywords, int minutes) {
        long groupId = ctx.groupId() != null ? ctx.groupId() : 0L;
        putMute(ctx.botId(), groupId, targetKeywords, minutes);
    }

    public static boolean hasMute(long botId, long groupId) {
        cleanup();
        KanriProposal proposal = PROPOSALS.get(new Key(botId, groupId));
        return proposal != null && !proposal.expired();
    }

    @Nullable
    public static KanriProposal peek(long botId, long groupId) {
        cleanup();
        Key key = new Key(botId, groupId);
        KanriProposal proposal = PROPOSALS.get(key);
        if (proposal == null || proposal.expired()) {
            PROPOSALS.remove(key);
            return null;
        }
        return proposal;
    }

    @Nullable
    public static KanriProposal consume(long botId, long groupId) {
        cleanup();
        Key key = new Key(botId, groupId);
        KanriProposal proposal = PROPOSALS.get(key);
        if (proposal == null || proposal.expired()) {
            PROPOSALS.remove(key);
            return null;
        }
        PROPOSALS.remove(key);
        return proposal;
    }

    private static void cleanup() {
        PROPOSALS.entrySet().removeIf(entry -> entry.getValue().expired());
    }

    public record KanriProposal(@Nonnull List<String> targetKeywords, int minutes, @Nonnull Instant createdAt) {
        boolean expired() {
            return createdAt.plus(TTL).isBefore(Instant.now());
        }
    }

    private record Key(long botId, long groupId) {
    }
}
