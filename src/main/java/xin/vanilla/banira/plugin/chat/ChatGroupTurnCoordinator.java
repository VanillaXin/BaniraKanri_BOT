package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.config.entity.extended.ChatEngagementSettings;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.enums.EnumMessageType;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 同群 LLM 单飞合并：进行中的回合若被新消息打断，则合并重跑，避免各说各话。
 */
@Slf4j
@Component
public class ChatGroupTurnCoordinator {

    private final ConcurrentHashMap<String, GroupTurnState> states = new ConcurrentHashMap<>();

    @Nonnull
    public ChatTurnCoalesceOutcome run(@Nonnull BaniraBot bot
            , @Nonnull BaniraCodeContext ctx
            , @Nonnull ChatConfig cfg
            , @Nonnull ChatTurnCoalesceRunner runner
    ) {
        if (!shouldCoalesce(ctx, cfg)) {
            StructuredReply reply = runner.run(bot, ctx, ChatTurnCoalesceState.none());
            return ChatTurnCoalesceOutcome.completed(reply, ctx);
        }

        String key = scopeKey(bot.getSelfId(), ctx);
        GroupTurnState state = states.computeIfAbsent(key, ignored -> new GroupTurnState());
        synchronized (state) {
            boolean hadOtherLeader = state.leaderThread != null && state.leaderThread != Thread.currentThread();
            state.register(ctx);
            if (hadOtherLeader) {
                state.superseded = true;
                LOGGER.debug("turn coalesce follower bot={} group={} msgId={} pending={}",
                        bot.getSelfId(), ctx.group(), ctx.msgId(), state.buffer.size());
                return ChatTurnCoalesceOutcome.follower();
            }
            state.leaderThread = Thread.currentThread();
        }

        try {
            int attempt = 0;
            StructuredReply lastReply = null;
            BaniraCodeContext lastReplyContext = null;
            while (true) {
                BaniraCodeContext currentCtx;
                List<BaniraCodeContext> supplementary;
                long generation;
                synchronized (state) {
                    state.superseded = false;
                    currentCtx = state.latest();
                    supplementary = state.supplementaryBeforeLatest();
                    generation = state.generation;
                }
                if (attempt > 0) {
                    sleep(cfg.engagement().coalesceRetryDelayMillis());
                }
                ChatTurnCoalesceState coalesceState = new ChatTurnCoalesceState(attempt, supplementary);
                StructuredReply reply = runner.run(bot, currentCtx, coalesceState);
                if (reply != null) {
                    lastReply = reply;
                    lastReplyContext = currentCtx;
                }
                synchronized (state) {
                    if (state.superseded || state.generation != generation) {
                        attempt++;
                        LOGGER.debug("turn coalesce retry bot={} group={} attempt={} pending={}",
                                bot.getSelfId(), ctx.group(), attempt, state.buffer.size());
                        continue;
                    }
                    BaniraCodeContext replyContext = reply != null
                            ? currentCtx
                            : lastReplyContext != null ? lastReplyContext : state.latest();
                    StructuredReply finalReply = reply != null ? reply : lastReply;
                    if (reply == null && lastReply != null) {
                        LOGGER.debug("turn coalesce kept previous reply bot={} group={} attempt={} latestMsgId={}",
                                bot.getSelfId(), ctx.group(), attempt, currentCtx.msgId());
                    }
                    state.reset();
                    state.leaderThread = null;
                    states.remove(key, state);
                    return ChatTurnCoalesceOutcome.completed(finalReply, replyContext);
                }
            }
        } finally {
            synchronized (state) {
                if (state.leaderThread == Thread.currentThread()) {
                    state.leaderThread = null;
                    if (state.buffer.isEmpty()) {
                        states.remove(key, state);
                    }
                }
            }
        }
    }

    private static boolean shouldCoalesce(@Nonnull BaniraCodeContext ctx, @Nonnull ChatConfig cfg) {
        ChatEngagementSettings settings = cfg.engagement();
        if (settings == null || !settings.enabled() || !settings.turnCoalescingEnabled()) {
            return false;
        }
        return ctx.msgType() == EnumMessageType.GROUP && BaniraUtils.isGroupIdValid(ctx.group());
    }

    @Nonnull
    private static String scopeKey(long botId, @Nonnull BaniraCodeContext ctx) {
        return botId + ":" + ctx.group();
    }

    private static void sleep(long millis) {
        if (millis <= 0) {
            return;
        }
        try {
            Thread.sleep(millis);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

    private static final class GroupTurnState {
        private final List<BaniraCodeContext> buffer = new ArrayList<>();
        private long generation;
        private boolean superseded;
        private Thread leaderThread;

        void register(@Nonnull BaniraCodeContext ctx) {
            if (containsMessage(ctx)) {
                return;
            }
            buffer.add(ctx);
            generation++;
        }

        boolean containsMessage(@Nonnull BaniraCodeContext ctx) {
            if (ctx.msgId() == null) {
                return false;
            }
            for (BaniraCodeContext existing : buffer) {
                if (existing.msgId() != null && Objects.equals(existing.msgId(), ctx.msgId())) {
                    return true;
                }
            }
            return false;
        }

        @Nonnull
        BaniraCodeContext latest() {
            return buffer.getLast();
        }

        @Nonnull
        List<BaniraCodeContext> supplementaryBeforeLatest() {
            if (buffer.size() <= 1) {
                return List.of();
            }
            return List.copyOf(buffer.subList(0, buffer.size() - 1));
        }

        void reset() {
            buffer.clear();
            generation = 0;
            superseded = false;
        }
    }
}
