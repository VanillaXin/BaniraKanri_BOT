package xin.vanilla.banira.plugin.chat;

import lombok.extern.slf4j.Slf4j;
import xin.vanilla.banira.config.entity.extended.ChatAffinitySettings;
import xin.vanilla.banira.config.entity.extended.ChatReplySettings;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.StringUtils;

import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.util.Deque;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedDeque;
import java.util.concurrent.ThreadLocalRandom;

/**
 * 决策器：是否回复（按发送者限频）
 */
@Slf4j
public class ReplyDecisionMaker {

    private final ChatReplySettings cfg;
    private final ChatAffinitySettings affinityCfg;
    private final Clock clock;
    private final ConcurrentHashMap<Long, UserReplyState> userStates = new ConcurrentHashMap<>();

    public ReplyDecisionMaker(ChatReplySettings cfg) {
        this(cfg, new ChatAffinitySettings(), Clock.systemUTC());
    }

    public ReplyDecisionMaker(ChatReplySettings cfg, ChatAffinitySettings affinityCfg) {
        this(cfg, affinityCfg, Clock.systemUTC());
    }

    public ReplyDecisionMaker(ChatReplySettings cfg, ChatAffinitySettings affinityCfg, Clock clock) {
        this.cfg = Objects.requireNonNull(cfg, "cfg");
        this.affinityCfg = Objects.requireNonNull(affinityCfg, "affinityCfg");
        this.clock = Objects.requireNonNull(clock, "clock");
    }

    public boolean shouldReply(BaniraCodeContext ctx, boolean isMentioned, boolean isNameMentioned, int affinityScore) {
        if (ctx == null || StringUtils.isNullOrEmptyEx(ctx.msg())) {
            return false;
        }
        if (!passesRateLimit(ctx, isMentioned)) {
            return false;
        }
        double p = computeReplyProbability(ctx, isMentioned, isNameMentioned, affinityScore,
                userStates.computeIfAbsent(ctx.sender(), id -> new UserReplyState()));
        double roll = ThreadLocalRandom.current().nextDouble();
        boolean decision = roll <= p;
        LOGGER.debug("decide reply sender={} p={} roll={} => {}", ctx.sender(),
                String.format("%.4f", p), String.format("%.4f", roll), decision);
        if (!decision) {
            return false;
        }
        markReplySent(ctx.sender());
        return true;
    }

    /**
     * 仅检查并占用回复频控名额（供 LLM 决策模式下实际发送前使用）。
     */
    public boolean tryAcquireReplySlot(BaniraCodeContext ctx, boolean bypassCooldown) {
        if (ctx == null || ctx.sender() <= 0) {
            return false;
        }
        if (!passesRateLimit(ctx, bypassCooldown)) {
            return false;
        }
        markReplySent(ctx.sender());
        return true;
    }

    private boolean passesRateLimit(BaniraCodeContext ctx, boolean bypassCooldown) {
        long senderId = ctx.sender();
        UserReplyState state = userStates.computeIfAbsent(senderId, id -> new UserReplyState());
        final Instant now = clock.instant();

        long elapsedSinceLast = Duration.between(state.lastReply, now).getSeconds();
        if (!bypassCooldown && elapsedSinceLast < cfg.perTargetCooldownSeconds()) {
            LOGGER.debug("skip reply due per-user cooldown sender={} elapsed={}s need={}s",
                    senderId, elapsedSinceLast, cfg.perTargetCooldownSeconds());
            return false;
        }

        int limitPerMinute = cfg.perTargetRateLimitPerMinute();
        if (bypassCooldown) {
            limitPerMinute = Math.max(limitPerMinute, limitPerMinute * 3);
        }
        cleanOldTimestamps(state, now);
        synchronized (state.recentTimestamps) {
            if (state.recentTimestamps.size() >= limitPerMinute) {
                LOGGER.warn("per-user rate limit hit: sender={} {}/min", senderId, limitPerMinute);
                return false;
            }
        }
        return true;
    }

    private void markReplySent(long senderId) {
        UserReplyState state = userStates.computeIfAbsent(senderId, id -> new UserReplyState());
        final Instant now = clock.instant();
        synchronized (state.recentTimestamps) {
            cleanOldTimestamps(state, now);
            state.recentTimestamps.addLast(now);
        }
        state.lastReply = now;
    }

    private void cleanOldTimestamps(UserReplyState state, Instant now) {
        Instant cutoff = now.minusSeconds(60);
        synchronized (state.recentTimestamps) {
            while (!state.recentTimestamps.isEmpty()) {
                Instant first = state.recentTimestamps.peekFirst();
                if (first == null || first.isBefore(cutoff)) {
                    state.recentTimestamps.pollFirst();
                } else {
                    break;
                }
            }
        }
    }

    double computeReplyProbabilityForTest(BaniraCodeContext ctx, boolean isMentioned, boolean isNameMentioned, int affinityScore) {
        UserReplyState state = userStates.computeIfAbsent(ctx.sender(), id -> new UserReplyState());
        return computeReplyProbability(ctx, isMentioned, isNameMentioned, affinityScore, state);
    }

    private double computeReplyProbability(BaniraCodeContext ctx, boolean isMentioned, boolean isNameMentioned, int affinityScore, UserReplyState state) {
        double p = cfg.baseReplyProbability();

        String msg = ctx.msg() == null ? "" : ctx.msg();
        int len = msg.length();
        boolean question = isQuestion(msg);
        boolean owner = BaniraUtils.isOwner(ctx.sender());
        boolean asksQuota = asksQuota(msg);

        if (isMentioned) {
            return 1.0;
        }
        if (isNameMentioned) {
            p = Math.max(p, cfg.nameMentionReplyProbability());
        }
        if (!isNameMentioned && isLowInformationMessage(msg)) {
            p = 0.0;
        } else if (question || asksQuota) {
            p = Math.min(1.0, p + 0.10);
        }
        if (!isNameMentioned && len < 12 && !question && !asksQuota) {
            p = Math.max(0.0, p - 0.06);
        }
        if (len > 400) {
            p = Math.max(0.02, p - 0.25);
        }

        int limit = cfg.perTargetRateLimitPerMinute();
        if (limit <= 0) {
            limit = 1;
        }
        int used;
        synchronized (state.recentTimestamps) {
            cleanOldTimestamps(state, clock.instant());
            used = state.recentTimestamps.size();
        }
        double ratio = (double) used / (double) limit;
        final double alpha = 5.0;
        double scale = 1.0 / (1.0 + alpha * ratio);
        if (ratio > 1.0) {
            scale *= 0.5;
        }

        double floor = 0.0;
        if (owner && (question || asksQuota || isMentioned || isNameMentioned)) {
            floor = 0.95;
        } else if (isMentioned) {
            floor = cfg.directMentionReplyProbability();
        } else if (isNameMentioned) {
            floor = cfg.nameMentionReplyProbability();
        } else if (question || asksQuota) {
            floor = 0.18;
        }

        double finalP = Math.max(p, floor) * scale * affinityMultiplier(affinityScore);
        return Math.clamp(finalP, 0.0, 1.0);
    }

    public int currentWindowCount(long senderId) {
        UserReplyState state = userStates.get(senderId);
        if (state == null) {
            return 0;
        }
        synchronized (state.recentTimestamps) {
            return state.recentTimestamps.size();
        }
    }

    private boolean isQuestion(String msg) {
        if (msg == null) {
            return false;
        }
        String trimmed = msg.trim();
        return trimmed.endsWith("?") || trimmed.endsWith("？") || trimmed.contains("吗")
                || trimmed.toLowerCase().contains("what") || trimmed.toLowerCase().contains("how");
    }

    private boolean isLowInformationMessage(String msg) {
        if (msg == null) {
            return true;
        }
        String normalized = msg
                .replaceAll("\\[CQ:[^]]+]", "")
                .replaceAll("[\\p{Punct}！？。。，、~～…·\\s]", "")
                .trim()
                .toLowerCase();
        if (normalized.length() <= 1) {
            return true;
        }
        return normalized.matches("^(哈+|哈哈+|笑死|草+|艹+|乐+|绷+|确实|是|对|嗯+|啊+|哦+|6+|w+|www+|牛+|牛逼|离谱|好家伙|什么鬼)$");
    }

    private boolean asksQuota(String msg) {
        if (msg == null) {
            return false;
        }
        String lower = msg.toLowerCase();
        return lower.contains("额度")
                || lower.contains("余额")
                || lower.contains("quota")
                || lower.contains("balance")
                || lower.contains("api 用量")
                || lower.contains("api用量");
    }

    private double affinityMultiplier(int affinityScore) {
        if (!affinityCfg.enabled()) {
            return 1.0;
        }
        if (affinityScore <= affinityCfg.veryLowThreshold()) {
            return affinityCfg.veryLowReplyMultiplier();
        }
        if (affinityScore <= affinityCfg.lowThreshold()) {
            return affinityCfg.lowReplyMultiplier();
        }
        return 1.0;
    }

    private static final class UserReplyState {
        private Instant lastReply = Instant.EPOCH;
        private final Deque<Instant> recentTimestamps = new ConcurrentLinkedDeque<>();
    }

}
