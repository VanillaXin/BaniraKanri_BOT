package xin.vanilla.banira.plugin.chat;

import lombok.extern.slf4j.Slf4j;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.util.StringUtils;

import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.util.Deque;
import java.util.Objects;
import java.util.concurrent.ConcurrentLinkedDeque;
import java.util.concurrent.ThreadLocalRandom;

/**
 * 决策器：是否回复
 */
@Slf4j
public class ReplyDecisionMaker {

    private final ChatConfig cfg;
    private final Clock clock;

    // 最近回复时间
    private Instant lastReply = Instant.EPOCH;

    // 最近 60 秒内的回复时间戳（用于限制每实例每分钟上限）
    private final Deque<Instant> recentTimestamps = new ConcurrentLinkedDeque<>();

    public ReplyDecisionMaker(ChatConfig cfg) {
        this(cfg, Clock.systemUTC());
    }

    public ReplyDecisionMaker(ChatConfig cfg, Clock clock) {
        this.cfg = Objects.requireNonNull(cfg, "cfg");
        this.clock = Objects.requireNonNull(clock, "clock");
    }

    public boolean shouldReply(BaniraCodeContext ctx, boolean isMentioned) {
        if (ctx == null || StringUtils.isNullOrEmptyEx(ctx.msg())) {
            return false;
        }
        if (!cfg.enabled()) {
            LOGGER.debug("reply disabled in config");
            return false;
        }

        final Instant now = clock.instant();

        // per-instance cooldown（避免刷屏）
        long elapsedSinceLast = Duration.between(lastReply, now).getSeconds();
        if (elapsedSinceLast < cfg.perTargetCooldownSeconds()) {
            LOGGER.debug("skip reply due per-instance cooldown elapsed={}s need={}s",
                    elapsedSinceLast, cfg.perTargetCooldownSeconds());
            return false;
        }

        // 滑动窗口：清理 60 秒之前的时间戳，并检查当前窗口是否已满
        final int limitPerMinute = cfg.perTargetRateLimitPerMinute();
        cleanOldTimestamps(now);
        synchronized (recentTimestamps) {
            if (recentTimestamps.size() >= limitPerMinute) {
                LOGGER.warn("per-target rate limit hit: {}/min", limitPerMinute);
                return false;
            }
        }

        // 计算概率并 roll（纯逻辑）
        double p = computeReplyProbability(ctx, isMentioned);
        double roll = ThreadLocalRandom.current().nextDouble();
        boolean decision = roll <= p;
        LOGGER.debug("decide reply p={} roll={} => {}", String.format("%.4f", p),
                String.format("%.4f", roll), decision);

        if (!decision) {
            return false;
        }

        // 命中：记录时间戳并更新 lastReply
        synchronized (recentTimestamps) {
            // 再次清理以减少 race 导致的超额（防守式）
            cleanOldTimestamps(now);
            if (recentTimestamps.size() >= limitPerMinute) {
                LOGGER.warn("per-target rate limit hit after race-check: {}/min", limitPerMinute);
                return false;
            }
            recentTimestamps.addLast(now);
        }
        lastReply = now;
        return true;
    }

    /**
     * 清理 recentTimestamps 中超过 60 秒的旧条目
     */
    private void cleanOldTimestamps(Instant now) {
        Instant cutoff = now.minusSeconds(60);
        synchronized (recentTimestamps) {
            while (!recentTimestamps.isEmpty()) {
                Instant first = recentTimestamps.peekFirst();
                if (first == null || first.isBefore(cutoff)) {
                    recentTimestamps.pollFirst();
                } else {
                    break;
                }
            }
        }
    }

    /**
     * 改进后的概率计算：考虑配额占用比例（used/limit），并用平滑函数惩罚
     */
    private double computeReplyProbability(BaniraCodeContext ctx, boolean isMentioned) {
        // 基础概率
        double p = cfg.baseReplyProbability();

        String msg = ctx == null || ctx.msg() == null ? "" : ctx.msg();
        int len = msg.length();
        boolean question = isQuestion(msg);

        // 直接被点名/回复时应明显提升
        if (isMentioned) p = Math.min(1.0, p + cfg.mentionBoost() + 0.35);
        if (question) p = Math.min(1.0, p + 0.20);

        // 简短问候类消息略加权，极长内容降低触发
        if (len < 12) p = Math.min(1.0, p + 0.15);
        if (len > 400) p = Math.max(0.02, p - 0.25);

        // 基于配额占用的缩放因子
        int limit = cfg.perTargetRateLimitPerMinute();
        int used;
        synchronized (recentTimestamps) {
            cleanOldTimestamps(clock.instant()); // 保证 recentTimestamps 最近状态
            used = recentTimestamps.size();
        }

        if (limit <= 0) limit = 1;
        double ratio = (double) used / (double) limit; // 0..inf，越接近 1 表示配额快用完

        // 平滑惩罚函数：scale = 1 / (1 + alpha * ratio)
        // alpha 控制敏感度（越大惩罚越强）
        final double alpha = 5.0; // 可调：5 为中等惩罚
        double scale = 1.0 / (1.0 + alpha * ratio);

        // 对超高占用（ratio > 1）采用更强的惩罚（避免瞬间超额导致仍然高概率）
        if (ratio > 1.0) {
            scale *= 0.5; // 进一步减半
        }

        // 最终结合
        double floor = 0.0;
        if (isMentioned) floor = 0.70;
        else if (question) floor = 0.45;

        double finalP = Math.max(p, floor) * scale;

        // clamp
        if (finalP < 0.0) finalP = 0.0;
        if (finalP > 1.0) finalP = 1.0;
        return finalP;
    }

    // 返回最近窗口内已使用的回复数
    public int currentWindowCount() {
        synchronized (recentTimestamps) {
            return recentTimestamps.size();
        }
    }

    private boolean isQuestion(String msg) {
        if (msg == null) return false;
        String trimmed = msg.trim();
        return trimmed.endsWith("?") || trimmed.endsWith("？") || trimmed.contains("吗") || trimmed.toLowerCase().contains("what") || trimmed.toLowerCase().contains("how");
    }
}
