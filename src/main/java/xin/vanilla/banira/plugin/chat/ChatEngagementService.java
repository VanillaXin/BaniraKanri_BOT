package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.extended.ChatEngagementSettings;
import xin.vanilla.banira.domain.AiGroupEngagement;
import xin.vanilla.banira.service.IAiGroupEngagementManager;
import xin.vanilla.banira.util.DateUtils;

import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.util.Date;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 按群隔离的对话兴趣值：内存缓存 + 可选数据库持久化。
 */
@Slf4j
@Component
public class ChatEngagementService {

    private final IAiGroupEngagementManager engagementManager;
    private final Clock clock;
    private final ConcurrentHashMap<String, CachedEngagement> cache = new ConcurrentHashMap<>();

    @Autowired
    public ChatEngagementService(@Nonnull IAiGroupEngagementManager engagementManager) {
        this(engagementManager, Clock.systemUTC());
    }

    ChatEngagementService(@Nonnull IAiGroupEngagementManager engagementManager, @Nonnull Clock clock) {
        this.engagementManager = Objects.requireNonNull(engagementManager, "engagementManager");
        this.clock = Objects.requireNonNull(clock, "clock");
    }

    public boolean hasActiveFollow(long botId, long groupId, @Nonnull ChatEngagementSettings settings) {
        if (!settings.enabled() || groupId <= 0) {
            return false;
        }
        CachedEngagement state = resolveState(botId, groupId, settings);
        return state != null && state.interest() >= settings.followInterestThreshold();
    }

    public int currentInterest(long botId, long groupId, @Nonnull ChatEngagementSettings settings) {
        CachedEngagement state = resolveState(botId, groupId, settings);
        return state != null ? state.interest() : 0;
    }

    public void update(long botId, long groupId, int interest, @Nonnull ChatEngagementSettings settings) {
        if (groupId <= 0 || !settings.enabled()) {
            return;
        }
        int clamped = EngagementMeta.clampInterest(interest);
        String key = cacheKey(botId, groupId);
        if (clamped <= 0) {
            cache.remove(key);
            if (settings.persistenceEnabled()) {
                engagementManager.clear(botId, groupId);
            }
            return;
        }
        long updatedAt = nowMillis();
        cache.put(key, new CachedEngagement(clamped, updatedAt));
        if (settings.persistenceEnabled()) {
            engagementManager.saveInterest(botId, groupId, clamped);
        }
    }

    public void applyPassiveDecay(long botId, long groupId, boolean strongTrigger, @Nonnull ChatEngagementSettings settings) {
        if (!settings.enabled() || !settings.passiveDecayEnabled() || strongTrigger || groupId <= 0) {
            return;
        }
        int current = currentInterest(botId, groupId, settings);
        if (current <= 0) {
            return;
        }
        int delta = Math.max(1, settings.passiveDecayDelta());
        int next = Math.max(0, current - delta);
        LOGGER.debug("engagement passive decay bot={} group={} {} -> {}", botId, groupId, current, next);
        update(botId, groupId, next, settings);
    }

    public void decayIfExpired(long botId, long groupId, @Nonnull ChatEngagementSettings settings) {
        if (groupId <= 0) {
            return;
        }
        CachedEngagement state = cache.get(cacheKey(botId, groupId));
        if (state != null && isExpired(state.updatedAtMillis(), settings)) {
            clear(botId, groupId, settings);
            return;
        }
        if (settings.persistenceEnabled()) {
            AiGroupEngagement record = engagementManager.get(botId, groupId);
            if (record != null && record.getUpdatedAt() != null
                    && isExpired(record.getUpdatedAt(), settings)) {
                clear(botId, groupId, settings);
            }
        }
    }

    @Nullable
    private CachedEngagement resolveState(long botId, long groupId, @Nonnull ChatEngagementSettings settings) {
        if (groupId <= 0 || !settings.enabled()) {
            return null;
        }
        decayIfExpired(botId, groupId, settings);
        String key = cacheKey(botId, groupId);
        CachedEngagement cached = cache.get(key);
        if (cached != null && !isExpired(cached.updatedAtMillis(), settings)) {
            return cached;
        }
        if (settings.persistenceEnabled()) {
            AiGroupEngagement record = engagementManager.get(botId, groupId);
            if (record != null && record.getInterest() != null && record.getInterest() > 0
                    && record.getUpdatedAt() != null
                    && !isExpired(record.getUpdatedAt(), settings)) {
                CachedEngagement loaded = new CachedEngagement(record.getInterest(), record.getUpdatedAt());
                cache.put(key, loaded);
                return loaded;
            }
        }
        cache.remove(key);
        return null;
    }

    private void clear(long botId, long groupId, @Nonnull ChatEngagementSettings settings) {
        cache.remove(cacheKey(botId, groupId));
        if (settings.persistenceEnabled()) {
            engagementManager.clear(botId, groupId);
        }
    }

    private boolean isExpired(long updatedAtMillis, @Nonnull ChatEngagementSettings settings) {
        long ttlSeconds = Math.max(15, settings.followTtlSeconds());
        Instant updatedAt = Instant.ofEpochMilli(updatedAtMillis);
        return Duration.between(updatedAt, clock.instant()).getSeconds() > ttlSeconds;
    }

    private long nowMillis() {
        return DateUtils.getTimestamp(new Date(clock.millis()));
    }

    @Nonnull
    private static String cacheKey(long botId, long groupId) {
        return botId + ":" + groupId;
    }

    private record CachedEngagement(int interest, long updatedAtMillis) {
    }
}
