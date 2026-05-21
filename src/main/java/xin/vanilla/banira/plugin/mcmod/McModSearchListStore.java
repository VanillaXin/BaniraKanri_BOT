package xin.vanilla.banira.plugin.mcmod;

import jakarta.annotation.PreDestroy;
import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

/**
 * MCMod 搜索列表会话存储
 */
@Component
public class McModSearchListStore {

    private final Map<Long, McModSearchListSession> sessions = new ConcurrentHashMap<>();
    private final ScheduledExecutorService cleanupExecutor = Executors.newSingleThreadScheduledExecutor(r -> {
        Thread thread = new Thread(r, "mcmod-search-list-cleanup");
        thread.setDaemon(true);
        return thread;
    });

    public McModSearchListStore() {
        cleanupExecutor.scheduleAtFixedRate(this::cleanupExpired, 30, 30, TimeUnit.SECONDS);
    }

    public void save(McModSearchListSession session) {
        if (session == null || session.messageId() == null || session.messageId() <= 0) {
            return;
        }
        sessions.put(session.messageId(), session);
    }

    public Optional<McModSearchListSession> findByMessageId(Long messageId) {
        if (messageId == null || messageId <= 0) {
            return Optional.empty();
        }
        McModSearchListSession session = sessions.get(messageId);
        if (session == null) {
            return Optional.empty();
        }
        if (session.expired()) {
            sessions.remove(messageId);
            return Optional.empty();
        }
        return Optional.of(session);
    }

    @PreDestroy
    public void shutdown() {
        cleanupExecutor.shutdown();
    }

    private void cleanupExpired() {
        sessions.entrySet().removeIf(entry -> entry.getValue() == null || entry.getValue().expired());
    }

}
