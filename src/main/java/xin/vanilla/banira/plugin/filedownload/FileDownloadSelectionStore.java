package xin.vanilla.banira.plugin.filedownload;

import jakarta.annotation.PreDestroy;
import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

/**
 * 下载选择会话存储
 */
@Component
public class FileDownloadSelectionStore {

    private final Map<Long, FileDownloadSelectionSession> sessions = new ConcurrentHashMap<>();
    private final ScheduledExecutorService cleanupExecutor = Executors.newSingleThreadScheduledExecutor(r -> {
        Thread thread = new Thread(r, "file-download-selection-cleanup");
        thread.setDaemon(true);
        return thread;
    });

    public FileDownloadSelectionStore() {
        cleanupExecutor.scheduleAtFixedRate(this::cleanupExpired, 30, 30, TimeUnit.SECONDS);
    }

    public void save(FileDownloadSelectionSession session) {
        if (session == null || session.messageId() == null || session.messageId() <= 0) {
            return;
        }
        sessions.put(session.messageId(), session);
    }

    public Optional<FileDownloadSelectionSession> findByMessageId(Long messageId) {
        if (messageId == null || messageId <= 0) {
            return Optional.empty();
        }
        FileDownloadSelectionSession session = sessions.get(messageId);
        if (session == null) {
            return Optional.empty();
        }
        if (session.expired()) {
            sessions.remove(messageId);
            return Optional.empty();
        }
        return Optional.of(session);
    }

    public void remove(Long messageId) {
        if (messageId != null) {
            sessions.remove(messageId);
        }
    }

    @PreDestroy
    public void shutdown() {
        cleanupExecutor.shutdown();
    }

    private void cleanupExpired() {
        sessions.entrySet().removeIf(entry -> entry.getValue() == null || entry.getValue().expired());
    }

}
