package xin.vanilla.banira.config;

import org.springframework.stereotype.Component;

import java.io.IOException;
import java.nio.file.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.*;
import java.util.function.Consumer;

@Component
public class YamlConfigWatcherService {

    private final WatchService watchService;
    private final Map<Path, List<Consumer<Path>>> listeners = new ConcurrentHashMap<>();
    private final Map<Path, ScheduledFuture<?>> debounceTasks = new ConcurrentHashMap<>();
    private final ScheduledExecutorService scheduler = Executors.newScheduledThreadPool(1);
    private final ExecutorService watcherExecutor = Executors.newSingleThreadExecutor();

    public YamlConfigWatcherService() throws IOException {
        this.watchService = FileSystems.getDefault().newWatchService();
        startLoop();
    }

    /**
     * 注册配置文件的路径和变更回调
     */
    public void register(Path configFile, Consumer<Path> onChange) throws IOException {
        Path dir = configFile.toAbsolutePath().getParent();
        dir.register(watchService, StandardWatchEventKinds.ENTRY_MODIFY, StandardWatchEventKinds.ENTRY_CREATE);
        listeners.computeIfAbsent(configFile.toAbsolutePath(), k -> new ArrayList<>()).add(onChange);
    }

    private void startLoop() {
        watcherExecutor.submit(() -> {
            while (true) {
                WatchKey key;
                try {
                    key = watchService.take();
                } catch (InterruptedException ie) {
                    Thread.currentThread().interrupt();
                    break;
                }
                for (WatchEvent<?> ev : key.pollEvents()) {
                    if (ev.kind() == StandardWatchEventKinds.OVERFLOW) continue;
                    Path changedName = (Path) ev.context();
                    Path dir = (Path) key.watchable();
                    Path absoluteChanged = dir.resolve(changedName).toAbsolutePath();

                    // 找出哪些注册的 configFile 是这个文件
                    for (Path registered : listeners.keySet()) {
                        if (registered.equals(absoluteChanged)) {
                            // debounce per file
                            debounceTasks.compute(registered, (p, existing) -> {
                                if (existing != null && !existing.isDone()) existing.cancel(false);
                                return scheduler.schedule(() -> {
                                    List<Consumer<Path>> cbs = listeners.getOrDefault(registered, Collections.emptyList());
                                    for (Consumer<Path> cb : cbs) {
                                        try {
                                            cb.accept(registered);
                                        } catch (Exception e) {
                                            e.printStackTrace();
                                        }
                                    }
                                }, 400, TimeUnit.MILLISECONDS);
                            });
                        }
                    }
                }
                key.reset();
            }
        });
    }
}
