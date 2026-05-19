package xin.vanilla.banira.config.other;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.fasterxml.jackson.dataformat.yaml.YAMLGenerator;
import lombok.extern.slf4j.Slf4j;
import xin.vanilla.banira.config.YamlConfigWatcherService;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * 分群配置管理器。
 * 按 groupId 持久化配置，并监听配置文件变化。
 */
@Slf4j
public class GroupedYamlConfigManager<T> {
    private final Path configPath;
    private final Class<T> clazz;
    private final T defaultInstance;
    private final ObjectMapper mapper;
    private final AtomicBoolean isProcessing = new AtomicBoolean(false);
    private volatile long lastModifiedOnWrite = 0;
    private volatile Map<Long, T> currentMap = new LinkedHashMap<>();

    public GroupedYamlConfigManager(Path configPath,
                                    Class<T> clazz,
                                    T defaultInstance,
                                    YamlConfigWatcherService watcherService
    ) throws IOException {
        this.configPath = configPath.toAbsolutePath();
        this.clazz = clazz;
        this.defaultInstance = defaultInstance;
        this.mapper = new ObjectMapper(new YAMLFactory().disable(YAMLGenerator.Feature.WRITE_DOC_START_MARKER))
                .configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
                .setDefaultPropertyInclusion(JsonInclude.Value.construct(
                        JsonInclude.Include.ALWAYS,
                        JsonInclude.Include.ALWAYS
                ));

        init();
        watcherService.register(this.configPath, path -> {
            try {
                if (isProcessing.get()) {
                    return;
                }
                if (Files.notExists(path)) {
                    reload();
                    return;
                }
                long currentModified = Files.getLastModifiedTime(path).toMillis();
                if (currentModified != lastModifiedOnWrite) {
                    reload();
                }
            } catch (Exception e) {
                LOGGER.error("Error reloading grouped other config: {}", this.configPath, e);
            }
        });
    }

    private void init() throws IOException {
        Files.createDirectories(configPath.getParent());
        if (Files.notExists(configPath)) {
            writeMap(new LinkedHashMap<>());
        }
        reload();
    }

    private synchronized void reload() throws IOException {
        isProcessing.set(true);
        try {
            Map<String, Object> raw = mapper.readValue(configPath.toFile(), new TypeReference<>() {
            });
            Map<Long, T> loaded = new LinkedHashMap<>();
            if (raw != null) {
                raw.forEach((k, v) -> {
                    try {
                        Long groupId = Long.parseLong(k);
                        if (v != null) {
                            loaded.put(groupId, mapper.convertValue(v, clazz));
                        }
                    } catch (Exception ignored) {
                    }
                });
            }
            currentMap = loaded;
            writeMap(currentMap);
        } finally {
            isProcessing.set(false);
        }
    }

    public synchronized T get(Long groupId) {
        Long normalized = normalizeGroupId(groupId);
        return currentMap.computeIfAbsent(normalized, key -> deepCopy(defaultInstance));
    }

    /**
     * 仅获取指定群配置，不触发默认创建。
     */
    public synchronized Optional<T> getOnly(Long groupId) {
        return Optional.ofNullable(currentMap.get(normalizeGroupId(groupId)));
    }

    /**
     * 获取指定群配置，若不存在则回退群号0配置。
     */
    public synchronized T getOrGlobal(Long groupId) {
        Long normalized = normalizeGroupId(groupId);
        if (normalized != 0L && currentMap.containsKey(normalized)) {
            return currentMap.get(normalized);
        }
        return get(0L);
    }

    /**
     * 指定群配置是否存在。
     */
    public synchronized boolean contains(Long groupId) {
        return currentMap.containsKey(normalizeGroupId(groupId));
    }

    /**
     * 保存当前所有群配置。
     */
    public synchronized void save() throws IOException {
        writeMap(currentMap);
    }

    /**
     * 获取当前配置快照。
     */
    public synchronized Map<Long, T> snapshot() {
        return new LinkedHashMap<>(currentMap);
    }

    private Long normalizeGroupId(Long groupId) {
        return groupId != null ? groupId : 0L;
    }

    private void writeMap(Map<Long, T> map) throws IOException {
        Map<String, Object> raw = new LinkedHashMap<>();
        map.forEach((k, v) -> {
            if (k != null && v != null) {
                raw.put(String.valueOf(k), v);
            }
        });
        String yaml = mapper.writeValueAsString(raw).replaceAll("(?m): null(\\s|$)", ":$1");
        Files.writeString(configPath, yaml, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
        lastModifiedOnWrite = Files.getLastModifiedTime(configPath).toMillis();
    }

    private T deepCopy(T source) {
        return mapper.convertValue(source, clazz);
    }
}
