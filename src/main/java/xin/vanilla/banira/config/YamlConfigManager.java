package xin.vanilla.banira.config;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.fasterxml.jackson.dataformat.yaml.YAMLGenerator;
import jakarta.validation.ConstraintViolation;
import jakarta.validation.Validation;
import jakarta.validation.Validator;
import jakarta.validation.ValidatorFactory;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationEventPublisher;
import xin.vanilla.banira.util.DateUtils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * YAML配置管理器
 */
@Slf4j
public class YamlConfigManager<T> {

    private final Path configPath;
    private final T defaultInstance;
    private final Class<T> clazz;
    private final ObjectMapper mapper;
    private final Validator validator;
    private final ApplicationEventPublisher eventPublisher;
    private final String configName;
    private final AtomicBoolean isProcessing = new AtomicBoolean(false);
    private volatile long lastModifiedOnWrite = 0;

    private volatile T currentInstance;

    public YamlConfigManager(Path configPath,
                             T defaultInstance,
                             Class<T> clazz,
                             String configName,
                             YamlConfigWatcherService watcherService,
                             ApplicationEventPublisher eventPublisher
    ) throws IOException {
        this.configPath = configPath.toAbsolutePath();
        this.defaultInstance = defaultInstance;
        this.clazz = clazz;
        this.mapper = createYamlMapper();
        ValidatorFactory factory = Validation.buildDefaultValidatorFactory();
        this.validator = factory.getValidator();
        this.eventPublisher = eventPublisher;
        this.configName = configName;

        init();
        // 注册热刷新
        watcherService.register(this.configPath, path -> {
            try {
                // 忽略自身写入触发的变更
                if (isProcessing.get()) {
                    return;
                }

                // 检查文件是否确实发生了变化
                long currentModified = Files.getLastModifiedTime(configPath).toMillis();
                if (currentModified != lastModifiedOnWrite) {
                    reloadOnChange();
                }
            } catch (Exception e) {
                LOGGER.error("Error reloading config", e);
            }
        });
    }

    private ObjectMapper createYamlMapper() {
        ObjectMapper mapper = new ObjectMapper(new YAMLFactory()
                .disable(YAMLGenerator.Feature.WRITE_DOC_START_MARKER));
        mapper.setSerializationInclusion(JsonInclude.Include.ALWAYS);
        return mapper;
    }

    private void init() throws IOException {
        Files.createDirectories(configPath.getParent());

        if (Files.notExists(configPath)) {
            writeConfig(defaultInstance);
            this.currentInstance = deepCopy(defaultInstance);
            publishEvent(currentInstance);
            return;
        }

        loadAndProcessConfig();
    }

    private synchronized void reloadOnChange() throws IOException {
        if (isProcessing.get()) {
            return;
        }
        loadAndProcessConfig();
    }

    private void loadAndProcessConfig() throws IOException {
        isProcessing.set(true);
        try {
            T loaded;
            try {
                loaded = mapper.readValue(configPath.toFile(), clazz);
            } catch (Exception e) {
                handleLoadFailure("parse-failure", "Failed to parse config file", e);
                return;
            }

            try {
                // 深度合并配置
                T merged = deepMerge(defaultInstance, loaded);
                Set<ConstraintViolation<T>> violations = validator.validate(merged);

                if (violations.isEmpty()) {
                    // 只有当配置确实改变时才写回
                    if (!merged.equals(currentInstance)) {
                        writeConfig(merged);
                    }
                    this.currentInstance = merged;
                    publishEvent(merged);
                } else {
                    handleValidationFailure(merged, violations);
                }
            } catch (Exception e) {
                handleLoadFailure("merge-failure", "Failed to merge config", e);
            }
        } finally {
            isProcessing.set(false);
        }
    }

    @SuppressWarnings("unchecked")
    private T deepMerge(T defaults, T overrides) {
        // 序列化为Map结构
        Map<String, Object> defaultMap = mapper.convertValue(defaults, Map.class);
        Map<String, Object> overrideMap = mapper.convertValue(overrides, Map.class);

        // 递归合并
        Map<String, Object> resultMap = deepMergeMaps(defaultMap, overrideMap);

        // 反序列化回对象
        return mapper.convertValue(resultMap, clazz);
    }

    @SuppressWarnings("unchecked")
    private Map<String, Object> deepMergeMaps(Map<String, Object> defaults, Map<String, Object> overrides) {
        Map<String, Object> result = new LinkedHashMap<>(defaults);

        for (Map.Entry<String, Object> entry : overrides.entrySet()) {
            String key = entry.getKey();
            Object overrideValue = entry.getValue();
            Object defaultValue = result.get(key);

            if (defaultValue instanceof Map && overrideValue instanceof Map) {
                // 递归合并嵌套Map
                result.put(key, deepMergeMaps(
                        (Map<String, Object>) defaultValue,
                        (Map<String, Object>) overrideValue
                ));
            } else if (overrideValue != null) {
                // 覆盖非空值
                result.put(key, overrideValue);
            }
        }
        return result;
    }

    @SuppressWarnings("unchecked")
    private void handleValidationFailure(T config, Set<ConstraintViolation<T>> violations) throws IOException {
        backupOriginal("validation-failure");

        // 修复非法字段
        Map<String, Object> configMap = mapper.convertValue(config, Map.class);
        Map<String, Object> defaultMap = mapper.convertValue(defaultInstance, Map.class);

        for (ConstraintViolation<T> violation : violations) {
            String path = violation.getPropertyPath().toString();
            applyDefaultForPath(configMap, defaultMap, path);
        }

        // 创建修复后的配置
        T fixed = mapper.convertValue(configMap, clazz);
        Set<ConstraintViolation<T>> fixedViolations = validator.validate(fixed);

        if (fixedViolations.isEmpty()) {
            writeConfig(fixed);
            this.currentInstance = fixed;
            publishEvent(fixed);
        } else {
            handleLoadFailure("fix-failure", "Failed to fix invalid config", null);
        }
    }

    @SuppressWarnings("unchecked")
    private void applyDefaultForPath(Map<String, Object> configMap, Map<String, Object> defaultMap, String propertyPath) {
        String[] pathSegments = propertyPath.split("\\.");
        Map<String, Object> current = configMap;
        Map<String, Object> defaultCurrent = defaultMap;

        for (int i = 0; i < pathSegments.length; i++) {
            String segment = pathSegments[i];
            boolean isLast = (i == pathSegments.length - 1);

            Object node = current.get(segment);
            Object defaultNode = defaultCurrent != null ? defaultCurrent.get(segment) : null;

            if (isLast) {
                // 最终字段：应用默认值
                if (defaultNode != null) {
                    current.put(segment, deepCopyValue(defaultNode));
                } else {
                    current.remove(segment);
                }
            } else {
                // 嵌套路径处理
                if (node instanceof Map && defaultNode instanceof Map) {
                    current = (Map<String, Object>) node;
                    defaultCurrent = (Map<String, Object>) defaultNode;
                } else if (defaultNode instanceof Map) {
                    // 替换无效结构
                    Map<String, Object> newMap = deepCopyMap((Map<String, Object>) defaultNode);
                    current.put(segment, newMap);
                    current = newMap;
                    defaultCurrent = (Map<String, Object>) defaultNode;
                } else {
                    // 路径中断，停止处理
                    return;
                }
            }
        }
    }

    private Object deepCopyValue(Object source) {
        return mapper.convertValue(source, source.getClass());
    }

    @SuppressWarnings("unchecked")
    private Map<String, Object> deepCopyMap(Map<String, Object> source) {
        Map<String, Object> copy = new LinkedHashMap<>();
        source.forEach((k, v) -> {
            if (v instanceof Map) {
                copy.put(k, deepCopyMap((Map<String, Object>) v));
            } else {
                copy.put(k, v);
            }
        });
        return copy;
    }

    private void handleLoadFailure(String reason, String message, Exception e) throws IOException {
        if (e != null) {
            LOGGER.error(message, e);
        } else {
            LOGGER.error(message);
        }

        backupOriginal(reason);
        writeConfig(defaultInstance);
        this.currentInstance = deepCopy(defaultInstance);
        publishEvent(currentInstance);
    }

    private void backupOriginal(String reason) throws IOException {
        String stamp = DateUtils.toDateTimeString(new Date()).replaceAll("[- :]", "_");
        Path backup = configPath.resolveSibling(
                configPath.getFileName() + "." + reason + ".bak." + stamp + ".yml");
        Files.copy(configPath, backup, StandardCopyOption.COPY_ATTRIBUTES);
    }

    private void writeConfig(T obj) throws IOException {
        try {
            String raw = mapper.writeValueAsString(obj);
            raw = raw.replaceAll("(?m): null(\\s|$)", ":$1");
            Files.writeString(configPath, raw,
                    StandardOpenOption.CREATE,
                    StandardOpenOption.TRUNCATE_EXISTING);
            // 记录本次写入的时间戳
            lastModifiedOnWrite = Files.getLastModifiedTime(configPath).toMillis();
        } catch (IOException e) {
            LOGGER.error("Failed to write config", e);
            throw e;
        }
    }

    private T deepCopy(T source) {
        try {
            return mapper.readValue(mapper.writeValueAsBytes(source), clazz);
        } catch (IOException e) {
            throw new RuntimeException("Failed to deep copy", e);
        }
    }

    public T getCurrent() {
        return currentInstance;
    }

    public void save() throws IOException {
        writeConfig(currentInstance);
    }

    private void publishEvent(T instance) {
        if (eventPublisher != null) {
            eventPublisher.publishEvent(new ConfigReloadedEvent<>(this, configName, clazz, instance));
        }
    }
}
