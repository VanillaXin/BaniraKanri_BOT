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
import java.util.Map;
import java.util.Set;

@Slf4j
public class YamlConfigManager<T> {

    private final Path configPath;
    private final T defaultInstance;
    private final Class<T> clazz;
    private final ObjectMapper mapper;
    private final Validator validator;
    private final ApplicationEventPublisher eventPublisher;
    private final String configName;

    // volatile single instance
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
                reloadOnChange();
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

        T loaded;
        try {
            loaded = mapper.readValue(configPath.toFile(), clazz);
        } catch (Exception e) {
            backupOriginal("parse-failure");
            writeConfig(defaultInstance);
            this.currentInstance = deepCopy(defaultInstance);
            publishEvent(currentInstance);
            return;
        }

        T merged = validateAndMerge(loaded);
        this.currentInstance = merged;
        publishEvent(merged);
    }

    private synchronized void reloadOnChange() throws IOException {
        T loaded;
        try {
            loaded = mapper.readValue(configPath.toFile(), clazz);
        } catch (Exception e) {
            // 读取出错，不覆盖当前，备份失败文件
            backupOriginal("hot-reload-parse-failure");
            return;
        }
        T merged = validateAndMerge(loaded);
        this.currentInstance = merged;
        publishEvent(merged);
    }

    private T validateAndMerge(T loaded) throws IOException {
        Set<ConstraintViolation<T>> violations = validator.validate(loaded);
        if (violations.isEmpty()) {
            return loaded;
        }
        // 验证失败：备份原始，按字段从 defaultInstance 覆盖
        backupOriginal("validation-failure");
        // 把 loaded / defaultInstance 都转成 Map
        Map<String, Object> loadedMap = mapper.convertValue(loaded, Map.class);
        Map<String, Object> defaultMap = mapper.convertValue(defaultInstance, Map.class);

        for (ConstraintViolation<T> v : violations) {
            String path = v.getPropertyPath().toString(); // 可能是 nested.field
            applyDefaultForPath(loadedMap, defaultMap, path);
        }
        // 反序列化回对象
        T merged = mapper.convertValue(loadedMap, clazz);
        // 写回修复后的
        writeConfig(merged);
        return merged;
    }

    /**
     * 递归地在 loadedMap 中将 propertyPath 指向的值替换成 defaultMap 中对应值
     */
    @SuppressWarnings("unchecked")
    private void applyDefaultForPath(Map<String, Object> loadedMap, Map<String, Object> defaultMap, String propertyPath) {
        String[] parts = propertyPath.split("\\.");
        Map<String, Object> cursorLoaded = loadedMap;
        Map<String, Object> cursorDefault = defaultMap;
        for (int i = 0; i < parts.length; i++) {
            String part = parts[i];
            if (i == parts.length - 1) {
                // 最后一层，替换
                Object defaultVal = cursorDefault != null ? cursorDefault.get(part) : null;
                if (defaultVal != null) {
                    cursorLoaded.put(part, defaultVal);
                } else {
                    // 如果 default 也没有，移除有问题的字段以让后续校验处理
                    cursorLoaded.remove(part);
                }
            } else {
                Object nextLoaded = cursorLoaded.get(part);
                Object nextDefault = cursorDefault != null ? cursorDefault.get(part) : null;
                if (!(nextLoaded instanceof Map) || !(nextDefault instanceof Map)) {
                    // 结构不一致，直接用 default 里的整个子树替换
                    if (nextDefault instanceof Map) {
                        cursorLoaded.put(part, deepCopyMap((Map<String, Object>) nextDefault));
                    }
                    return;
                }
                cursorLoaded = (Map<String, Object>) nextLoaded;
                cursorDefault = (Map<String, Object>) nextDefault;
            }
        }
    }

    @SuppressWarnings("unchecked")
    private Map<String, Object> deepCopyMap(Map<String, Object> source) {
        return mapper.convertValue(mapper.convertValue(source, Map.class), Map.class);
    }

    private void backupOriginal(String reason) throws IOException {
        String stamp = DateUtils.toDateTimeString(new Date()).replaceAll("[- :]", "_");
        Path backup = configPath.resolveSibling(
                configPath.getFileName() + "." + reason + ".bak." + stamp + ".yml");
        Files.copy(configPath, backup, StandardCopyOption.COPY_ATTRIBUTES);
    }

    private void writeConfig(T obj) throws IOException {
        String raw = mapper.writeValueAsString(obj);
        raw = raw.replaceAll("(?m): null(\\s|$)", ":$1");
        Files.writeString(configPath, raw, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
    }

    private T deepCopy(T source) {
        try {
            byte[] bytes = mapper.writeValueAsBytes(source);
            return mapper.readValue(bytes, clazz);
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
