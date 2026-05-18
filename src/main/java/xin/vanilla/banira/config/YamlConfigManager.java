package xin.vanilla.banira.config;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.fasterxml.jackson.dataformat.yaml.YAMLGenerator;
import jakarta.validation.ConstraintViolation;
import jakarta.validation.Validation;
import jakarta.validation.Validator;
import jakarta.validation.ValidatorFactory;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationEventPublisher;
import xin.vanilla.banira.config.entity.GlobalConfig;
import xin.vanilla.banira.config.entity.GroupConfig;
import xin.vanilla.banira.config.entity.InstructionsConfig;
import xin.vanilla.banira.config.entity.basic.OtherConfig;
import xin.vanilla.banira.config.entity.basic.PermissionConfig;
import xin.vanilla.banira.config.entity.basic.PluginConfig;
import xin.vanilla.banira.config.entity.extended.*;
import xin.vanilla.banira.event.GlobalConfigReloadedEvent;
import xin.vanilla.banira.event.GroupConfigReloadedEvent;
import xin.vanilla.banira.event.InstructionsConfigReloadedEvent;
import xin.vanilla.banira.event.PluginConfigReloadedEvent;
import xin.vanilla.banira.util.DateUtils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.BiConsumer;

/**
 * YAML配置管理器
 */
@Slf4j
public class YamlConfigManager<T> {
    private static final String GROUP_SPLIT_DIR = "group";
    private static final String GROUP_CHAT_CONFIG = "group-chat-config.yml";
    private static final String GROUP_SOCIAL_MEDIA_CONFIG = "group-social-media-config.yml";
    private static final String GROUP_MCMOD_COMMENT_CONFIG = "group-mcmod-comment-config.yml";
    private static final String GROUP_MCMOD_COOKIE_CONFIG = "group-mcmod-cookie-config.yml";
    private static final String GROUP_WIFE_CONFIG = "group-wife-config.yml";
    private static final String GROUP_MC_CONFIG = "group-mc-config.yml";
    private static final String GROUP_STATUS_BG_CONFIG = "group-status-bg-config.yml";
    private static final String GROUP_RANDOM_IMG_CONFIG = "group-random-img-config.yml";

    private final Path configPath;
    private final T defaultInstance;
    private final Class<T> clazz;
    private final ObjectMapper mapper;
    private final Validator validator;
    private final ApplicationEventPublisher eventPublisher;
    private final String configName;
    private final AtomicBoolean isProcessing = new AtomicBoolean(false);
    private volatile long lastModifiedOnWrite = 0;
    private final Map<Path, Long> splitLastModifiedOnWrite = new HashMap<>();

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
        @SuppressWarnings("resource")
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
        if (isGroupConfigManager()) {
            registerGroupSplitWatchers(watcherService);
        }
    }

    private ObjectMapper createYamlMapper() {
        ObjectMapper mapper = new ObjectMapper(new YAMLFactory()
                .disable(YAMLGenerator.Feature.WRITE_DOC_START_MARKER));
        mapper.setDefaultPropertyInclusion(JsonInclude.Value.construct(
                JsonInclude.Include.ALWAYS,
                JsonInclude.Include.ALWAYS
        ));
        // 忽略未知字段
        mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        return mapper;
    }

    private boolean isGroupConfigManager() {
        return GroupConfig.class.isAssignableFrom(clazz);
    }

    private void registerGroupSplitWatchers(YamlConfigWatcherService watcherService) throws IOException {
        for (Path splitPath : getGroupSplitPaths()) {
            watcherService.register(splitPath, path -> {
                try {
                    if (isProcessing.get()) {
                        return;
                    }
                    if (Files.notExists(path)) {
                        reloadOnChange();
                        return;
                    }
                    long currentModified = Files.getLastModifiedTime(path).toMillis();
                    long lastWrite = splitLastModifiedOnWrite.getOrDefault(path, 0L);
                    if (currentModified != lastWrite) {
                        reloadOnChange();
                    }
                } catch (Exception e) {
                    LOGGER.error("Error reloading split group config: {}", path, e);
                }
            });
        }
    }

    private void init() throws IOException {
        Files.createDirectories(configPath.getParent());
        if (isGroupConfigManager()) {
            Files.createDirectories(resolveGroupSplitDir());
        }

        if (Files.notExists(configPath)) {
            writeConfig(defaultInstance);
            this.currentInstance = deepCopy(defaultInstance);
            publishEvent(currentInstance);
            return;
        }

        loadAndProcessConfig();
    }

    private Path resolveGroupSplitDir() {
        return configPath.getParent().resolve(GROUP_SPLIT_DIR);
    }

    private Path resolveGroupSplitFile(String fileName) {
        return resolveGroupSplitDir().resolve(fileName).toAbsolutePath();
    }

    private List<Path> getGroupSplitPaths() {
        List<Path> paths = new ArrayList<>();
        paths.add(resolveGroupSplitFile(GROUP_CHAT_CONFIG));
        paths.add(resolveGroupSplitFile(GROUP_SOCIAL_MEDIA_CONFIG));
        paths.add(resolveGroupSplitFile(GROUP_MCMOD_COMMENT_CONFIG));
        paths.add(resolveGroupSplitFile(GROUP_MCMOD_COOKIE_CONFIG));
        paths.add(resolveGroupSplitFile(GROUP_WIFE_CONFIG));
        paths.add(resolveGroupSplitFile(GROUP_MC_CONFIG));
        paths.add(resolveGroupSplitFile(GROUP_STATUS_BG_CONFIG));
        paths.add(resolveGroupSplitFile(GROUP_RANDOM_IMG_CONFIG));
        return paths;
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
            Map<String, Object> loaded;
            try {
                loaded = mapper.readValue(configPath.toFile(), new TypeReference<>() {
                });
            } catch (Exception e) {
                handleLoadFailure("parse-failure", "Failed to parse config file", e);
                return;
            }

            try {
                // 深度合并配置
                T merged = deepMerge(defaultInstance, loaded);
                merged = applySplitOverrides(merged);
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
    private T applySplitOverrides(T config) throws IOException {
        if (!isGroupConfigManager()) {
            return config;
        }
        GroupConfig groupConfig = (GroupConfig) config;
        Map<Long, OtherConfig> otherConfigMap = groupConfig.otherConfig();
        if (otherConfigMap == null) {
            otherConfigMap = new LinkedHashMap<>();
            groupConfig.otherConfig(otherConfigMap);
        }
        mergeSplitMap(readSplitObjectMap(resolveGroupSplitFile(GROUP_CHAT_CONFIG), ChatConfig.class), otherConfigMap,
                OtherConfig::chatConfig);
        mergeSplitMap(readSplitObjectMap(resolveGroupSplitFile(GROUP_SOCIAL_MEDIA_CONFIG), Boolean.class), otherConfigMap,
                (other, value) -> other.socialMedia(value != null && value));
        mergeSplitMap(readSplitObjectMap(resolveGroupSplitFile(GROUP_MCMOD_COMMENT_CONFIG), McModCommentConfig.class), otherConfigMap,
                OtherConfig::mcModCommentConfig);
        mergeSplitMap(readSplitObjectMap(resolveGroupSplitFile(GROUP_MCMOD_COOKIE_CONFIG), McModCookieConfig.class), otherConfigMap,
                OtherConfig::mcModCookieConfig);
        mergeSplitMap(readSplitListMap(resolveGroupSplitFile(GROUP_WIFE_CONFIG), WifeConfig.class), otherConfigMap,
                OtherConfig::wifeConfig);
        mergeSplitMap(readSplitObjectMap(resolveGroupSplitFile(GROUP_MC_CONFIG), McConfig.class), otherConfigMap,
                OtherConfig::mcConfig);
        mergeSplitMap(readSplitObjectMap(resolveGroupSplitFile(GROUP_STATUS_BG_CONFIG), String.class), otherConfigMap,
                OtherConfig::statusBgUrl);
        mergeSplitMap(readSplitListMap(resolveGroupSplitFile(GROUP_RANDOM_IMG_CONFIG), String.class), otherConfigMap,
                OtherConfig::randomImgPath);
        return (T) groupConfig;
    }

    @SuppressWarnings("unchecked")
    private T deepMerge(T defaults, Map<String, Object> overrideMap) {
        // 序列化为Map结构
        Map<String, Object> defaultMap = mapper.convertValue(defaults, Map.class);

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
            String raw = serializeForWrite(obj);
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

    private String serializeForWrite(T obj) throws IOException {
        if (obj instanceof GroupConfig groupConfig) {
            writeGroupSplitConfigs(groupConfig);
            Map<String, Object> compactConfig = new LinkedHashMap<>();
            Map<Long, List<PermissionConfig>> maid = groupConfig.maid();
            if (maid != null && !maid.isEmpty()) {
                Map<String, Object> compactMaid = new LinkedHashMap<>();
                maid.forEach((groupId, permissions) -> {
                    if (permissions != null && !permissions.isEmpty()) {
                        compactMaid.put(String.valueOf(groupId), permissions);
                    }
                });
                if (!compactMaid.isEmpty()) {
                    compactConfig.put("maid", compactMaid);
                }
            }

            Map<Long, OtherConfig> otherConfig = groupConfig.otherConfig();
            if (otherConfig != null && !otherConfig.isEmpty()) {
                Map<String, Object> defaultOtherMap = mapper.convertValue(new OtherConfig(), new TypeReference<>() {
                });
                Map<String, Object> compactOther = new LinkedHashMap<>();
                otherConfig.forEach((groupId, config) -> {
                    if (config == null) {
                        return;
                    }
                    Map<String, Object> currentMap = mapper.convertValue(config, new TypeReference<>() {
                    });
                    Map<String, Object> compactMap = pruneMapByDefaults(currentMap, defaultOtherMap, true);
                    if (!compactMap.isEmpty()) {
                        compactOther.put(String.valueOf(groupId), compactMap);
                    }
                });
                if (!compactOther.isEmpty()) {
                    compactConfig.put("otherConfig", compactOther);
                }
            }

            return mapper.writeValueAsString(compactConfig);
        }
        return mapper.writeValueAsString(obj);
    }

    private Map<String, Object> pruneMapByDefaults(Map<String, Object> current, Map<String, Object> defaults, boolean skipSplitFields) {
        Map<String, Object> result = new LinkedHashMap<>();
        for (Map.Entry<String, Object> entry : current.entrySet()) {
            String key = entry.getKey();
            if (skipSplitFields && isSplitFieldKey(key)) {
                continue;
            }
            Object currentValue = entry.getValue();
            Object defaultValue = defaults != null ? defaults.get(key) : null;
            Object prunedValue = pruneValueByDefault(currentValue, defaultValue, skipSplitFields);
            if (!isEmptyValue(prunedValue)) {
                result.put(key, prunedValue);
            }
        }
        return result;
    }

    @SuppressWarnings("unchecked")
    private Object pruneValueByDefault(Object currentValue, Object defaultValue, boolean skipSplitFields) {
        switch (currentValue) {
            case null -> {
                return null;
            }
            case Map<?, ?> currentMap -> {
                Map<String, Object> defaultMap = defaultValue instanceof Map<?, ?> map
                        ? (Map<String, Object>) map
                        : null;
                Map<String, Object> compactMap = pruneMapByDefaults((Map<String, Object>) currentMap, defaultMap, skipSplitFields);
                return compactMap.isEmpty() ? null : compactMap;
            }
            case List<?> currentList -> {
                if (currentList.isEmpty()) {
                    return null;
                }
                if (defaultValue instanceof List<?> defaultList && Objects.equals(currentList, defaultList)) {
                    return null;
                }
                return currentList;
            }
            default -> {
            }
        }
        return Objects.equals(currentValue, defaultValue) ? null : currentValue;
    }

    private boolean isSplitFieldKey(String key) {
        return "chatConfig".equals(key)
                || "socialMedia".equals(key)
                || "mcModCommentConfig".equals(key)
                || "mcModCookieConfig".equals(key)
                || "wifeConfig".equals(key)
                || "mcConfig".equals(key)
                || "statusBgUrl".equals(key)
                || "randomImgPath".equals(key);
    }

    private void writeGroupSplitConfigs(GroupConfig groupConfig) throws IOException {
        Map<Long, OtherConfig> otherConfigMap = groupConfig.otherConfig();
        if (otherConfigMap == null) {
            otherConfigMap = Collections.emptyMap();
        }
        Map<Long, ChatConfig> chatConfigMap = new LinkedHashMap<>();
        Map<Long, Boolean> socialMediaMap = new LinkedHashMap<>();
        Map<Long, McModCommentConfig> mcModCommentMap = new LinkedHashMap<>();
        Map<Long, McModCookieConfig> mcModCookieMap = new LinkedHashMap<>();
        Map<Long, List<WifeConfig>> wifeConfigMap = new LinkedHashMap<>();
        Map<Long, McConfig> mcConfigMap = new LinkedHashMap<>();
        Map<Long, String> statusBgMap = new LinkedHashMap<>();
        Map<Long, List<String>> randomImgPathMap = new LinkedHashMap<>();

        OtherConfig defaultOtherConfig = new OtherConfig();
        otherConfigMap.forEach((groupId, config) -> {
            if (config == null) {
                return;
            }
            if (!Objects.equals(config.chatConfig(), defaultOtherConfig.chatConfig())) {
                chatConfigMap.put(groupId, config.chatConfig());
            }
            if (!Objects.equals(config.socialMedia(), defaultOtherConfig.socialMedia())) {
                socialMediaMap.put(groupId, config.socialMedia());
            }
            if (!Objects.equals(config.mcModCommentConfig(), defaultOtherConfig.mcModCommentConfig())) {
                mcModCommentMap.put(groupId, config.mcModCommentConfig());
            }
            if (!Objects.equals(config.mcModCookieConfig(), defaultOtherConfig.mcModCookieConfig())) {
                mcModCookieMap.put(groupId, config.mcModCookieConfig());
            }
            if (!Objects.equals(config.wifeConfig(), defaultOtherConfig.wifeConfig())) {
                wifeConfigMap.put(groupId, config.wifeConfig());
            }
            if (!Objects.equals(config.mcConfig(), defaultOtherConfig.mcConfig())) {
                mcConfigMap.put(groupId, config.mcConfig());
            }
            if (!Objects.equals(config.statusBgUrl(), defaultOtherConfig.statusBgUrl())) {
                statusBgMap.put(groupId, config.statusBgUrl());
            }
            if (!Objects.equals(config.randomImgPath(), defaultOtherConfig.randomImgPath())) {
                randomImgPathMap.put(groupId, config.randomImgPath());
            }
        });

        writeSplitObjectMap(resolveGroupSplitFile(GROUP_CHAT_CONFIG), chatConfigMap);
        writeSplitObjectMap(resolveGroupSplitFile(GROUP_SOCIAL_MEDIA_CONFIG), socialMediaMap);
        writeSplitObjectMap(resolveGroupSplitFile(GROUP_MCMOD_COMMENT_CONFIG), mcModCommentMap);
        writeSplitObjectMap(resolveGroupSplitFile(GROUP_MCMOD_COOKIE_CONFIG), mcModCookieMap);
        writeSplitObjectMap(resolveGroupSplitFile(GROUP_WIFE_CONFIG), wifeConfigMap);
        writeSplitObjectMap(resolveGroupSplitFile(GROUP_MC_CONFIG), mcConfigMap);
        writeSplitObjectMap(resolveGroupSplitFile(GROUP_STATUS_BG_CONFIG), statusBgMap);
        writeSplitObjectMap(resolveGroupSplitFile(GROUP_RANDOM_IMG_CONFIG), randomImgPathMap);
    }

    private <V> void mergeSplitMap(Map<Long, V> splitValues,
                                   Map<Long, OtherConfig> target,
                                   BiConsumer<OtherConfig, V> setter
    ) {
        splitValues.forEach((groupId, value) -> {
            OtherConfig config = target.computeIfAbsent(groupId, key -> new OtherConfig());
            setter.accept(config, value);
        });
    }

    private <V> Map<Long, V> readSplitObjectMap(Path splitPath, Class<V> clazz) throws IOException {
        if (Files.notExists(splitPath)) {
            return new LinkedHashMap<>();
        }
        Map<String, Object> rawMap = mapper.readValue(splitPath.toFile(), new TypeReference<>() {
        });
        if (rawMap == null || rawMap.isEmpty()) {
            return new LinkedHashMap<>();
        }
        Map<Long, V> result = new LinkedHashMap<>();
        rawMap.forEach((k, v) -> {
            Long groupId = parseGroupId(k);
            if (groupId != null && v != null) {
                result.put(groupId, mapper.convertValue(v, clazz));
            }
        });
        return result;
    }

    private <V> Map<Long, List<V>> readSplitListMap(Path splitPath, Class<V> elementType) throws IOException {
        if (Files.notExists(splitPath)) {
            return new LinkedHashMap<>();
        }
        Map<String, Object> rawMap = mapper.readValue(splitPath.toFile(), new TypeReference<>() {
        });
        if (rawMap == null || rawMap.isEmpty()) {
            return new LinkedHashMap<>();
        }
        Map<Long, List<V>> result = new LinkedHashMap<>();
        rawMap.forEach((k, v) -> {
            Long groupId = parseGroupId(k);
            if (groupId == null || v == null) {
                return;
            }
            List<V> values = mapper.convertValue(v, mapper.getTypeFactory().constructCollectionType(List.class, elementType));
            if (values != null) {
                result.put(groupId, values);
            }
        });
        return result;
    }

    private <V> void writeSplitObjectMap(Path splitPath, Map<Long, V> map) throws IOException {
        Files.createDirectories(splitPath.getParent());
        Map<String, Object> raw = new LinkedHashMap<>();
        map.forEach((k, v) -> {
            if (k != null && v != null && !isEmptyValue(v)) {
                raw.put(String.valueOf(k), v);
            }
        });
        if (raw.isEmpty()) {
            Files.deleteIfExists(splitPath);
            splitLastModifiedOnWrite.remove(splitPath);
            return;
        }
        String yaml = mapper.writeValueAsString(raw);
        yaml = yaml.replaceAll("(?m): null(\\s|$)", ":$1");
        Files.writeString(splitPath, yaml, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
        splitLastModifiedOnWrite.put(splitPath, Files.getLastModifiedTime(splitPath).toMillis());
    }

    private Long parseGroupId(String key) {
        try {
            return Long.parseLong(key);
        } catch (Exception ignored) {
            return null;
        }
    }

    private boolean isEmptyValue(Object value) {
        return switch (value) {
            case null -> true;
            case Map<?, ?> map -> map.isEmpty();
            case List<?> list -> list.isEmpty();
            default -> false;
        };
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
            if (instance instanceof GlobalConfig config) {
                eventPublisher.publishEvent(new GlobalConfigReloadedEvent(this, configName, config));
            } else if (instance instanceof GroupConfig config) {
                eventPublisher.publishEvent(new GroupConfigReloadedEvent(this, configName, config));
            } else if (instance instanceof InstructionsConfig config) {
                eventPublisher.publishEvent(new InstructionsConfigReloadedEvent(this, configName, config));
            } else if (instance instanceof PluginConfig config) {
                eventPublisher.publishEvent(new PluginConfigReloadedEvent(this, configName, config));
            }
        }
    }
}
