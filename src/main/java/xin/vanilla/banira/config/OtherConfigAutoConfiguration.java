package xin.vanilla.banira.config;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.fasterxml.jackson.dataformat.yaml.YAMLGenerator;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import xin.vanilla.banira.config.entity.basic.OtherConfig;
import xin.vanilla.banira.config.entity.basic.PluginConfig;
import xin.vanilla.banira.config.entity.extended.*;
import xin.vanilla.banira.config.other.OtherConfigDefinition;
import xin.vanilla.banira.config.other.OtherConfigKeys;
import xin.vanilla.banira.config.other.OtherConfigRegistry;
import xin.vanilla.banira.plugin.socialmedia.SocialMediaSettings;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Supplier;

@Configuration
public class OtherConfigAutoConfiguration {
    private static final ObjectMapper YAML_MAPPER = new ObjectMapper(new YAMLFactory()
            .disable(YAMLGenerator.Feature.WRITE_DOC_START_MARKER))
            .configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
    private static final Path GLOBAL_CONFIG_PATH = Paths.get("./config/global-config.yml");
    private static final Path LEGACY_PLUGIN_CONFIG_PATH = Paths.get("./config/plugin-config.yml");
    private static final Path LEGACY_SOCIAL_MEDIA_CONFIG_PATH = Paths.get("./config/social-media-config.yml");
    private static final Path LEGACY_GROUP_CONFIG_PATH = Paths.get("./config/group-config.yml");
    private static final Path LEGACY_GROUP_SPLIT_DIR = Paths.get("./config/group");
    private static final Path NEW_SHARED_DIR = Paths.get("./config/other/shared");
    private static final Path NEW_GROUPED_DIR = Paths.get("./config/other/grouped");
    private static final Path NEW_PLUGIN_CONFIG_PATH = NEW_SHARED_DIR.resolve(OtherConfigKeys.PLUGIN + ".yml");
    private static final Path NEW_SOCIAL_MEDIA_CONFIG_PATH = NEW_SHARED_DIR.resolve(OtherConfigKeys.SOCIAL_MEDIA + ".yml");
    private static final Path NEW_GROUP_OTHER_CONFIG_PATH = NEW_GROUPED_DIR.resolve(OtherConfigKeys.GROUP_OTHER + ".yml");

    private static volatile boolean migrated = false;

    @Bean
    public OtherConfigDefinition<PluginConfig> pluginOtherConfigDefinition() throws IOException {
        migrateLegacyOtherConfigs();
        return OtherConfigDefinition.shared(OtherConfigKeys.PLUGIN, PluginConfig.class, PluginConfig::new);
    }

    @Bean
    public OtherConfigDefinition<SocialMediaSettings> socialMediaOtherConfigDefinition() throws IOException {
        migrateLegacyOtherConfigs();
        return OtherConfigDefinition.shared(OtherConfigKeys.SOCIAL_MEDIA, SocialMediaSettings.class, SocialMediaSettings::new);
    }

    @Bean
    public OtherConfigDefinition<OtherConfig> groupOtherConfigDefinition() throws IOException {
        migrateLegacyOtherConfigs();
        return OtherConfigDefinition.grouped(OtherConfigKeys.GROUP_OTHER, OtherConfig.class, OtherConfig::new);
    }

    @Bean
    public Supplier<PluginConfig> pluginConfig(OtherConfigRegistry otherConfigRegistry) {
        return () -> otherConfigRegistry.getShared(OtherConfigKeys.PLUGIN, PluginConfig.class);
    }

    @Bean
    public Supplier<SocialMediaSettings> socialMediaConfig(OtherConfigRegistry otherConfigRegistry) {
        return () -> otherConfigRegistry.getShared(OtherConfigKeys.SOCIAL_MEDIA, SocialMediaSettings.class);
    }

    @Bean
    public Function<Long, OtherConfig> groupOtherConfig(OtherConfigRegistry otherConfigRegistry) {
        return groupId -> otherConfigRegistry.getGrouped(OtherConfigKeys.GROUP_OTHER, groupId, OtherConfig.class);
    }

    private synchronized void migrateLegacyOtherConfigs() throws IOException {
        if (migrated) {
            return;
        }
        Files.createDirectories(NEW_SHARED_DIR);
        Files.createDirectories(NEW_GROUPED_DIR);

        migrateSharedConfig("pluginConfig", LEGACY_PLUGIN_CONFIG_PATH, NEW_PLUGIN_CONFIG_PATH);
        migrateSharedConfig("socialMedia", LEGACY_SOCIAL_MEDIA_CONFIG_PATH, NEW_SOCIAL_MEDIA_CONFIG_PATH);
        migrateGroupedOtherConfig();

        migrated = true;
    }

    private void migrateSharedConfig(String globalField, Path legacyPath, Path targetPath) throws IOException {
        if (Files.exists(targetPath)) {
            return;
        }
        if (Files.exists(legacyPath)) {
            Files.copy(legacyPath, targetPath);
            return;
        }
        if (Files.notExists(GLOBAL_CONFIG_PATH)) {
            return;
        }
        Map<String, Object> globalMap = readYamlMap(GLOBAL_CONFIG_PATH);
        if (!globalMap.containsKey(globalField)) {
            return;
        }
        Object value = globalMap.remove(globalField);
        writeYamlValue(targetPath, value);
        writeYamlMap(GLOBAL_CONFIG_PATH, globalMap);
    }

    private void migrateGroupedOtherConfig() throws IOException {
        if (Files.exists(NEW_GROUP_OTHER_CONFIG_PATH)) {
            return;
        }
        Map<Long, OtherConfig> merged = new LinkedHashMap<>();

        if (Files.exists(LEGACY_GROUP_CONFIG_PATH)) {
            Map<String, Object> groupMap = readYamlMap(LEGACY_GROUP_CONFIG_PATH);
            Object otherConfig = groupMap.get("otherConfig");
            if (otherConfig instanceof Map<?, ?> rawOtherConfig) {
                rawOtherConfig.forEach((k, v) -> {
                    Long groupId = toGroupId(k);
                    if (groupId != null && v != null) {
                        merged.put(groupId, YAML_MAPPER.convertValue(v, OtherConfig.class));
                    }
                });
            }
        }

        mergeGroupField(merged, LEGACY_GROUP_SPLIT_DIR.resolve("group-chat-config.yml"), ChatConfig.class, OtherConfig::chatConfig);
        mergeGroupField(merged, LEGACY_GROUP_SPLIT_DIR.resolve("group-social-media-config.yml"), Boolean.class, (target, value) -> target.socialMedia(value != null && value));
        mergeGroupField(merged, LEGACY_GROUP_SPLIT_DIR.resolve("group-mcmod-comment-config.yml"), McModCommentConfig.class, OtherConfig::mcModCommentConfig);
        mergeGroupField(merged, LEGACY_GROUP_SPLIT_DIR.resolve("group-mcmod-cookie-config.yml"), McModCookieConfig.class, OtherConfig::mcModCookieConfig);
        mergeGroupField(merged, LEGACY_GROUP_SPLIT_DIR.resolve("group-mc-config.yml"), McConfig.class, OtherConfig::mcConfig);
        mergeGroupField(merged, LEGACY_GROUP_SPLIT_DIR.resolve("group-status-bg-config.yml"), String.class, OtherConfig::statusBgUrl);
        mergeGroupListField(merged, LEGACY_GROUP_SPLIT_DIR.resolve("group-wife-config.yml"), WifeConfig.class, OtherConfig::wifeConfig);
        mergeGroupListField(merged, LEGACY_GROUP_SPLIT_DIR.resolve("group-random-img-config.yml"), String.class, OtherConfig::randomImgPath);

        if (!merged.isEmpty()) {
            writeYamlValue(NEW_GROUP_OTHER_CONFIG_PATH, merged);
        }
    }

    private <V> void mergeGroupField(Map<Long, OtherConfig> merged,
                                     Path splitPath,
                                     Class<V> type,
                                     FieldSetter<V> setter
    ) throws IOException {
        if (Files.notExists(splitPath)) {
            return;
        }
        Map<String, Object> map = readYamlMap(splitPath);
        map.forEach((k, v) -> {
            Long groupId = toGroupId(k);
            if (groupId == null || v == null) {
                return;
            }
            OtherConfig config = merged.computeIfAbsent(groupId, key -> new OtherConfig());
            setter.set(config, YAML_MAPPER.convertValue(v, type));
        });
    }

    private <V> void mergeGroupListField(Map<Long, OtherConfig> merged,
                                         Path splitPath,
                                         Class<V> elementType,
                                         FieldSetter<List<V>> setter
    ) throws IOException {
        if (Files.notExists(splitPath)) {
            return;
        }
        Map<String, Object> map = readYamlMap(splitPath);
        map.forEach((k, v) -> {
            Long groupId = toGroupId(k);
            if (groupId == null || v == null) {
                return;
            }
            OtherConfig config = merged.computeIfAbsent(groupId, key -> new OtherConfig());
            List<V> list = YAML_MAPPER.convertValue(v, YAML_MAPPER.getTypeFactory().constructCollectionType(List.class, elementType));
            setter.set(config, list);
        });
    }

    private Long toGroupId(Object key) {
        try {
            return Long.parseLong(String.valueOf(key));
        } catch (Exception ignored) {
            return null;
        }
    }

    private Map<String, Object> readYamlMap(Path path) throws IOException {
        if (Files.notExists(path) || Files.size(path) == 0L) {
            return new LinkedHashMap<>();
        }
        Map<String, Object> result = YAML_MAPPER.readValue(path.toFile(), new TypeReference<>() {
        });
        return result != null ? new LinkedHashMap<>(result) : new LinkedHashMap<>();
    }

    private void writeYamlValue(Path path, Object value) throws IOException {
        Files.createDirectories(path.toAbsolutePath().getParent());
        String yaml = YAML_MAPPER.writeValueAsString(value != null ? value : new LinkedHashMap<>());
        yaml = yaml.replaceAll("(?m): null(\\s|$)", ":$1");
        Files.writeString(path, yaml, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
    }

    private void writeYamlMap(Path path, Map<String, Object> map) throws IOException {
        writeYamlValue(path, map);
    }

    @FunctionalInterface
    private interface FieldSetter<V> {
        void set(OtherConfig config, V value);
    }
}
