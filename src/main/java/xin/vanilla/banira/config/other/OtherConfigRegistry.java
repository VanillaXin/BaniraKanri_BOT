package xin.vanilla.banira.config.other;

import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.YamlConfigManager;
import xin.vanilla.banira.config.YamlConfigWatcherService;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
@Component
public class OtherConfigRegistry {
    private final Map<String, OtherConfigDefinition<?>> definitionMap = new HashMap<>();
    private final Map<String, YamlConfigManager<?>> sharedManagers = new HashMap<>();
    private final Map<String, GroupedYamlConfigManager<?>> groupedManagers = new HashMap<>();

    public OtherConfigRegistry(List<OtherConfigDefinition<?>> definitions,
                               YamlConfigWatcherService watcherService,
                               ApplicationEventPublisher publisher
    ) throws Exception {
        for (OtherConfigDefinition<?> definition : definitions) {
            definition.validate();
            String key = definition.key();
            if (definitionMap.containsKey(key)) {
                throw new IllegalStateException("Duplicated other config key: " + key);
            }
            definitionMap.put(key, definition);
            if (definition.scope() == OtherConfigScope.SHARED) {
                sharedManagers.put(key, buildSharedManager(definition, watcherService, publisher));
            } else {
                groupedManagers.put(key, buildGroupedManager(definition, watcherService));
            }
        }
        LOGGER.info("Loaded {} other config definitions", definitionMap.size());
    }

    private <T> YamlConfigManager<T> buildSharedManager(OtherConfigDefinition<T> definition,
                                                        YamlConfigWatcherService watcherService,
                                                        ApplicationEventPublisher publisher
    ) throws Exception {
        Path path = Paths.get("./config/other/shared/" + definition.key() + ".yml");
        return new YamlConfigManager<>(
                path,
                definition.defaultSupplier().get(),
                definition.type(),
                "other-shared-" + definition.key(),
                watcherService,
                publisher
        );
    }

    private <T> GroupedYamlConfigManager<T> buildGroupedManager(OtherConfigDefinition<T> definition,
                                                                YamlConfigWatcherService watcherService
    ) throws Exception {
        Path path = Paths.get("./config/other/grouped/" + definition.key() + ".yml");
        return new GroupedYamlConfigManager<>(
                path,
                definition.type(),
                definition.defaultSupplier().get(),
                watcherService
        );
    }

    public <T> T getShared(String key, Class<T> clazz) {
        OtherConfigDefinition<?> definition = requiredDefinition(key, OtherConfigScope.SHARED);
        ensureType(definition, clazz);
        YamlConfigManager<?> manager = sharedManagers.get(key);
        return clazz.cast(manager.getCurrent());
    }

    public <T> T getGrouped(String key, Long groupId, Class<T> clazz) {
        OtherConfigDefinition<?> definition = requiredDefinition(key, OtherConfigScope.GROUPED);
        ensureType(definition, clazz);
        GroupedYamlConfigManager<?> manager = groupedManagers.get(key);
        return clazz.cast(manager.get(groupId));
    }

    public boolean hasGrouped(String key, Long groupId) {
        requiredDefinition(key, OtherConfigScope.GROUPED);
        GroupedYamlConfigManager<?> manager = groupedManagers.get(key);
        return manager.contains(groupId);
    }

    public <T> Map<Long, T> getGroupedSnapshot(String key, Class<T> clazz) {
        OtherConfigDefinition<?> definition = requiredDefinition(key, OtherConfigScope.GROUPED);
        ensureType(definition, clazz);
        GroupedYamlConfigManager<?> manager = groupedManagers.get(key);
        Map<Long, ?> snapshot = manager.snapshot();
        Map<Long, T> result = new HashMap<>();
        snapshot.forEach((k, v) -> result.put(k, clazz.cast(v)));
        return result;
    }

    public void saveShared(String key) throws Exception {
        requiredDefinition(key, OtherConfigScope.SHARED);
        YamlConfigManager<?> manager = sharedManagers.get(key);
        manager.save();
    }

    public void saveGrouped(String key) throws Exception {
        requiredDefinition(key, OtherConfigScope.GROUPED);
        GroupedYamlConfigManager<?> manager = groupedManagers.get(key);
        manager.save();
    }

    private OtherConfigDefinition<?> requiredDefinition(String key, OtherConfigScope scope) {
        OtherConfigDefinition<?> definition = definitionMap.get(key);
        if (definition == null) {
            throw new IllegalArgumentException("Unknown other config key: " + key);
        }
        if (definition.scope() != scope) {
            throw new IllegalArgumentException("Other config key scope mismatch, key=" + key + ", expected=" + scope);
        }
        return definition;
    }

    private <T> void ensureType(OtherConfigDefinition<?> definition, Class<T> clazz) {
        if (!clazz.isAssignableFrom(definition.type())) {
            throw new IllegalArgumentException("Other config type mismatch, key=" + definition.key() + ", expected=" + clazz.getName() + ", actual=" + definition.type().getName());
        }
    }
}
