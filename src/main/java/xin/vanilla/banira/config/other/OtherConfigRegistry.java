package xin.vanilla.banira.config.other;

import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider;
import org.springframework.core.type.filter.AssignableTypeFilter;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.YamlConfigManager;
import xin.vanilla.banira.config.YamlConfigWatcherService;
import xin.vanilla.banira.config.contract.GroupConfig;
import xin.vanilla.banira.config.contract.SharedConfig;

import java.lang.reflect.Modifier;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Supplier;

/**
 * 其他配置注册中心。
 * 自动扫描 SharedConfig / GroupConfig 实现类并统一托管。
 */
@Slf4j
@Component
public class OtherConfigRegistry {
    private static final String CONFIG_SCAN_PACKAGE = "xin.vanilla.banira";

    private final Map<Class<?>, YamlConfigManager<?>> sharedManagers = new HashMap<>();
    private final Map<Class<?>, GroupedYamlConfigManager<?>> groupedManagers = new HashMap<>();

    public OtherConfigRegistry(
            YamlConfigWatcherService watcherService,
            ApplicationEventPublisher publisher
    ) throws Exception {
        for (Class<? extends SharedConfig> sharedType : scanManagedTypes(SharedConfig.class)) {
            sharedManagers.put(sharedType, buildSharedManager(sharedType, watcherService, publisher));
        }
        for (Class<? extends GroupConfig> groupedType : scanManagedTypes(GroupConfig.class)) {
            groupedManagers.put(groupedType, buildGroupedManager(groupedType, watcherService));
        }
        LOGGER.info("Loaded shared configs: {}, group configs: {}", sharedManagers.size(), groupedManagers.size());
    }

    private <T extends SharedConfig> YamlConfigManager<T> buildSharedManager(Class<T> type,
                                                                             YamlConfigWatcherService watcherService,
                                                                             ApplicationEventPublisher publisher
    ) throws Exception {
        Path path = Paths.get("./config/shared/" + toKebabFileName(type) + ".yml");
        return new YamlConfigManager<>(
                path,
                createDefault(type),
                type,
                "shared-" + type.getSimpleName(),
                watcherService,
                publisher
        );
    }

    private <T extends GroupConfig> GroupedYamlConfigManager<T> buildGroupedManager(Class<T> type,
                                                                                    YamlConfigWatcherService watcherService
    ) throws Exception {
        Path path = Paths.get("./config/group/" + toKebabFileName(type) + ".yml");
        return new GroupedYamlConfigManager<>(
                path,
                type,
                createDefault(type),
                watcherService
        );
    }

    public <T extends SharedConfig> T getShared(Class<T> clazz) {
        YamlConfigManager<?> manager = requireSharedManager(clazz);
        return clazz.cast(manager.getCurrent());
    }

    /**
     * 获取群配置（群优先，缺失时回退群0配置）。
     */
    public <T extends GroupConfig> T getGroupOrGlobal(Class<T> clazz, Long groupId) {
        GroupedYamlConfigManager<?> manager = requireGroupedManager(clazz);
        return clazz.cast(manager.getOrGlobal(groupId));
    }

    /**
     * 获取群配置用于写入（不存在时基于群0配置创建独立副本）。
     */
    public <T extends GroupConfig> T getGroupOrCreateFromGlobal(Class<T> clazz, Long groupId) {
        GroupedYamlConfigManager<?> manager = requireGroupedManager(clazz);
        return clazz.cast(manager.getOrCreateFromGlobal(groupId));
    }

    public <T extends GroupConfig> T getGrouped(Class<T> clazz, Long groupId) {
        GroupedYamlConfigManager<?> manager = requireGroupedManager(clazz);
        return clazz.cast(manager.get(groupId));
    }

    public <T extends GroupConfig> Optional<T> getGroupedOnly(Class<T> clazz, Long groupId) {
        GroupedYamlConfigManager<?> manager = requireGroupedManager(clazz);
        return manager.getOnly(groupId).map(clazz::cast);
    }

    public <T extends GroupConfig> boolean hasGrouped(Class<T> clazz, Long groupId) {
        GroupedYamlConfigManager<?> manager = requireGroupedManager(clazz);
        return manager.contains(groupId);
    }

    public <T extends GroupConfig> Map<Long, T> getGroupedSnapshot(Class<T> clazz) {
        GroupedYamlConfigManager<?> manager = requireGroupedManager(clazz);
        Map<Long, ?> snapshot = manager.snapshot();
        Map<Long, T> result = new HashMap<>();
        snapshot.forEach((k, v) -> result.put(k, clazz.cast(v)));
        return result;
    }

    /**
     * 共享配置 Supplier。
     */
    public <T extends SharedConfig> Supplier<T> sharedSupplier(Class<T> clazz) {
        return () -> getShared(clazz);
    }

    /**
     * 群配置 Supplier（群优先，缺失回退0群）。
     */
    public <T extends GroupConfig> Supplier<T> groupSupplier(Class<T> clazz, Long groupId) {
        return () -> getGroupOrGlobal(clazz, groupId);
    }

    /**
     * 群配置 Supplier（仅指定群，不回退）。
     */
    public <T extends GroupConfig> Supplier<Optional<T>> groupOnlySupplier(Class<T> clazz, Long groupId) {
        return () -> getGroupedOnly(clazz, groupId);
    }

    public <T extends SharedConfig> void saveShared(Class<T> clazz) throws Exception {
        YamlConfigManager<?> manager = requireSharedManager(clazz);
        manager.save();
    }

    public <T extends GroupConfig> void saveGrouped(Class<T> clazz) throws Exception {
        GroupedYamlConfigManager<?> manager = requireGroupedManager(clazz);
        manager.save();
    }

    private <T extends SharedConfig> YamlConfigManager<?> requireSharedManager(Class<T> clazz) {
        YamlConfigManager<?> manager = sharedManagers.get(clazz);
        if (manager == null) {
            throw new IllegalArgumentException("Unknown shared config type: " + clazz.getName());
        }
        return manager;
    }

    private <T extends GroupConfig> GroupedYamlConfigManager<?> requireGroupedManager(Class<T> clazz) {
        GroupedYamlConfigManager<?> manager = groupedManagers.get(clazz);
        if (manager == null) {
            throw new IllegalArgumentException("Unknown group config type: " + clazz.getName());
        }
        return manager;
    }

    private <T> T createDefault(Class<T> type) throws Exception {
        return type.getDeclaredConstructor().newInstance();
    }

    /**
     * 扫描可托管配置类型。
     */
    @SuppressWarnings("unchecked")
    private <T> java.util.List<Class<? extends T>> scanManagedTypes(Class<T> assignableType) throws Exception {
        java.util.List<Class<? extends T>> result = new ArrayList<>();
        ClassPathScanningCandidateComponentProvider scanner = new ClassPathScanningCandidateComponentProvider(false);
        scanner.addIncludeFilter(new AssignableTypeFilter(assignableType));
        for (var candidate : scanner.findCandidateComponents(CONFIG_SCAN_PACKAGE)) {
            Class<?> type = Class.forName(candidate.getBeanClassName());
            if (type.isInterface() || Modifier.isAbstract(type.getModifiers())) {
                continue;
            }
            if (!assignableType.isAssignableFrom(type)) {
                continue;
            }
            result.add((Class<? extends T>) type);
        }
        return result;
    }

    private String toKebabFileName(Class<?> type) {
        String name = type.getSimpleName();
        String kebab = name.replaceAll("([a-z])([A-Z])", "$1-$2").toLowerCase();
        return kebab;
    }
}
