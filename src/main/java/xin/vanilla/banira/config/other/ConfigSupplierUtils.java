package xin.vanilla.banira.config.other;

import xin.vanilla.banira.config.contract.GroupConfig;
import xin.vanilla.banira.config.contract.SharedConfig;
import xin.vanilla.banira.start.SpringContextHolder;

import java.util.Optional;
import java.util.function.Supplier;

/**
 * 配置 Supplier 快捷工具。
 */
public final class ConfigSupplierUtils {
    private ConfigSupplierUtils() {
    }

    private static OtherConfigRegistry registry() {
        return SpringContextHolder.getBean(OtherConfigRegistry.class);
    }

    /**
     * 获取共享配置 Supplier。
     */
    public static <T extends SharedConfig> Supplier<T> shared(Class<T> clazz) {
        return registry().sharedSupplier(clazz);
    }

    /**
     * 获取群配置 Supplier，优先群配置，不存在时回退群号0配置。
     */
    public static <T extends GroupConfig> Supplier<T> group(Class<T> clazz, Long groupId) {
        return registry().groupSupplier(clazz, groupId);
    }

    /**
     * 获取仅指定群配置的 Supplier，不回退全局配置。
     */
    public static <T extends GroupConfig> Supplier<Optional<T>> groupOnly(Class<T> clazz, Long groupId) {
        return registry().groupOnlySupplier(clazz, groupId);
    }
}
