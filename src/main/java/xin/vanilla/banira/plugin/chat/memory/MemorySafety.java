package xin.vanilla.banira.plugin.chat.memory;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.config.entity.extended.ChatMemorySettings;
import xin.vanilla.banira.plugin.chat.ChatGuardService;

/**
 * 记忆写入前的基础清洗，避免把指令注入和密钥当成长期事实。
 */
public final class MemorySafety {

    private MemorySafety() {
    }

    @Nonnull
    public static String normalize(@Nullable String content, @Nonnull ChatMemorySettings settings) {
        return ChatGuardService.defaults().normalizeMemory(content, settings);
    }

    @Nonnull
    public static String normalize(@Nullable String content, @Nonnull ChatConfig cfg) {
        return ChatGuardService.from(cfg).normalizeMemory(content, cfg.memory());
    }

    public static boolean isSafeToStore(@Nullable String content, @Nonnull ChatMemorySettings settings) {
        return ChatGuardService.defaults().isSafeToStore(content, settings);
    }

    public static boolean isSafeToStore(@Nullable String content, @Nonnull ChatConfig cfg) {
        return ChatGuardService.from(cfg).isSafeToStore(content, cfg.memory());
    }

}
