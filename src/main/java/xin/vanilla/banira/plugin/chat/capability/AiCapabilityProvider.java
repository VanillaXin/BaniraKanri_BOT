package xin.vanilla.banira.plugin.chat.capability;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.util.List;

/**
 * 插件向 AI 注册可调用的能力
 */
public interface AiCapabilityProvider {

    default void registerAiCapabilities(@Nonnull List<AiCapability> capabilities, @Nullable Long groupId) {
    }

}
