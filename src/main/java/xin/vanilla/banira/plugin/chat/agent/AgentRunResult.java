package xin.vanilla.banira.plugin.chat.agent;

import jakarta.annotation.Nonnull;

import java.util.List;

/**
 * Agent 单次运行结果
 */
public record AgentRunResult(@Nonnull String speechText, @Nonnull List<String> toolReferences, boolean directHandled) {

    public AgentRunResult(@Nonnull String speechText, @Nonnull List<String> toolReferences) {
        this(speechText, toolReferences, false);
    }
}
