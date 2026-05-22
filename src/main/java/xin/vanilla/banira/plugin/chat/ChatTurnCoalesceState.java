package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;
import xin.vanilla.banira.domain.BaniraCodeContext;

import java.util.List;

/**
 * 同群 LLM 合并执行时的上下文。
 */
public record ChatTurnCoalesceState(int retryAttempt, @Nonnull List<BaniraCodeContext> supplementaryMessages) {

    @Nonnull
    public static ChatTurnCoalesceState none() {
        return new ChatTurnCoalesceState(0, List.of());
    }

    public boolean hasSupplementary() {
        return retryAttempt > 0 || !supplementaryMessages.isEmpty();
    }
}
