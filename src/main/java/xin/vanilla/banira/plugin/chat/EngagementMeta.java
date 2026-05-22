package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;

/**
 * LLM 返回的参与决策元数据。
 */
public record EngagementMeta(boolean shouldReply, int interest) {

    @Nonnull
    public static EngagementMeta replyWithInterest(int interest) {
        return new EngagementMeta(true, clampInterest(interest));
    }

    @Nonnull
    public static EngagementMeta silentWithInterest(int interest) {
        return new EngagementMeta(false, clampInterest(interest));
    }

    @Nonnull
    public static EngagementMeta inferFromSpeech(@Nonnull String speech) {
        if (speech.isBlank()) {
            return silentWithInterest(0);
        }
        return replyWithInterest(0);
    }

    public static int clampInterest(int interest) {
        return Math.clamp(interest, 0, 100);
    }
}
