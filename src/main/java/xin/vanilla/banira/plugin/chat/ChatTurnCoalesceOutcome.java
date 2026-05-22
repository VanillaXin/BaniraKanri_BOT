package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import xin.vanilla.banira.domain.BaniraCodeContext;

/**
 * 同群 LLM 合并执行结果。
 */
public sealed interface ChatTurnCoalesceOutcome permits ChatTurnCoalesceOutcome.Follower, ChatTurnCoalesceOutcome.Leader {

    @Nonnull
    static Follower follower() {
        return Follower.INSTANCE;
    }

    @Nonnull
    static Leader completed(@Nullable StructuredReply reply, @Nonnull BaniraCodeContext replyContext) {
        return new Leader(reply, replyContext);
    }

    record Follower() implements ChatTurnCoalesceOutcome {
        static final Follower INSTANCE = new Follower();
    }

    record Leader(@Nullable StructuredReply reply,
                  @Nonnull BaniraCodeContext replyContext) implements ChatTurnCoalesceOutcome {
    }
}
