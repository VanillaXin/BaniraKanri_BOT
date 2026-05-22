package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.plugin.common.BaniraBot;

@FunctionalInterface
interface ChatTurnCoalesceRunner {

    @Nonnull
    StructuredReply run(@Nonnull BaniraBot bot
            , @Nonnull BaniraCodeContext ctx
            , @Nonnull ChatTurnCoalesceState coalesceState
    );
}
