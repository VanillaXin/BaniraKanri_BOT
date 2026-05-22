package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.config.entity.extended.ChatEngagementSettings;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.enums.EnumMessageType;
import xin.vanilla.banira.plugin.common.BaniraBot;

import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

class ChatGroupTurnCoordinatorTest {

    @Test
    void mergesSupersededTurnIntoSinglePipelineRun() {
        ChatGroupTurnCoordinator coordinator = new ChatGroupTurnCoordinator();
        ChatConfig cfg = new ChatConfig()
                .engagement(new ChatEngagementSettings()
                        .enabled(true)
                        .turnCoalescingEnabled(true)
                        .coalesceRetryDelayMillis(0));
        BaniraBot bot = org.mockito.Mockito.mock(BaniraBot.class);
        org.mockito.Mockito.when(bot.getSelfId()).thenReturn(100L);

        AtomicInteger runs = new AtomicInteger();
        ChatTurnCoalesceRunner[] runnerHolder = new ChatTurnCoalesceRunner[1];
        runnerHolder[0] = (ignoredBot, ctx, coalesceState) -> {
            int count = runs.incrementAndGet();
            if (count == 1) {
                ChatTurnCoalesceOutcome followerOutcome = coordinator.run(
                        bot,
                        ctx(200L, 2, "补充一句"),
                        cfg,
                        runnerHolder[0]
                );
                Assertions.assertInstanceOf(ChatTurnCoalesceOutcome.Follower.class, followerOutcome);
                return new StructuredReply("旧答案", List.of());
            }
            Assertions.assertTrue(coalesceState.hasSupplementary());
            return new StructuredReply("合并后的答案", List.of());
        };

        ChatTurnCoalesceOutcome outcome = coordinator.run(bot, ctx(200L, 1, "先问一句"), cfg, runnerHolder[0]);
        Assertions.assertInstanceOf(ChatTurnCoalesceOutcome.Leader.class, outcome);
        ChatTurnCoalesceOutcome.Leader leader = (ChatTurnCoalesceOutcome.Leader) outcome;
        Assertions.assertEquals("合并后的答案", leader.reply().speech());
        Assertions.assertEquals(2, leader.replyContext().msgId());
        Assertions.assertEquals(2, runs.get());
    }

    @Test
    void keepsPreviousReplyWhenSupersedingTurnProducesNoReply() {
        ChatGroupTurnCoordinator coordinator = new ChatGroupTurnCoordinator();
        ChatConfig cfg = new ChatConfig()
                .engagement(new ChatEngagementSettings()
                        .enabled(true)
                        .turnCoalescingEnabled(true)
                        .coalesceRetryDelayMillis(0));
        BaniraBot bot = org.mockito.Mockito.mock(BaniraBot.class);
        org.mockito.Mockito.when(bot.getSelfId()).thenReturn(100L);

        AtomicInteger runs = new AtomicInteger();
        ChatTurnCoalesceRunner[] runnerHolder = new ChatTurnCoalesceRunner[1];
        runnerHolder[0] = (ignoredBot, ctx, coalesceState) -> {
            int count = runs.incrementAndGet();
            if (count == 1) {
                ChatTurnCoalesceOutcome followerOutcome = coordinator.run(
                        bot,
                        ctx(200L, 2, "[图片]"),
                        cfg,
                        runnerHolder[0]
                );
                Assertions.assertInstanceOf(ChatTurnCoalesceOutcome.Follower.class, followerOutcome);
                return new StructuredReply("上一版有效回复", List.of());
            }
            return null;
        };

        ChatTurnCoalesceOutcome outcome = coordinator.run(bot, ctx(200L, 1, "@白茶酱 你再搜搜"), cfg, runnerHolder[0]);

        Assertions.assertInstanceOf(ChatTurnCoalesceOutcome.Leader.class, outcome);
        ChatTurnCoalesceOutcome.Leader leader = (ChatTurnCoalesceOutcome.Leader) outcome;
        Assertions.assertEquals("上一版有效回复", leader.reply().speech());
        Assertions.assertEquals(1, leader.replyContext().msgId());
        Assertions.assertEquals(2, runs.get());
    }

    @Nonnull
    private static BaniraCodeContext ctx(long groupId, int msgId, String text) {
        return new BaniraCodeContext(null, List.of(), groupId, 2L, 2L)
                .msg(text)
                .msgId(msgId)
                .msgType(EnumMessageType.GROUP);
    }
}
