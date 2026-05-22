package xin.vanilla.banira.plugin.chat;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import xin.vanilla.banira.config.entity.extended.ChatAffinitySettings;
import xin.vanilla.banira.config.entity.extended.ChatReplySettings;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.enums.EnumMessageType;

import java.util.List;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;

class ReplyDecisionMakerTest {

    @Test
    void directMentionShouldHaveCertainReplyProbabilityEvenWithLowAffinity() {
        ChatAffinitySettings affinity = new ChatAffinitySettings()
                .enabled(true)
                .veryLowThreshold(20)
                .veryLowReplyMultiplier(0.1);
        ReplyDecisionMaker decisionMaker = new ReplyDecisionMaker(new ChatReplySettings(), affinity);
        BaniraCodeContext ctx = new BaniraCodeContext(null, List.of(), 10001L, 20002L, 20002L)
                .msg("在吗")
                .msgType(EnumMessageType.GROUP);

        Assertions.assertEquals(1.0, decisionMaker.computeReplyProbabilityForTest(ctx, true, false, 0));
    }

    @Test
    void strongNamePrefixTriggerShouldHaveCertainReplyProbabilityEvenWithLowAffinity() {
        ChatAffinitySettings affinity = new ChatAffinitySettings()
                .enabled(true)
                .veryLowThreshold(20)
                .veryLowReplyMultiplier(0.1);
        ReplyDecisionMaker decisionMaker = new ReplyDecisionMaker(new ChatReplySettings(), affinity);
        BaniraCodeContext ctx = new BaniraCodeContext(null, List.of(), 10001L, 20002L, 20002L)
                .msg("月见你在吗")
                .msgType(EnumMessageType.GROUP);

        Assertions.assertEquals(1.0, decisionMaker.computeReplyProbabilityForTest(ctx, true, true, 0));
    }

    @Test
    void ordinaryLowInformationMessageShouldNotRandomlyTrigger() {
        ReplyDecisionMaker decisionMaker = new ReplyDecisionMaker(new ChatReplySettings(), new ChatAffinitySettings());
        BaniraCodeContext ctx = new BaniraCodeContext(null, List.of(), 10001L, 20002L, 20002L)
                .msg("笑死")
                .msgType(EnumMessageType.GROUP);

        Assertions.assertEquals(0.0, decisionMaker.computeReplyProbabilityForTest(ctx, false, false, 50));
    }

    @Test
    void ordinaryQuestionShouldHaveLowButPossibleProbability() {
        ReplyDecisionMaker decisionMaker = new ReplyDecisionMaker(new ChatReplySettings(), new ChatAffinitySettings());
        BaniraCodeContext ctx = new BaniraCodeContext(null, List.of(), 10001L, 20002L, 20002L)
                .msg("这个怎么弄？")
                .msgType(EnumMessageType.GROUP);

        double probability = decisionMaker.computeReplyProbabilityForTest(ctx, false, false, 50);
        Assertions.assertTrue(probability > 0.0 && probability <= 0.20);
    }

    @Test
    void strongMentionShouldHaveLargerBurstLimit() {
        ChatReplySettings settings = new ChatReplySettings().perTargetRateLimitPerMinute(2);
        ReplyDecisionMaker decisionMaker = new ReplyDecisionMaker(
                settings,
                new ChatAffinitySettings(),
                Clock.fixed(Instant.parse("2026-05-28T00:00:00Z"), ZoneOffset.UTC)
        );
        BaniraCodeContext ctx = new BaniraCodeContext(null, List.of(), 10001L, 20002L, 20002L)
                .msg("@香草白茶 你再搜搜")
                .msgType(EnumMessageType.GROUP);

        Assertions.assertTrue(decisionMaker.tryAcquireReplySlot(ctx, true));
        Assertions.assertTrue(decisionMaker.tryAcquireReplySlot(ctx, true));
        Assertions.assertTrue(decisionMaker.tryAcquireReplySlot(ctx, true));
        Assertions.assertTrue(decisionMaker.tryAcquireReplySlot(ctx, true));
        Assertions.assertFalse(decisionMaker.tryAcquireReplySlot(ctx, true));
    }
}
