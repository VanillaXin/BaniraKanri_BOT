package xin.vanilla.banira.plugin.chat;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import jakarta.annotation.Nonnull;
import xin.vanilla.banira.config.entity.extended.ChatEngagementSettings;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.enums.EnumMessageType;

import java.util.List;

class ChatPreflightRunnerTest {

    private static final long BOT_ID = 10001L;
    private static final long GROUP_ID = 243186411L;

    @Test
    void shouldSkipPreflightForConfirmationText() {
        ChatEngagementSettings settings = new ChatEngagementSettings().enabled(true).preflightEnabled(true);
        BaniraCodeContext ctx = groupContext("yes!");

        boolean needs = ChatPreflightRunner.needsPreflight(ctx, false, false, false, settings, 0, List.of(), BOT_ID);

        Assertions.assertFalse(needs);
    }

    @Test
    void shouldSkipPreflightWhenBotAwaitingConfirmation() {
        ChatEngagementSettings settings = new ChatEngagementSettings().enabled(true).preflightEnabled(true);
        MessageRecord botAsk = new MessageRecord()
                .setSenderId(BOT_ID)
                .setMsgRecode("现在帮你处理，禁言10分钟，行吗？");
        BaniraCodeContext ctx = groupContext("yes!");

        boolean needs = ChatPreflightRunner.needsPreflight(
                ctx, false, false, false, settings, 0, List.of(botAsk), BOT_ID);

        Assertions.assertFalse(needs);
    }

    @Test
    void shouldSkipPreflightWhenFollowInterestIsHigh() {
        ChatEngagementSettings settings = new ChatEngagementSettings()
                .enabled(true)
                .preflightEnabled(true)
                .followInterestThreshold(50);
        BaniraCodeContext ctx = groupContext("然后呢");

        boolean needs = ChatPreflightRunner.needsPreflight(ctx, false, false, false, settings, 55, List.of(), BOT_ID);

        Assertions.assertFalse(needs);
    }

    @Test
    void shouldRunPreflightForUnrelatedChatter() {
        ChatEngagementSettings settings = new ChatEngagementSettings().enabled(true).preflightEnabled(true);
        BaniraCodeContext ctx = groupContext("今天吃啥");

        boolean needs = ChatPreflightRunner.needsPreflight(ctx, false, false, false, settings, 0, List.of(), BOT_ID);

        Assertions.assertTrue(needs);
    }

    @Test
    void shouldRunPreflightForPlainBotNameMention() {
        ChatEngagementSettings settings = new ChatEngagementSettings().enabled(true).preflightEnabled(true);
        BaniraCodeContext ctx = groupContext("白茶酱终于找到答案了");

        boolean needs = ChatPreflightRunner.needsPreflight(ctx, false, true, false, settings, 0, List.of(), BOT_ID);

        Assertions.assertTrue(needs);
    }

    @Test
    void shouldSkipPreflightForNameMentionedRequestAfterMedia() {
        ChatEngagementSettings settings = new ChatEngagementSettings().enabled(true).preflightEnabled(true);
        BaniraCodeContext ctx = groupContext("[CQ:image,file=abc.jpg]白茶酱锐评一下");

        boolean needs = ChatPreflightRunner.needsPreflight(ctx, false, true, false, settings, 0, List.of(), BOT_ID);

        Assertions.assertFalse(needs);
    }

    @Test
    void shouldSkipPreflightForBotNamePrefix() {
        ChatEngagementSettings settings = new ChatEngagementSettings().enabled(true).preflightEnabled(true);
        BaniraCodeContext ctx = groupContext("白茶酱 你搜搜");

        boolean needs = ChatPreflightRunner.needsPreflight(ctx, false, true, true, settings, 0, List.of(), BOT_ID);

        Assertions.assertFalse(needs);
    }

    @Nonnull
    private static BaniraCodeContext groupContext(@Nonnull String text) {
        return new BaniraCodeContext(null, List.of(), GROUP_ID, 2L, 2L)
                .msg(text)
                .msgType(EnumMessageType.GROUP);
    }
}
