package xin.vanilla.banira.plugin.chat;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import xin.vanilla.banira.config.entity.extended.ChatReplySettings;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.enums.EnumMessageType;

import java.util.List;

class ChatReplyContextPolicyTest {

    @Test
    void shouldQuoteCurrentMessageWhenGroupIsBusy() {
        ChatReplySettings settings = new ChatReplySettings()
                .busyGroupWindowSeconds(60)
                .busyGroupMessageThreshold(3)
                .busyGroupDistinctSenderThreshold(2);
        BaniraCodeContext ctx = context(100L, 123);
        StructuredReply reply = new StructuredReply("查到了", List.of());

        StructuredReply result = ChatReplyContextPolicy.applyBusyGroupQuote(ctx, List.of(
                record(91L, 1L),
                record(92L, 2L),
                record(93L, 1L)
        ), reply, settings);

        Assertions.assertEquals(123, result.replyToMessageId());
    }

    @Test
    void shouldNotQuoteWhenGroupIsQuiet() {
        ChatReplySettings settings = new ChatReplySettings()
                .busyGroupWindowSeconds(60)
                .busyGroupMessageThreshold(4)
                .busyGroupDistinctSenderThreshold(2);
        BaniraCodeContext ctx = context(100L, 123);
        StructuredReply reply = new StructuredReply("嗯", List.of());

        StructuredReply result = ChatReplyContextPolicy.applyBusyGroupQuote(ctx, List.of(
                record(91L, 1L),
                record(92L, 2L)
        ), reply, settings);

        Assertions.assertNull(result.replyToMessageId());
    }

    @Test
    void shouldNotOverrideModelSpecifiedReplyTarget() {
        ChatReplySettings settings = new ChatReplySettings()
                .busyGroupWindowSeconds(60)
                .busyGroupMessageThreshold(1)
                .busyGroupDistinctSenderThreshold(0);
        BaniraCodeContext ctx = context(100L, 123);
        StructuredReply reply = new StructuredReply("是这个", List.of(), 77, List.of(), false);

        StructuredReply result = ChatReplyContextPolicy.applyBusyGroupQuote(ctx, List.of(
                record(91L, 1L)
        ), reply, settings);

        Assertions.assertEquals(77, result.replyToMessageId());
    }

    @Test
    void shouldDetectDuplicateRecentBotReply() {
        BaniraCodeContext ctx = context(100L, 123).msg("笑死");
        StructuredReply reply = new StructuredReply("试试微软拼音？自带的，没广告，稳定", List.of());

        boolean duplicate = ChatReplyContextPolicy.isDuplicateRecentBotReply(ctx, List.of(
                record(91L, 999L).setMsgRecode("试试微软拼音？自带的，没广告，稳定")
        ), reply, 999L);

        Assertions.assertTrue(duplicate);
    }

    @Test
    void shouldNotTreatRequestedRepeatAsDuplicate() {
        BaniraCodeContext ctx = context(100L, 123).msg("刚才那个再说一下");
        StructuredReply reply = new StructuredReply("试试微软拼音？自带的，没广告，稳定", List.of());

        boolean duplicate = ChatReplyContextPolicy.isDuplicateRecentBotReply(ctx, List.of(
                record(91L, 999L).setMsgRecode("试试微软拼音？自带的，没广告，稳定")
        ), reply, 999L);

        Assertions.assertFalse(duplicate);
    }

    @Test
    void shouldNotTreatRetrySearchAsDuplicate() {
        BaniraCodeContext ctx = context(100L, 123).msg("@香草白茶 你再搜搜");
        StructuredReply reply = new StructuredReply("又搜了下，还是没看到可靠说法", List.of());

        boolean duplicate = ChatReplyContextPolicy.isDuplicateRecentBotReply(ctx, List.of(
                record(91L, 999L).setMsgRecode("又搜了下，还是没看到可靠说法")
        ), reply, 999L);

        Assertions.assertFalse(duplicate);
    }

    @Test
    void shouldSuppressStaleIdentityAnswerWhenCurrentMessageChangedTopic() {
        BaniraCodeContext ctx = context(100L, 123).msg("哪儿有自拍");
        StructuredReply reply = new StructuredReply("你是月酱，我的主人呀", List.of());

        Assertions.assertTrue(ChatReplyContextPolicy.isStaleIdentityAnswer(ctx, reply));
    }

    @Test
    void shouldAllowIdentityAnswerWhenCurrentMessageAsksIdentity() {
        BaniraCodeContext ctx = context(100L, 123).msg("我是谁");
        StructuredReply reply = new StructuredReply("你是我的主人呀", List.of());

        Assertions.assertFalse(ChatReplyContextPolicy.isStaleIdentityAnswer(ctx, reply));
    }

    private static BaniraCodeContext context(long time, int msgId) {
        return new BaniraCodeContext(null, List.of(), 1L, 2L, 2L)
                .msgType(EnumMessageType.GROUP)
                .time(time)
                .msgId(msgId);
    }

    private static MessageRecord record(long time, long sender) {
        return new MessageRecord()
                .setTime(time)
                .setSenderId(sender)
                .setMsgRecode("test");
    }
}
