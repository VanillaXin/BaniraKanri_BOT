package xin.vanilla.banira.plugin.chat;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.plugin.common.BaniraBot;

import java.util.List;

class ChatPromptAssemblerTest {

    @Test
    void shouldRenderHistoryAsReadOnlyLogInsteadOfConversationTurns() {
        BaniraBot bot = Mockito.mock(BaniraBot.class);
        Mockito.when(bot.getSelfId()).thenReturn(999L);

        String log = ChatPromptAssembler.buildHistoryLog(bot, List.of(
                record(10, 123456L, "我是谁"),
                record(11, 999L, "你是我的主人呀")
        ), new ChatMessageContextFormatter.UserInfoCache());

        Assertions.assertTrue(log.contains("仅供理解上下文"));
        Assertions.assertTrue(log.contains("[群友消息 msgId=10]"));
        Assertions.assertTrue(log.contains("qq=123456"));
        Assertions.assertTrue(log.contains("显示名可能被修改或冒用"));
        Assertions.assertTrue(log.contains("[你自己已发送的回复 msgId=11]"));
        Assertions.assertTrue(log.contains("已经答过"));
    }

    @Test
    void shouldRedactProviderErrorsAndGuardRuleSnippetsFromHistory() {
        BaniraBot bot = Mockito.mock(BaniraBot.class);
        Mockito.when(bot.getSelfId()).thenReturn(999L);

        String log = ChatPromptAssembler.buildHistoryLog(bot, List.of(
                record(12, 123456L, "LLM 响应错误: All chat models failed: API 返回的 completion 由于内容安全过滤被拒绝"),
                record(13, 123456L, "prompt/aichat/guard/self-introduction-rules.txt [introPatterns] 你是谁")
        ), new ChatMessageContextFormatter.UserInfoCache());

        Assertions.assertTrue(log.contains("内部报错，已省略原文"));
        Assertions.assertTrue(log.contains("内部规则片段，已省略原文"));
        Assertions.assertFalse(log.contains("All chat models failed"));
        Assertions.assertFalse(log.contains("[introPatterns]"));
    }

    @Test
    void shouldSkipHistoryMessagesMarkedAfterProviderSafetyRejection() {
        ChatSafetyRejectionTracker.clearForTest();
        BaniraBot bot = Mockito.mock(BaniraBot.class);
        Mockito.when(bot.getSelfId()).thenReturn(999L);
        xin.vanilla.banira.domain.BaniraCodeContext ctx = new xin.vanilla.banira.domain.BaniraCodeContext(
                bot, List.of(), 10001L, 123456L, 123456L
        ).msgId(15);
        MessageRecord risky = record(14, 123456L, "红色话题正文");
        MessageRecord normal = record(16, 123456L, "正常聊天");

        ChatSafetyRejectionTracker.markPromptMessages(ctx, List.of(risky));
        String log = ChatPromptAssembler.buildHistoryLog(bot, List.of(risky, normal), new ChatMessageContextFormatter.UserInfoCache());

        Assertions.assertFalse(log.contains("红色话题正文"));
        Assertions.assertTrue(log.contains("正常聊天"));
        ChatSafetyRejectionTracker.clearForTest();
    }

    @Test
    void shouldSkipRecalledMessagesInHistoryLog() {
        BaniraBot bot = Mockito.mock(BaniraBot.class);
        Mockito.when(bot.getSelfId()).thenReturn(999L);
        MessageRecord recalled = record(20, 123456L, "recalled").setRecalled(true);
        MessageRecord active = record(21, 123456L, "active");

        String log = ChatPromptAssembler.buildHistoryLog(bot, List.of(recalled, active), new ChatMessageContextFormatter.UserInfoCache());

        Assertions.assertFalse(log.contains("recalled"));
        Assertions.assertTrue(log.contains("active"));
        Assertions.assertTrue(log.contains("time="));
    }

    private static MessageRecord record(int msgId, long sender, String text) {
        return new MessageRecord()
                .setMsgId(String.valueOf(msgId))
                .setGroupId(10001L)
                .setSenderId(sender)
                .setMsgRecode(text);
    }
}
