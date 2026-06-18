package xin.vanilla.banira.plugin.chat;

import com.mikuac.shiro.enums.MsgTypeEnum;
import com.mikuac.shiro.model.ArrayMsg;
import dev.langchain4j.data.message.ChatMessage;
import dev.langchain4j.data.message.ImageContent;
import dev.langchain4j.data.message.UserMessage;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.enums.EnumMessageType;
import xin.vanilla.banira.plugin.chat.agent.AgentContext;
import xin.vanilla.banira.plugin.chat.capability.AiCapabilityRegistry;
import xin.vanilla.banira.plugin.chat.memory.MemoryRetriever;
import xin.vanilla.banira.plugin.common.BaniraBot;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Base64;
import java.util.List;
import java.util.Map;

class ChatPromptAssemblerTest {

    private static final byte[] ONE_PIXEL_PNG = Base64.getDecoder().decode(
            "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+/p9sAAAAASUVORK5CYII="
    );

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

    @Test
    void shouldNotAttachRecentHistoryImagesBeforeToolRequest() throws Exception {
        Path image = Files.createTempFile("banira-chat-image", ".png");
        Files.write(image, ONE_PIXEL_PNG);
        BaniraBot bot = Mockito.mock(BaniraBot.class);
        Mockito.when(bot.getSelfId()).thenReturn(999L);
        ChatConfig cfg = new ChatConfig()
                .useDefaultPersonaPrompt(false);
        cfg.model().imageInputEnabled(true);
        cfg.memory().retrieveLimit(0);
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        Mockito.when(registry.available(Mockito.any(), Mockito.any())).thenReturn(List.of());
        MemoryRetriever memoryRetriever = Mockito.mock(MemoryRetriever.class);
        Mockito.when(memoryRetriever.retrieve(Mockito.any(), Mockito.any(), Mockito.anyString())).thenReturn(List.of());
        Mockito.when(memoryRetriever.format(Mockito.any(), Mockito.anyList())).thenReturn("");
        ChatPromptAssembler assembler = new ChatPromptAssembler(cfg, memoryRetriever, registry);
        BaniraCodeContext ctx = new BaniraCodeContext(bot, List.of(), 10001L, 123456L, 123456L)
                .msg("你仔细看看那张图")
                .msgId(31)
                .msgType(EnumMessageType.GROUP);
        MessageRecord imageRecord = record(30, 888888L, "[CQ:image,file=test.png,url=" + image.toUri() + "]");

        List<ChatMessage> prompt = assembler.build(
                bot,
                ctx,
                AgentContext.from(bot, ctx),
                List.of(imageRecord),
                false,
                0,
                ChatTurnCoalesceState.none()
        );

        Assertions.assertFalse(prompt.stream()
                .filter(UserMessage.class::isInstance)
                .map(UserMessage.class::cast)
                .flatMap(message -> message.contents().stream())
                .anyMatch(ImageContent.class::isInstance));
    }

    @Test
    void shouldUseOriginalMessageImageForCurrentTurn() throws Exception {
        Path image = Files.createTempFile("banira-current-image", ".png");
        Files.write(image, ONE_PIXEL_PNG);
        BaniraBot bot = Mockito.mock(BaniraBot.class);
        Mockito.when(bot.getSelfId()).thenReturn(999L);
        ChatConfig cfg = new ChatConfig()
                .useDefaultPersonaPrompt(false);
        cfg.model().imageInputEnabled(true);
        cfg.memory().retrieveLimit(0);
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        Mockito.when(registry.available(Mockito.any(), Mockito.any())).thenReturn(List.of());
        MemoryRetriever memoryRetriever = Mockito.mock(MemoryRetriever.class);
        Mockito.when(memoryRetriever.retrieve(Mockito.any(), Mockito.any(), Mockito.anyString())).thenReturn(List.of());
        Mockito.when(memoryRetriever.format(Mockito.any(), Mockito.anyList())).thenReturn("");
        ChatPromptAssembler assembler = new ChatPromptAssembler(cfg, memoryRetriever, registry);
        BaniraCodeContext ctx = new BaniraCodeContext(
                bot,
                List.of(imageArrayMsg(image), new ArrayMsg().setType(MsgTypeEnum.text).setData(Map.of("text", "看看这个"))),
                10001L,
                123456L,
                123456L
        )
                .msg("看看这个")
                .msgId(32)
                .msgType(EnumMessageType.GROUP);

        List<ChatMessage> prompt = assembler.build(
                bot,
                ctx,
                AgentContext.from(bot, ctx),
                List.of(),
                false,
                0,
                ChatTurnCoalesceState.none()
        );

        Assertions.assertTrue(prompt.stream()
                .filter(UserMessage.class::isInstance)
                .map(UserMessage.class::cast)
                .flatMap(message -> message.contents().stream())
                .anyMatch(ImageContent.class::isInstance));
    }

    private static ArrayMsg imageArrayMsg(Path image) {
        return new ArrayMsg()
                .setType(MsgTypeEnum.image)
                .setData(Map.of("file", image.getFileName().toString(), "url", image.toUri().toString()));
    }

    private static MessageRecord record(int msgId, long sender, String text) {
        return new MessageRecord()
                .setMsgId(String.valueOf(msgId))
                .setGroupId(10001L)
                .setSenderId(sender)
                .setMsgRecode(text);
    }
}
