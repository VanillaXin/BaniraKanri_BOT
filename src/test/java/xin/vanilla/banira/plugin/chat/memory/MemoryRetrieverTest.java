package xin.vanilla.banira.plugin.chat.memory;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.domain.AiMemory;
import xin.vanilla.banira.enums.EnumMessageType;
import xin.vanilla.banira.plugin.chat.agent.AgentContext;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.service.IAiMemoryManager;

import java.util.List;

class MemoryRetrieverTest {

    @Test
    void shouldUseSemanticMemoryBeforeKeywordFallback() {
        MemoryRetriever retriever = new MemoryRetriever();
        IAiMemoryManager memoryManager = Mockito.mock(IAiMemoryManager.class);
        MemoryEmbeddingService embeddingService = Mockito.mock(MemoryEmbeddingService.class);
        ReflectionTestUtils.setField(retriever, "aiMemoryManager", memoryManager);
        ReflectionTestUtils.setField(retriever, "memoryEmbeddingService", embeddingService);

        ChatConfig cfg = new ChatConfig();
        cfg.memory().keywordFallbackEnabled(false);
        AiMemory memory = new AiMemory()
                .setId(1L)
                .setBotId(100L)
                .setGroupId(200L)
                .setUserId(300L)
                .setContent("用户喜欢短句回复")
                .setTags("source:test")
                .setLastUsedAt(1L);
        AgentContext ctx = context();
        Mockito.when(embeddingService.retrieve(Mockito.eq(ctx), Mockito.eq(cfg), Mockito.anyString(), Mockito.eq(memoryManager)))
                .thenReturn(List.of(new MemoryEmbeddingService.SemanticMemoryMatch(memory, 0.92)));

        List<AiMemory> result = retriever.retrieve(ctx, cfg, "说话风格偏好");

        Assertions.assertEquals(1, result.size());
        Assertions.assertEquals("用户喜欢短句回复", result.getFirst().getContent());
        Mockito.verify(memoryManager).touchMemory(Mockito.eq(1L), Mockito.anyLong());
    }

    @Test
    void shouldFormatMemoryWithExplicitScope() {
        MemoryRetriever retriever = new MemoryRetriever();
        AgentContext ctx = context();

        String formatted = retriever.format(ctx, List.of(
                new AiMemory()
                        .setId(1L)
                        .setUserId(300L)
                        .setContent("当前用户希望被叫月酱")
                        .setTags("source:auto"),
                new AiMemory()
                        .setId(2L)
                        .setUserId(0L)
                        .setContent("主人账号希望你称呼主人账号为「月酱」")
                        .setTags("behavior,owner_instruction,source:owner"),
                new AiMemory()
                        .setId(3L)
                        .setUserId(444L)
                        .setContent("萝卜籽喜欢绕弯子")
                        .setTags("source:auto")
        ));

        Assertions.assertTrue(formatted.contains("[当前发言者 qq=300 的记忆]"));
        Assertions.assertTrue(formatted.contains("[主人长期偏好"));
        Assertions.assertTrue(formatted.contains("不要把这条套用成当前发言者身份"));
        Assertions.assertTrue(formatted.contains("[其他用户 qq=444 的记忆"));
    }

    @Test
    void shouldDisplayLegacyOwnerNamingMemoryAsExplicitSubject() {
        MemoryRetriever retriever = new MemoryRetriever();

        String formatted = retriever.format(context(), List.of(
                new AiMemory()
                        .setId(1L)
                        .setUserId(0L)
                        .setContent("主人对你的长期行为要求：白茶酱，以后叫我月酱，别叫我主人")
                        .setTags("behavior,owner_instruction,source:owner")
        ));

        Assertions.assertTrue(formatted.contains("称呼主人账号为「月酱」"));
        Assertions.assertTrue(formatted.contains("不要称呼主人账号为「主人」"));
        Assertions.assertFalse(formatted.contains("以后叫我"));
    }

    @Test
    void shouldFormatLowImportanceMemorySeparately() {
        MemoryRetriever retriever = new MemoryRetriever();

        String formatted = retriever.format(context(), List.of(
                new AiMemory()
                        .setId(1L)
                        .setUserId(0L)
                        .setContent("你曾在本群回复过：「这不是长期结论」")
                        .setTags("type:episodic,importance:low,source:auto_reply,bot_said")
        ));

        Assertions.assertTrue(formatted.contains("轻量会话记忆"));
        Assertions.assertTrue(formatted.contains("你曾说过"));
        Assertions.assertTrue(formatted.contains("不是长期设定"));
    }

    private static AgentContext context() {
        BaniraBot bot = Mockito.mock(BaniraBot.class);
        Mockito.when(bot.getSelfId()).thenReturn(100L);
        return new AgentContext()
                .bot(bot)
                .groupId(200L)
                .senderId(300L)
                .msgType(EnumMessageType.GROUP)
                .userMessage("说话风格偏好");
    }
}
