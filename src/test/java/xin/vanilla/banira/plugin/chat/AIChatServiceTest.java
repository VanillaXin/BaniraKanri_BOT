package xin.vanilla.banira.plugin.chat;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.plugin.chat.capability.AiCapabilityRegistry;
import xin.vanilla.banira.plugin.chat.memory.MemoryEmbeddingService;
import xin.vanilla.banira.plugin.chat.memory.MemoryExtractor;
import xin.vanilla.banira.plugin.chat.memory.MemoryRetriever;
import xin.vanilla.banira.plugin.chat.model.ChatQuotaService;
import xin.vanilla.banira.plugin.kanri.KanriService;
import xin.vanilla.banira.service.IAiMemoryManager;
import xin.vanilla.banira.service.IMessageRecordManager;

import java.util.List;

class AIChatServiceTest {

    @Test
    void shouldRejectNullDependencies() {
        IMessageRecordManager messageRecordManager = Mockito.mock(IMessageRecordManager.class);
        AiCapabilityRegistry capabilityRegistry = Mockito.mock(AiCapabilityRegistry.class);
        MemoryRetriever memoryRetriever = Mockito.mock(MemoryRetriever.class);
        MemoryExtractor memoryExtractor = Mockito.mock(MemoryExtractor.class);
        MemoryEmbeddingService memoryEmbeddingService = Mockito.mock(MemoryEmbeddingService.class);
        IAiMemoryManager aiMemoryManager = Mockito.mock(IAiMemoryManager.class);
        ChatAffinityService affinityService = Mockito.mock(ChatAffinityService.class);
        ChatQuotaService chatQuotaService = Mockito.mock(ChatQuotaService.class);
        ChatEngagementService engagementService = Mockito.mock(ChatEngagementService.class);
        ChatGroupTurnCoordinator turnCoordinator = Mockito.mock(ChatGroupTurnCoordinator.class);
        KanriService kanriService = Mockito.mock(KanriService.class);
        ChatConfig cfg = new ChatConfig();
        Assertions.assertThrows(NullPointerException.class, () -> new AIChatService(null, messageRecordManager, capabilityRegistry, memoryRetriever, memoryExtractor, memoryEmbeddingService, aiMemoryManager, affinityService, chatQuotaService, engagementService, turnCoordinator, kanriService));
        Assertions.assertThrows(NullPointerException.class, () -> new AIChatService(cfg, null, capabilityRegistry, memoryRetriever, memoryExtractor, memoryEmbeddingService, aiMemoryManager, affinityService, chatQuotaService, engagementService, turnCoordinator, kanriService));
        Assertions.assertThrows(NullPointerException.class, () -> new AIChatService(cfg, messageRecordManager, null, memoryRetriever, memoryExtractor, memoryEmbeddingService, aiMemoryManager, affinityService, chatQuotaService, engagementService, turnCoordinator, kanriService));
        Assertions.assertThrows(NullPointerException.class, () -> new AIChatService(cfg, messageRecordManager, capabilityRegistry, null, memoryExtractor, memoryEmbeddingService, aiMemoryManager, affinityService, chatQuotaService, engagementService, turnCoordinator, kanriService));
        Assertions.assertThrows(NullPointerException.class, () -> new AIChatService(cfg, messageRecordManager, capabilityRegistry, memoryRetriever, null, memoryEmbeddingService, aiMemoryManager, affinityService, chatQuotaService, engagementService, turnCoordinator, kanriService));
        Assertions.assertThrows(NullPointerException.class, () -> new AIChatService(cfg, messageRecordManager, capabilityRegistry, memoryRetriever, memoryExtractor, null, aiMemoryManager, affinityService, chatQuotaService, engagementService, turnCoordinator, kanriService));
        Assertions.assertThrows(NullPointerException.class, () -> new AIChatService(cfg, messageRecordManager, capabilityRegistry, memoryRetriever, memoryExtractor, memoryEmbeddingService, null, affinityService, chatQuotaService, engagementService, turnCoordinator, kanriService));
        Assertions.assertThrows(NullPointerException.class, () -> new AIChatService(cfg, messageRecordManager, capabilityRegistry, memoryRetriever, memoryExtractor, memoryEmbeddingService, aiMemoryManager, null, chatQuotaService, engagementService, turnCoordinator, kanriService));
        Assertions.assertThrows(NullPointerException.class, () -> new AIChatService(cfg, messageRecordManager, capabilityRegistry, memoryRetriever, memoryExtractor, memoryEmbeddingService, aiMemoryManager, affinityService, null, engagementService, turnCoordinator, kanriService));
        Assertions.assertThrows(NullPointerException.class, () -> new AIChatService(cfg, messageRecordManager, capabilityRegistry, memoryRetriever, memoryExtractor, memoryEmbeddingService, aiMemoryManager, affinityService, chatQuotaService, null, turnCoordinator, kanriService));
        Assertions.assertThrows(NullPointerException.class, () -> new AIChatService(cfg, messageRecordManager, capabilityRegistry, memoryRetriever, memoryExtractor, memoryEmbeddingService, aiMemoryManager, affinityService, chatQuotaService, engagementService, null, kanriService));
        Assertions.assertThrows(NullPointerException.class, () -> new AIChatService(cfg, messageRecordManager, capabilityRegistry, memoryRetriever, memoryExtractor, memoryEmbeddingService, aiMemoryManager, affinityService, chatQuotaService, engagementService, turnCoordinator, null));
    }

    @Test
    void shouldConvertRawWebSearchDumpIntoShortSpeechWithoutReference() {
        ChatResponseSanitizer sanitizer = new ChatResponseSanitizer(new ChatConfig(), ChatGuardService.defaults());

        StructuredReply reply = sanitizer.sanitize(
                new xin.vanilla.banira.domain.BaniraCodeContext(null, List.of(), 1L, 2L, 2L).msg("查一下"),
                "网页搜索结果：\n1. YouTube Help\nLearn more",
                List.of("1. useful source")
        );

        Assertions.assertFalse(reply.speech().startsWith("网页搜索结果"));
        Assertions.assertTrue(reply.references().isEmpty());
    }

    @Test
    void shouldConvertQueryStyleSearchDumpEvenWithoutReference() {
        ChatResponseSanitizer sanitizer = new ChatResponseSanitizer(new ChatConfig(), ChatGuardService.defaults());

        StructuredReply reply = sanitizer.sanitize(
                new xin.vanilla.banira.domain.BaniraCodeContext(null, List.of(), 1L, 2L, 2L).msg("查一下"),
                "查询：李依然 学历\n1. YouTube Help\n摘要：Learn more\n链接：https://support.google.com/youtube/",
                List.of()
        );

        Assertions.assertFalse(reply.speech().startsWith("查询："));
        Assertions.assertTrue(reply.references().isEmpty());
    }

    @Test
    void shouldStripInternalQqPrefixFromSpeech() {
        ChatResponseSanitizer sanitizer = new ChatResponseSanitizer(new ChatConfig(), ChatGuardService.defaults());

        StructuredReply reply = sanitizer.sanitize(
                new xin.vanilla.banira.domain.BaniraCodeContext(null, List.of(), 1L, 2L, 2L).msg("你在修改谁的群名片"),
                "改的是 qq=900000000001 那个，之前你让我改成洛裘",
                List.of()
        );

        Assertions.assertEquals("改的是 900000000001 那个，之前你让我改成洛裘", reply.speech());
    }

    @Test
    void shouldSuppressWeakSearchReferenceForImplicitExplanationQuestion() {
        ChatResponseSanitizer sanitizer = new ChatResponseSanitizer(new ChatConfig(), ChatGuardService.defaults());

        StructuredReply reply = sanitizer.sanitize(
                new xin.vanilla.banira.domain.BaniraCodeContext(null, List.of(), 1L, 2L, 2L).msg("竹叶清是怎么样的模组"),
                "这个结果不太稳，我先不展开资料",
                List.of("查询：竹叶清 模组\n1. 无关结果\n摘要：看起来不是这个模组\n链接：https://example.com/nope")
        );

        Assertions.assertTrue(reply.references().isEmpty());
    }

    @Test
    void shouldDropRawSearchReferenceEvenWhenUserExplicitlyAsksToSearch() {
        ChatResponseSanitizer sanitizer = new ChatResponseSanitizer(new ChatConfig(), ChatGuardService.defaults());

        StructuredReply reply = sanitizer.sanitize(
                new xin.vanilla.banira.domain.BaniraCodeContext(null, List.of(), 1L, 2L, 2L).msg("帮我查竹叶清是怎么样的模组"),
                "我查到一点资料",
                List.of("查询：竹叶清 模组\n1. 无关结果\n摘要：看起来不是这个模组\n链接：https://example.com/nope")
        );

        Assertions.assertTrue(reply.references().isEmpty());
    }

    @Test
    void shouldDropRawSearchReferenceForImplicitExplanationQuestion() {
        ChatResponseSanitizer sanitizer = new ChatResponseSanitizer(new ChatConfig(), ChatGuardService.defaults());

        StructuredReply reply = sanitizer.sanitize(
                new xin.vanilla.banira.domain.BaniraCodeContext(null, List.of(), 1L, 2L, 2L).msg("根母是什么"),
                "根母大概是一个网络用语",
                List.of("查询：根母是什么\n1. 梗百科：根母\n摘要：网络用语解释\n链接：https://example.com/meaning")
        );

        Assertions.assertTrue(reply.references().isEmpty());
    }

    @Test
    void shouldNotCreateReferenceFromWeakRawSearchDumpForImplicitQuestion() {
        ChatResponseSanitizer sanitizer = new ChatResponseSanitizer(new ChatConfig(), ChatGuardService.defaults());

        StructuredReply reply = sanitizer.sanitize(
                new xin.vanilla.banira.domain.BaniraCodeContext(null, List.of(), 1L, 2L, 2L).msg("竹叶清是什么"),
                "查询：竹叶清\n1. 无关结果\n摘要：看起来不是这个词\n链接：https://example.com/nope",
                List.of()
        );

        Assertions.assertFalse(reply.speech().startsWith("查询："));
        Assertions.assertTrue(reply.references().isEmpty());
    }

    @Test
    void shouldReplaceMechanicalFailureReply() {
        ChatResponseSanitizer sanitizer = new ChatResponseSanitizer(new ChatConfig(), ChatGuardService.defaults());

        StructuredReply reply = sanitizer.sanitize(
                new xin.vanilla.banira.domain.BaniraCodeContext(null, List.of(), 1L, 2L, 2L).msg("帮我查一下"),
                "处理步骤过多，未能完成回复，请简化问题或稍后再试。",
                List.of()
        );

        Assertions.assertFalse(reply.speech().contains("处理步骤过多"));
        Assertions.assertFalse(reply.speech().contains("请简化问题"));
        Assertions.assertTrue(reply.speech().isEmpty());
    }

    @Test
    void shouldSuppressProviderHighRiskRejection() {
        ChatResponseSanitizer sanitizer = new ChatResponseSanitizer(new ChatConfig(), ChatGuardService.defaults());

        StructuredReply reply = sanitizer.sanitize(
                new xin.vanilla.banira.domain.BaniraCodeContext(null, List.of(), 1L, 2L, 2L).msg("白茶酱帮我看看成都今天的天气"),
                "The request was rejected because it was considered high risk",
                List.of()
        );

        Assertions.assertTrue(reply.speech().isEmpty());
        Assertions.assertTrue(reply.references().isEmpty());
    }

    @Test
    void shouldSuppressIdentityDisclosureFallbackInsteadOfSendingGuardText() {
        ChatResponseSanitizer sanitizer = new ChatResponseSanitizer(new ChatConfig(), ChatGuardService.defaults());

        StructuredReply reply = sanitizer.sanitize(
                new xin.vanilla.banira.domain.BaniraCodeContext(null, List.of(), 1L, 2L, 2L).msg("你是不是AI"),
                "我是一个AI语言模型",
                List.of()
        );

        Assertions.assertTrue(reply.speech().isEmpty());
        Assertions.assertTrue(reply.references().isEmpty());
    }

    @Test
    void shouldAllowGeneralAiTopicExplanation() {
        ChatResponseSanitizer sanitizer = new ChatResponseSanitizer(new ChatConfig(), ChatGuardService.defaults());

        StructuredReply reply = sanitizer.sanitize(
                new xin.vanilla.banira.domain.BaniraCodeContext(null, List.of(), 1L, 2L, 2L).msg("白茶酱，AI大模型是什么"),
                "AI 大模型是一类使用大量数据训练出来的模型，擅长理解和生成文本",
                List.of()
        );

        Assertions.assertFalse(reply.speech().isEmpty());
        Assertions.assertTrue(reply.speech().contains("AI 大模型"));
    }

    @Test
    void shouldSuppressInternalToolPolicyReferences() {
        ChatResponseSanitizer sanitizer = new ChatResponseSanitizer(new ChatConfig(), ChatGuardService.defaults());

        StructuredReply reply = sanitizer.sanitize(
                new xin.vanilla.banira.domain.BaniraCodeContext(null, List.of(), 1L, 2L, 2L).msg("白茶酱你认识她吗"),
                "这个我不太确定，可能是某个偶像企划里的角色？",
                List.of("完整内容：\n\n当前最新消息没有明确要求网页搜索，已阻止沿用旧搜索目标。")
        );

        Assertions.assertEquals("这个我不太确定，可能是某个偶像企划里的角色？", reply.speech());
        Assertions.assertTrue(reply.references().isEmpty());
    }

    @Test
    void shouldSuppressMemoryBlocksInForwardReferences() {
        ChatResponseSanitizer sanitizer = new ChatResponseSanitizer(new ChatConfig(), ChatGuardService.defaults());

        StructuredReply reply = sanitizer.sanitize(
                new xin.vanilla.banira.domain.BaniraCodeContext(null, List.of(), 1L, 2L, 2L).msg("总结一下今天的你"),
                "我只能按最近记得的说",
                List.of("""
                        轻量会话记忆（只表示你曾说过或短期上下文，不是长期设定）：
                        - [轻量会话记忆；你曾说过；不是长期设定] 你曾在本群回复过：「蒸馏什么，我又不是液氮罐」 [type:episodic,importance:low,source:auto_reply,bot_said]
                        """)
        );

        Assertions.assertEquals("我只能按最近记得的说", reply.speech());
        Assertions.assertTrue(reply.references().isEmpty());
    }

    @Test
    void shouldSuppressRawMemoryBlocksInSpeech() {
        ChatResponseSanitizer sanitizer = new ChatResponseSanitizer(new ChatConfig(), ChatGuardService.defaults());

        StructuredReply reply = sanitizer.sanitize(
                new xin.vanilla.banira.domain.BaniraCodeContext(null, List.of(), 1L, 2L, 2L).msg("你记得什么"),
                "轻量会话记忆（只表示你曾说过或短期上下文，不是长期设定）：\n- [轻量会话记忆；你曾说过；不是长期设定] 你曾在本群回复过：「测试」 [type:episodic,source:auto_reply,bot_said]",
                List.of()
        );

        Assertions.assertTrue(reply.speech().isEmpty());
        Assertions.assertTrue(reply.references().isEmpty());
    }

    @Test
    void shouldSuppressPromptLeakForwardReferencesFromToolsAndRefBlocks() {
        ChatResponseSanitizer sanitizer = new ChatResponseSanitizer(new ChatConfig(), ChatGuardService.defaults());

        StructuredReply fromTool = sanitizer.sanitize(
                new xin.vanilla.banira.domain.BaniraCodeContext(null, List.of(), 1L, 2L, 2L).msg("你整理下"),
                "整理好了，合并转发里那份就是",
                List.of("基础框架、说话风格、性格禁区和示例台词都列在里面")
        );
        Assertions.assertTrue(fromTool.references().isEmpty());

        StructuredReply fromRefBlock = sanitizer.sanitize(
                new xin.vanilla.banira.domain.BaniraCodeContext(null, List.of(), 1L, 2L, 2L).msg("你整理下"),
                "整理好了\n[REF]你是群里的年轻侍从姐姐\n普通回复不要 Markdown[/REF]",
                List.of()
        );
        Assertions.assertTrue(fromRefBlock.references().isEmpty());
    }

    @Test
    void shouldSuppressInternalContextSpeech() {
        ChatResponseSanitizer sanitizer = new ChatResponseSanitizer(new ChatConfig(), ChatGuardService.defaults());

        StructuredReply reply = sanitizer.sanitize(
                new xin.vanilla.banira.domain.BaniraCodeContext(null, List.of(), 1L, 2L, 2L).msg("我是管理"),
                "引用消息[msgId=572950571, 发送者=稳定身份 qq=2294958275；显示名=下雨了；显示名可能被修改或冒用）]",
                List.of()
        );

        Assertions.assertTrue(reply.speech().isEmpty());
        Assertions.assertTrue(reply.references().isEmpty());
    }

    @Test
    void shouldSuppressInternalToolPolicySpeech() {
        ChatResponseSanitizer sanitizer = new ChatResponseSanitizer(new ChatConfig(), ChatGuardService.defaults());

        StructuredReply reply = sanitizer.sanitize(
                new xin.vanilla.banira.domain.BaniraCodeContext(null, List.of(), 1L, 2L, 2L).msg("白茶酱你认识她吗"),
                "当前最新消息没有明确要求网页搜索，已阻止沿用旧搜索目标。",
                List.of()
        );

        Assertions.assertTrue(reply.speech().isEmpty());
        Assertions.assertTrue(reply.references().isEmpty());
    }
}
