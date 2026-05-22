package xin.vanilla.banira.plugin.chat;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import xin.vanilla.banira.plugin.chat.capability.AiCapability;
import xin.vanilla.banira.plugin.chat.capability.CapabilityInvocationPolicy;
import xin.vanilla.banira.plugin.chat.capability.PendingAiActionStore;
import xin.vanilla.banira.plugin.chat.agent.AgentContext;

import java.util.List;
import java.util.Map;

class KanriProposalParserTest {

    @Test
    void extractsTargetKeywordsFromMuteProposal() {
        String speech = "现在帮你处理，闹的那几个：吉祥物、我是mjj、假·吉祥物，禁言10分钟，行吗？";

        List<String> keywords = KanriProposalParser.extractMuteTargetKeywords(speech);

        Assertions.assertEquals(List.of("吉祥物", "我是mjj", "假·吉祥物"), keywords);
        Assertions.assertEquals(10, KanriProposalParser.extractMuteMinutes(speech, 10));
    }

    @Test
    void recognizesProceedIntentPhrases() {
        Assertions.assertTrue(PendingAiActionStore.isKanriProceedIntent("yes!"));
        Assertions.assertTrue(PendingAiActionStore.isKanriRetryIntent("@白茶酱 你再试试"));
        Assertions.assertTrue(PendingAiActionStore.isKanriProceedIntent("@白茶酱 请动手吧"));
        Assertions.assertTrue(PendingAiActionStore.isKanriProceedIntent("为什么不帮我禁言他们"));
    }

    @Test
    void allowsMuteWhenUserSaysProceed() {
        CapabilityInvocationPolicy.Decision allowed = CapabilityInvocationPolicy.evaluate(
                new AgentContext().userMessage("@白茶酱 请动手吧"),
                new AiCapability().name("execute_kanri").mutating(true),
                "execute_kanri",
                Map.of("action", "mute", "args", "123456789 10", "confirm", "true")
        );

        Assertions.assertTrue(allowed.allowed());
    }
}
