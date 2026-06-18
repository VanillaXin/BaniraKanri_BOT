package xin.vanilla.banira.plugin.chat.agent;

import dev.langchain4j.data.message.AiMessage;
import dev.langchain4j.model.chat.request.ChatRequest;
import dev.langchain4j.model.chat.response.ChatResponse;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.plugin.chat.ChatSafetyRejectionTracker;
import xin.vanilla.banira.plugin.chat.model.ChatModelRouter;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

class ChatAgentRunnerTest {

    @Test
    void shouldRegenerateWhenAgentReceivesProviderRejectionText() {
        AtomicInteger calls = new AtomicInteger();
        ChatModelRouter router = new ChatModelRouter(new ChatConfig()) {
            @Override
            public ChatResponse chat(ChatRequest request) {
                int call = calls.incrementAndGet();
                String text = call == 1
                        ? "The request was rejected"
                        : "这个话题不展开了，换个方向";
                return ChatResponse.builder()
                        .aiMessage(AiMessage.from(text))
                        .build();
            }
        };

        AgentRunResult result = ChatAgentRunner.run(
                router,
                new ArrayList<>(),
                new Object(),
                3,
                new ArrayList<>()
        );

        Assertions.assertEquals("这个话题不展开了，换个方向", result.speechText());
        Assertions.assertEquals(2, calls.get());
        Assertions.assertFalse(ChatSafetyRejectionTracker.looksLikeProviderSafetyText(result.speechText()));
    }

    @Test
    void shouldSuppressProviderRejectionAfterRecoveryExhausted() {
        ChatModelRouter router = new ChatModelRouter(new ChatConfig()) {
            @Override
            public ChatResponse chat(ChatRequest request) {
                return ChatResponse.builder()
                        .aiMessage(AiMessage.from("The request was rejected"))
                        .build();
            }
        };

        AgentRunResult result = ChatAgentRunner.run(
                router,
                List.of(),
                new Object(),
                3,
                new ArrayList<>()
        );

        Assertions.assertTrue(result.speechText().isEmpty());
    }
}
