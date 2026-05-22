package xin.vanilla.banira.plugin.chat.capability;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import xin.vanilla.banira.plugin.chat.agent.AgentContext;

import java.util.Map;

class AiCapabilityTest {

    @Test
    void shouldRequireConfirmationBeforeSensitiveExecution() {
        AiCapability capability = new AiCapability()
                .name("danger")
                .requireConfirmation(true)
                .executor((ctx, args) -> "executed");

        String first = capability.execute(new AgentContext(), Map.of());
        Assertions.assertTrue(first.contains("确认"));

        String confirmed = capability.execute(new AgentContext(), Map.of("confirm", "true"));
        Assertions.assertEquals("executed", confirmed);
    }

}
