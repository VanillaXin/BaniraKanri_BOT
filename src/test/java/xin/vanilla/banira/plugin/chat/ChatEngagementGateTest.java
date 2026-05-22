package xin.vanilla.banira.plugin.chat;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import xin.vanilla.banira.config.entity.extended.ChatEngagementSettings;
import xin.vanilla.banira.config.entity.extended.ChatReplySettings;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.enums.EnumMessageType;

class ChatEngagementGateTest {

    @Test
    void shouldInvokePreflightForSemanticGroupMessage() {
        ChatEngagementSettings engagement = new ChatEngagementSettings()
                .ownerAlwaysInvoke(false)
                .randomBubbleEnabled(false);
        ChatEngagementService service = Mockito.mock(ChatEngagementService.class);
        ChatEngagementGate gate = new ChatEngagementGate(
                engagement,
                new ChatReplySettings(),
                service,
                Mockito.mock(ReplyDecisionMaker.class)
        );

        boolean invoke = gate.shouldInvokeModel(10000L, groupMessage("夏生先生是谁"), false, false, false, 50);

        Assertions.assertTrue(invoke);
    }

    @Test
    void shouldSkipPureMediaAndShortNoiseInSemiFullPreflight() {
        ChatEngagementSettings engagement = new ChatEngagementSettings()
                .ownerAlwaysInvoke(false)
                .randomBubbleEnabled(false);
        ChatEngagementService service = Mockito.mock(ChatEngagementService.class);
        ChatEngagementGate gate = new ChatEngagementGate(
                engagement,
                new ChatReplySettings(),
                service,
                Mockito.mock(ReplyDecisionMaker.class)
        );

        Assertions.assertFalse(gate.shouldInvokeModel(10000L, groupMessage("[CQ:image,file=a.jpg]"), false, false, false, 50));
        Assertions.assertFalse(gate.shouldInvokeModel(10000L, groupMessage("。"), false, false, false, 50));
    }

    private static BaniraCodeContext groupMessage(String text) {
        return new BaniraCodeContext(null, java.util.List.of(), 1L, 2L, 2L)
                .msg(text)
                .msgType(EnumMessageType.GROUP);
    }
}
