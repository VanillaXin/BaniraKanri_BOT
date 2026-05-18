package xin.vanilla.banira.plugin.chat;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.service.IMessageRecordManager;

class AIChatServiceTest {

    @Test
    void shouldRejectNullDependencies() {
        IMessageRecordManager messageRecordManager = Mockito.mock(IMessageRecordManager.class);
        Assertions.assertThrows(NullPointerException.class, () -> new AIChatService(null, messageRecordManager));
        Assertions.assertThrows(NullPointerException.class, () -> new AIChatService(new ChatConfig(), null));
    }
}
