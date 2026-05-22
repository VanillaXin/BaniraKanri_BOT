package xin.vanilla.banira.plugin.chat;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import xin.vanilla.banira.config.entity.extended.ChatEngagementSettings;
import xin.vanilla.banira.service.IAiGroupEngagementManager;

class ChatEngagementServiceTest {

    @Test
    void storesAndClearsInterestInMemory() {
        IAiGroupEngagementManager manager = Mockito.mock(IAiGroupEngagementManager.class);
        ChatEngagementService service = new ChatEngagementService(manager);
        ChatEngagementSettings settings = new ChatEngagementSettings()
                .enabled(true)
                .persistenceEnabled(false)
                .followInterestThreshold(50);
        service.update(100L, 123L, 80, settings);
        Assertions.assertTrue(service.hasActiveFollow(100L, 123L, settings));
        Assertions.assertEquals(80, service.currentInterest(100L, 123L, settings));

        service.update(100L, 123L, 10, settings);
        Assertions.assertFalse(service.hasActiveFollow(100L, 123L, settings));
    }

    @Test
    void passiveDecayReducesInterest() {
        IAiGroupEngagementManager manager = Mockito.mock(IAiGroupEngagementManager.class);
        ChatEngagementService service = new ChatEngagementService(manager);
        ChatEngagementSettings settings = new ChatEngagementSettings()
                .enabled(true)
                .persistenceEnabled(false)
                .passiveDecayEnabled(true)
                .passiveDecayDelta(3);
        service.update(100L, 123L, 52, settings);
        service.applyPassiveDecay(100L, 123L, false, settings);
        Assertions.assertEquals(47, service.currentInterest(100L, 123L, settings));
    }

    @Test
    void passiveDecaySkipsStrongTrigger() {
        IAiGroupEngagementManager manager = Mockito.mock(IAiGroupEngagementManager.class);
        ChatEngagementService service = new ChatEngagementService(manager);
        ChatEngagementSettings settings = new ChatEngagementSettings()
                .enabled(true)
                .persistenceEnabled(false)
                .passiveDecayDelta(3);
        service.update(100L, 123L, 52, settings);
        service.applyPassiveDecay(100L, 123L, true, settings);
        Assertions.assertEquals(52, service.currentInterest(100L, 123L, settings));
    }
}
