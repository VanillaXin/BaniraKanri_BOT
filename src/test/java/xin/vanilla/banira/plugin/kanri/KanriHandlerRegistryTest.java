package xin.vanilla.banira.plugin.kanri;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.List;

class KanriHandlerRegistryTest {

    @Test
    void shouldKeepFirstHandlerWhenActionDuplicated() {
        KanriHandler handler1 = Mockito.mock(KanriHandler.class);
        Mockito.when(handler1.getAction()).thenReturn(List.of("mute"));
        KanriHandler handler2 = Mockito.mock(KanriHandler.class);
        Mockito.when(handler2.getAction()).thenReturn(List.of("mute"));

        KanriHandlerRegistry registry = new KanriHandlerRegistry();
        ReflectionTestUtils.setField(registry, "handlers", List.of(handler1, handler2));
        registry.init();

        Assertions.assertSame(handler1, registry.resolve("mute"));
    }

    @Test
    void shouldUseWhitelistForAiHandlers() {
        KanriHandlerRegistry registry = new KanriHandlerRegistry();
        Assertions.assertFalse(registry.isAiAllowed(new AdminCommand()));
        Assertions.assertFalse(registry.isAiAllowed(new KickCommand()));
        Assertions.assertTrue(registry.isAiAllowed(new MuteCommand()));
        Assertions.assertTrue(registry.isAiAllowed(new GroupNameCommand()));
    }

}
