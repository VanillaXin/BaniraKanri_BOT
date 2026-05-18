package xin.vanilla.banira.plugin.kanri;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;
import xin.vanilla.banira.plugin.KanriPlugin;

import java.util.List;

class KanriPluginTest {

    @Test
    void shouldFailWhenActionDuplicated() {
        KanriHandler handler1 = Mockito.mock(KanriHandler.class);
        Mockito.when(handler1.getAction()).thenReturn(List.of("mute"));
        KanriHandler handler2 = Mockito.mock(KanriHandler.class);
        Mockito.when(handler2.getAction()).thenReturn(List.of("mute"));

        KanriPlugin plugin = new KanriPlugin();
        ReflectionTestUtils.setField(plugin, "handlers", List.of(handler1, handler2));

        Assertions.assertThrows(IllegalStateException.class, plugin::initHandlerMap);
    }
}
