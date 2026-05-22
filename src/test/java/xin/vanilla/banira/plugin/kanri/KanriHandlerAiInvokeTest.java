package xin.vanilla.banira.plugin.kanri;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.plugin.common.BaniraBot;

class KanriHandlerAiInvokeTest {

    @Test
    void shouldNotSendDirectFailMessageWhenAiInvoked() {
        BaniraBot bot = Mockito.mock(BaniraBot.class);
        KanriContext context = new KanriContext(null, bot, 1L, 2L, 0, null, "")
                .aiInvoked(true);
        context.noPermissionTargets().add(999L);

        KanriHandler handler = new MuteCommand();
        handler.executeFail(context);

        Mockito.verify(bot, Mockito.never()).sendGroupMsg(Mockito.anyLong(), Mockito.anyString(), Mockito.anyBoolean());
    }
}
