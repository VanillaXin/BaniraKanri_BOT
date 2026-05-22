package xin.vanilla.banira.plugin.kanri;

import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.plugin.common.BaniraBot;

class KanriSelfOperationPolicyTest {

    @Test
    void shouldDenySelfMuteWhenBotIsNotGroupAdmin() {
        BaniraBot bot = Mockito.mock(BaniraBot.class);
        Mockito.when(bot.isGroupOwnerOrAdmin(1L)).thenReturn(false);

        Assertions.assertFalse(KanriSelfOperationPolicy.canAllowSelfMuteOrLoud(bot, 1L, 123456L));
    }

    @Test
    void allowsMuteWhenArgsTargetSenderOnly() {
        BaniraBot bot = Mockito.mock(BaniraBot.class);
        Mockito.when(bot.isGroupOwnerOrAdmin(1L)).thenReturn(true);
        Mockito.when(bot.isGroupOwner(1L, 123456L)).thenReturn(false);
        Mockito.when(bot.isGroupAdmin(1L, 123456L)).thenReturn(false);

        GroupMessageEvent event = Mockito.mock(GroupMessageEvent.class);
        KanriContext context = new KanriContext(
                event,
                bot,
                1L,
                123456L,
                0,
                "",
                "self mute request"
        );
        MuteCommand handler = new MuteCommand();
        String[] args = {"123456", "10"};

        Assertions.assertTrue(KanriSelfOperationPolicy.allowsMute(context, args, handler));
    }

    @Test
    void muteCommandAllowsOperatorToMuteSelfWhenBotCanOperateTarget() {
        BaniraBot bot = Mockito.mock(BaniraBot.class);
        Mockito.when(bot.isGroupOwnerOrAdmin(1L)).thenReturn(true);
        Mockito.when(bot.isGroupOwner(1L, 123456L)).thenReturn(false);
        Mockito.when(bot.isGroupAdmin(1L, 123456L)).thenReturn(true);
        Mockito.when(bot.isUpperInGroup(1L, 123456L)).thenReturn(true);

        GroupMessageEvent event = Mockito.mock(GroupMessageEvent.class);
        Mockito.when(event.getArrayMsg()).thenReturn(java.util.List.of());
        KanriContext context = new KanriContext(
                event,
                bot,
                1L,
                123456L,
                0,
                "",
                "123456 10"
        ).aiInvoked(true);
        MuteCommand handler = new MuteCommand();

        int result = handler.execute(context, new String[]{"123456", "10"});

        Assertions.assertEquals(KanriHandler.SUCCESS, result);
        Mockito.verify(bot).setGroupBan(1L, 123456L, 600);
    }
}
