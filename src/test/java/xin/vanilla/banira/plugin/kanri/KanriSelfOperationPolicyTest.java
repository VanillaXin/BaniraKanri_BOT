package xin.vanilla.banira.plugin.kanri;

import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.plugin.common.BaniraBot;

class KanriSelfOperationPolicyTest {

    private static final long TEST_GROUP_ID = 1L;
    private static final long TEST_BOT_QQ = 900000000001L;
    private static final long TEST_SENDER_QQ = 900000000002L;
    private static final long TEST_OTHER_QQ = 900000000003L;
    private static final String TEST_SENDER = "900000000002";
    private static final String TEST_OTHER = "900000000003";

    @Test
    void shouldDenySelfMuteWhenBotIsNotGroupAdmin() {
        BaniraBot bot = Mockito.mock(BaniraBot.class);
        Mockito.when(bot.isGroupOwnerOrAdmin(TEST_GROUP_ID)).thenReturn(false);

        Assertions.assertFalse(KanriSelfOperationPolicy.canAllowSelfMuteOrLoud(bot, TEST_GROUP_ID, TEST_SENDER_QQ));
    }

    @Test
    void allowsMuteWhenArgsTargetSenderOnly() {
        BaniraBot bot = Mockito.mock(BaniraBot.class);
        Mockito.when(bot.isGroupOwnerOrAdmin(TEST_GROUP_ID)).thenReturn(true);
        Mockito.when(bot.isGroupOwner(TEST_GROUP_ID, TEST_SENDER_QQ)).thenReturn(false);
        Mockito.when(bot.isGroupAdmin(TEST_GROUP_ID, TEST_SENDER_QQ)).thenReturn(false);

        GroupMessageEvent event = Mockito.mock(GroupMessageEvent.class);
        KanriContext context = new KanriContext(
                event,
                bot,
                TEST_GROUP_ID,
                TEST_SENDER_QQ,
                0,
                "",
                "self mute request"
        );
        MuteCommand handler = new MuteCommand();
        String[] args = {TEST_SENDER, "10"};

        Assertions.assertTrue(KanriSelfOperationPolicy.allowsMute(context, args, handler));
    }

    @Test
    void allowsCardWhenArgsTargetSenderOnly() {
        BaniraBot bot = Mockito.mock(BaniraBot.class);
        Mockito.when(bot.getSelfId()).thenReturn(TEST_BOT_QQ);
        Mockito.when(bot.isGroupOwnerOrAdmin(TEST_GROUP_ID)).thenReturn(true);

        GroupMessageEvent event = Mockito.mock(GroupMessageEvent.class);
        KanriContext context = new KanriContext(
                event,
                bot,
                TEST_GROUP_ID,
                TEST_SENDER_QQ,
                0,
                "",
                TEST_SENDER + " 新名片"
        );
        CardCommand handler = new CardCommand();
        String[] args = {TEST_SENDER, "新名片"};

        Assertions.assertTrue(KanriSelfOperationPolicy.allowsCard(context, args, handler));
    }

    @Test
    void deniesCardWhenArgsTargetOtherMember() {
        BaniraBot bot = Mockito.mock(BaniraBot.class);
        Mockito.when(bot.getSelfId()).thenReturn(TEST_BOT_QQ);
        Mockito.when(bot.isGroupOwnerOrAdmin(TEST_GROUP_ID)).thenReturn(true);

        GroupMessageEvent event = Mockito.mock(GroupMessageEvent.class);
        KanriContext context = new KanriContext(
                event,
                bot,
                TEST_GROUP_ID,
                TEST_SENDER_QQ,
                0,
                "",
                TEST_OTHER + " 新名片"
        );
        CardCommand handler = new CardCommand();
        String[] args = {TEST_OTHER, "新名片"};

        Assertions.assertFalse(KanriSelfOperationPolicy.allowsCard(context, args, handler));
    }

    @Test
    void muteCommandAllowsOperatorToMuteSelfWhenBotCanOperateTarget() {
        BaniraBot bot = Mockito.mock(BaniraBot.class);
        Mockito.when(bot.isGroupOwnerOrAdmin(TEST_GROUP_ID)).thenReturn(true);
        Mockito.when(bot.isGroupOwner(TEST_GROUP_ID, TEST_SENDER_QQ)).thenReturn(false);
        Mockito.when(bot.isGroupAdmin(TEST_GROUP_ID, TEST_SENDER_QQ)).thenReturn(true);
        Mockito.when(bot.isUpperInGroup(TEST_GROUP_ID, TEST_SENDER_QQ)).thenReturn(true);

        GroupMessageEvent event = Mockito.mock(GroupMessageEvent.class);
        Mockito.when(event.getArrayMsg()).thenReturn(java.util.List.of());
        KanriContext context = new KanriContext(
                event,
                bot,
                TEST_GROUP_ID,
                TEST_SENDER_QQ,
                0,
                "",
                TEST_SENDER + " 10"
        ).aiInvoked(true);
        MuteCommand handler = new MuteCommand();

        int result = handler.execute(context, new String[]{TEST_SENDER, "10"});

        Assertions.assertEquals(KanriHandler.SUCCESS, result);
        Mockito.verify(bot).setGroupBan(TEST_GROUP_ID, TEST_SENDER_QQ, 600);
    }
}
