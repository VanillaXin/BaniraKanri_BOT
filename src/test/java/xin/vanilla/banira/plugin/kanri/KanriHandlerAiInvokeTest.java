package xin.vanilla.banira.plugin.kanri;

import com.mikuac.shiro.common.utils.MessageConverser;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.plugin.common.BaniraBot;

class KanriHandlerAiInvokeTest {

    private static final long TEST_GROUP_ID = 1L;
    private static final long TEST_BOT_QQ = 900000000001L;
    private static final long TEST_SENDER_QQ = 900000000002L;
    private static final long TEST_TARGET_QQ = 900000000003L;
    private static final String TEST_TARGET = "900000000003";

    @Test
    void shouldNotSendDirectFailMessageWhenAiInvoked() {
        BaniraBot bot = Mockito.mock(BaniraBot.class);
        KanriContext context = new KanriContext(null, bot, TEST_GROUP_ID, TEST_SENDER_QQ, 0, null, "")
                .aiInvoked(true);
        context.noPermissionTargets().add(TEST_TARGET_QQ);

        KanriHandler handler = new MuteCommand();
        handler.executeFail(context);

        Mockito.verify(bot, Mockito.never()).sendGroupMsg(Mockito.anyLong(), Mockito.anyString(), Mockito.anyBoolean());
    }

    @Test
    void shouldUseOnlyToolArgsAsTargetsWhenAiInvoked() {
        BaniraBot bot = Mockito.mock(BaniraBot.class);
        com.mikuac.shiro.dto.event.message.GroupMessageEvent event =
                Mockito.mock(com.mikuac.shiro.dto.event.message.GroupMessageEvent.class);
        Mockito.when(event.getArrayMsg())
                .thenReturn(MessageConverser.stringToArray("[CQ:at,qq=" + TEST_BOT_QQ + "] 改群名片"));
        KanriContext context = new KanriContext(event, bot, TEST_GROUP_ID, TEST_SENDER_QQ, 0, null, "")
                .aiInvoked(true);

        KanriHandler handler = new CardCommand();

        Assertions.assertEquals(
                java.util.Set.of(TEST_TARGET_QQ),
                handler.getUserIdsWithReply(context, new String[]{TEST_TARGET, "新名片"})
        );
    }
}
