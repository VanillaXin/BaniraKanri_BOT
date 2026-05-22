package xin.vanilla.banira.util;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.service.IMessageRecordManager;

class BaniraUtilsTest {

    @Test
    void shouldResolveReplySenderFromGroupMessageRecord() {
        BaniraBot bot = Mockito.mock(BaniraBot.class);
        Mockito.when(bot.getSelfId()).thenReturn(10000L);
        IMessageRecordManager manager = Mockito.mock(IMessageRecordManager.class);
        Mockito.when(manager.getGroupMessageRecord(123456L, 7788))
                .thenReturn(new MessageRecord().setSenderId(998877L));

        long sender = BaniraUtils.resolveReplyUserId(bot, 123456L, 7788L, 0L, manager);

        Assertions.assertEquals(998877L, sender);
        Mockito.verify(manager).getGroupMessageRecord(123456L, 7788);
        Mockito.verify(manager, Mockito.never()).getPrivateMessageRecord(Mockito.anyLong(), Mockito.anyInt());
    }

    @Test
    void shouldPreferReplySegmentQqWithoutQueryingRecords() {
        BaniraBot bot = Mockito.mock(BaniraBot.class);
        IMessageRecordManager manager = Mockito.mock(IMessageRecordManager.class);

        long sender = BaniraUtils.resolveReplyUserId(bot, 123456L, 7788L, 112233L, manager);

        Assertions.assertEquals(112233L, sender);
        Mockito.verifyNoInteractions(manager);
    }
}
