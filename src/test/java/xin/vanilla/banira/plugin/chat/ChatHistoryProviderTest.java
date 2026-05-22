package xin.vanilla.banira.plugin.chat;

import com.mikuac.shiro.common.utils.MessageConverser;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.enums.EnumMessageType;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.service.IMessageRecordManager;

import java.util.List;

class ChatHistoryProviderTest {

    @Test
    void shouldIncludeQuotedMessageNeighborhood() {
        IMessageRecordManager manager = Mockito.mock(IMessageRecordManager.class);
        BaniraBot bot = Mockito.mock(BaniraBot.class);
        Mockito.when(bot.getSelfId()).thenReturn(10000L);
        ChatHistoryProvider provider = new ChatHistoryProvider(new ChatConfig(), manager);

        MessageRecord anchor = record(20, 200, 10000L, "确实不知道，我搜了也没搜到");
        Mockito.when(manager.getMessageRecordList(Mockito.any()))
                .thenReturn(List.of())
                .thenReturn(List.of(record(19, 199, 30000L, "韬定律")))
                .thenReturn(List.of(record(21, 201, 40000L, "你再搜搜")));
        Mockito.when(manager.getGroupMessageRecord(20000L, 200)).thenReturn(anchor);
        BaniraCodeContext ctx = new BaniraCodeContext(
                bot,
                MessageConverser.stringToArray("[CQ:reply,id=200][CQ:at,qq=10000] 你再搜搜看"),
                20000L,
                50000L,
                50000L
        ).msg("[CQ:reply,id=200][CQ:at,qq=10000] 你再搜搜看")
                .msgId(300)
                .msgType(EnumMessageType.GROUP);

        List<MessageRecord> result = provider.history(bot, ctx);

        Assertions.assertTrue(result.stream().anyMatch(record -> "韬定律".equals(record.getMsgRecode())));
        Assertions.assertTrue(result.stream().anyMatch(record -> "确实不知道，我搜了也没搜到".equals(record.getMsgRecode())));
    }

    @Test
    void shouldSkipRecalledMessagesAndQuotedAnchor() {
        IMessageRecordManager manager = Mockito.mock(IMessageRecordManager.class);
        BaniraBot bot = Mockito.mock(BaniraBot.class);
        Mockito.when(bot.getSelfId()).thenReturn(10000L);
        ChatHistoryProvider provider = new ChatHistoryProvider(new ChatConfig(), manager);

        MessageRecord recalled = record(20, 200, 30000L, "recalled").setRecalled(true);
        MessageRecord active = record(21, 201, 40000L, "active");
        Mockito.when(manager.getMessageRecordList(Mockito.any()))
                .thenReturn(List.of(recalled, active));
        Mockito.when(manager.getGroupMessageRecord(20000L, 200)).thenReturn(recalled);

        BaniraCodeContext ctx = new BaniraCodeContext(
                bot,
                MessageConverser.stringToArray("[CQ:reply,id=200][CQ:at,qq=10000] check"),
                20000L,
                50000L,
                50000L
        ).msg("[CQ:reply,id=200][CQ:at,qq=10000] check")
                .msgId(300)
                .msgType(EnumMessageType.GROUP);

        List<MessageRecord> result = provider.history(bot, ctx);

        Assertions.assertFalse(result.stream().anyMatch(MessageRecord::recalled));
        Assertions.assertTrue(result.stream().anyMatch(record -> "active".equals(record.getMsgRecode())));
        Assertions.assertFalse(result.stream().anyMatch(record -> "recalled".equals(record.getMsgRecode())));
    }

    private static MessageRecord record(long id, int msgId, long sender, String text) {
        return new MessageRecord()
                .setId(id)
                .setMsgId(msgId)
                .setBotId(10000L)
                .setSenderId(sender)
                .setGroupId(20000L)
                .setTime(id)
                .setMsgType(EnumMessageType.GROUP)
                .setMsgRecode(text);
    }
}
