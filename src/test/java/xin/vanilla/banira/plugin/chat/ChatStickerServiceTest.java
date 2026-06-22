package xin.vanilla.banira.plugin.chat;

import com.mikuac.shiro.common.utils.MessageConverser;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.enums.EnumMessageType;
import xin.vanilla.banira.plugin.chat.agent.AgentContext;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.service.IMessageRecordManager;

import java.util.List;

class ChatStickerServiceTest {

    @Test
    void shouldResolveArchiveFromQuotedMessageRecord() {
        IMessageRecordManager manager = Mockito.mock(IMessageRecordManager.class);
        Mockito.when(manager.getGroupMessageRecord(20000L, 42))
                .thenReturn(new MessageRecord()
                        .setMsgId("42")
                        .setGroupId(20000L)
                        .setSenderId(40000L)
                        .setMsgRecode("[CQ:unknown,file=stickers.zip,url=https://example.test/stickers.zip]"));
        AgentContext ctx = context("[CQ:reply,id=42][CQ:at,qq=10000] 你把这些表情包导入一下");

        String resolved = ChatStickerService.resolveArchiveSource(ctx, "", manager);

        Assertions.assertEquals("https://example.test/stickers.zip", resolved);
    }

    @Test
    void shouldResolveArchiveFromRecentGroupFileWhenNoLinkInCurrentMessage() {
        IMessageRecordManager manager = Mockito.mock(IMessageRecordManager.class);
        Mockito.when(manager.getMessageRecordList(Mockito.any()))
                .thenReturn(List.of(
                        new MessageRecord()
                                .setMsgId("41")
                                .setGroupId(20000L)
                                .setSenderId(40000L)
                                .setMsgRecode("[CQ:unknown,file=stickers.zip,url=https://example.test/recent-pack.zip]"),
                        new MessageRecord()
                                .setMsgId("40")
                                .setGroupId(20000L)
                                .setSenderId(40000L)
                                .setMsgRecode("普通聊天")
                ));
        AgentContext ctx = context("[CQ:at,qq=10000] 你把这些表情包导入一下");

        String resolved = ChatStickerService.resolveArchiveSource(ctx, "", manager);

        Assertions.assertEquals("https://example.test/recent-pack.zip", resolved);
    }

    private static AgentContext context(String currentMessage) {
        BaniraBot bot = Mockito.mock(BaniraBot.class);
        Mockito.when(bot.getSelfId()).thenReturn(10000L);
        BaniraCodeContext messageContext = new BaniraCodeContext(
                bot,
                MessageConverser.stringToArray(currentMessage),
                20000L,
                30000L,
                30000L
        ).msg(currentMessage).msgId(43).msgType(EnumMessageType.GROUP);
        return new AgentContext()
                .bot(bot)
                .messageContext(messageContext)
                .groupId(20000L)
                .senderId(30000L)
                .msgType(EnumMessageType.GROUP)
                .msgId("43")
                .userMessage(currentMessage);
    }
}
