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
    void shouldInferQuestionStickerMetadataFromArchivePath() {
        ChatStickerService.StickerMetadata metadata = ChatStickerService.fallbackStickerMetadata("疑问/问号猫.png");

        Assertions.assertTrue(metadata.description().contains("疑问") || metadata.description().contains("问号"));
        Assertions.assertTrue(metadata.scene().contains("疑问") || metadata.scene().contains("困惑"));
        Assertions.assertTrue(metadata.keywords().contains("疑问"));
        Assertions.assertTrue(metadata.keywords().contains("困惑"));
    }

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

    @Test
    void shouldPreferGroupFileIdWhenArchiveMessageHasAuthenticatedUrl() {
        AgentContext ctx = context("[CQ:unknown,file=stickers.zip,name=stickers.zip,file_id=file-123,busid=102,url=https://auth.example.test/tmp.zip]");

        String resolved = ChatStickerService.resolveArchiveSource(ctx, "", null);

        Assertions.assertEquals("stickers.zip", resolved);
    }

    @Test
    void shouldResolveMultipleImagesFromCurrentMessage() {
        AgentContext ctx = context("[CQ:at,qq=10000] 收一下 [CQ:image,file=a.jpg,url=https://example.test/a.jpg][CQ:image,file=b.jpg,url=https://example.test/b.jpg]");

        List<ChatStickerService.StickerImageCandidate> candidates = ChatStickerService.resolveStickerImageSources(ctx, "", null);

        Assertions.assertEquals(2, candidates.size());
        Assertions.assertEquals("https://example.test/a.jpg", candidates.get(0).sourceUrl());
        Assertions.assertEquals("https://example.test/b.jpg", candidates.get(1).sourceUrl());
    }

    @Test
    void shouldResolveSingleStickerFromRecentImageWhenUserSaysCollectThis() {
        IMessageRecordManager manager = Mockito.mock(IMessageRecordManager.class);
        Mockito.when(manager.getMessageRecordList(Mockito.any()))
                .thenReturn(List.of(
                        new MessageRecord()
                                .setMsgId("42")
                                .setGroupId(20000L)
                                .setSenderId(10000L)
                                .setMsgRecode("[CQ:image,file=cat.jpg,url=https://example.test/cat-question.jpg]"),
                        new MessageRecord()
                                .setMsgId("41")
                                .setGroupId(20000L)
                                .setSenderId(40000L)
                                .setMsgRecode("普通聊天")
                ));
        AgentContext ctx = context("[CQ:at,qq=10000] 可以把这个纳入收藏");

        ChatStickerService.StickerImageCandidate candidate = ChatStickerService.resolveStickerImageSource(ctx, "", manager);

        Assertions.assertNotNull(candidate);
        Assertions.assertEquals("https://example.test/cat-question.jpg", candidate.sourceUrl());
        Assertions.assertEquals(42L, candidate.sourceMsgId());
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
