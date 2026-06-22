package xin.vanilla.banira.plugin.chat;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import xin.vanilla.banira.domain.MessageRecord;

import java.util.List;

class ChatConversationSignalsTest {

    private static final long BOT_ID = 10001L;

    @Test
    void detectsAwaitingReplyInBotSpeech() {
        Assertions.assertTrue(ChatConversationSignals.awaitsUserReply("禁言10分钟，行吗？"));
        Assertions.assertFalse(ChatConversationSignals.awaitsUserReply("好的，已经禁言了"));
    }

    @Test
    void detectsShortReplyToBotQuestion() {
        MessageRecord botAsk = new MessageRecord()
                .setSenderId(BOT_ID)
                .setMsgRecode("吉祥物、我是mjj、假·吉祥物，禁言10分钟，行吗？");

        Assertions.assertTrue(ChatConversationSignals.isShortReplyToBotQuestion(
                List.of(botAsk), BOT_ID, "按你刚才说的来"));
    }
}
