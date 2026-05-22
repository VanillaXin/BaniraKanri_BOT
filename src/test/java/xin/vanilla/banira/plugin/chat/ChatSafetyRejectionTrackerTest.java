package xin.vanilla.banira.plugin.chat;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class ChatSafetyRejectionTrackerTest {

    @AfterEach
    void tearDown() {
        ChatSafetyRejectionTracker.clearForTest();
    }

    @Test
    void shouldRecognizeProviderSafetyRejectionMessages() {
        Assertions.assertTrue(ChatSafetyRejectionTracker.looksLikeProviderSafetyText(
                "The request was rejected because it was considered high risk"
        ));
        Assertions.assertTrue(ChatSafetyRejectionTracker.looksLikeProviderSafetyText(
                "API 返回的 completion 由于内容安全过滤被拒绝"
        ));
        Assertions.assertFalse(ChatSafetyRejectionTracker.looksLikeProviderSafetyText("普通网络错误"));
    }
}
