package xin.vanilla.banira.plugin.chat.capability;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class AiDirectResultTest {

    @Test
    void shouldMarkAndUnwrapDirectResult() {
        String result = AiDirectResult.sent("已发送。");

        Assertions.assertTrue(AiDirectResult.isDirect(result));
        Assertions.assertEquals("已发送。", AiDirectResult.message(result));
    }
}
