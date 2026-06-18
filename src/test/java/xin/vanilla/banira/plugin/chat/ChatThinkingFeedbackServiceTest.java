package xin.vanilla.banira.plugin.chat;

import com.mikuac.shiro.enums.MsgTypeEnum;
import com.mikuac.shiro.model.ArrayMsg;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import xin.vanilla.banira.config.entity.extended.ChatReplySettings;
import xin.vanilla.banira.domain.BaniraCodeContext;

import java.lang.reflect.Method;
import java.util.List;

class ChatThinkingFeedbackServiceTest {

    @Test
    void shouldNotFeedbackForQuickContextQuestion() throws Exception {
        BaniraCodeContext ctx = new BaniraCodeContext(null, List.of(), 1L, 2L, 2L)
                .msg("@香草青茶 你为什么不理她");

        Assertions.assertFalse(looksWorthFeedback(ctx));
    }

    @Test
    void shouldFeedbackForLongRunningSearchOrMedia() throws Exception {
        BaniraCodeContext search = new BaniraCodeContext(null, List.of(), 1L, 2L, 2L)
                .msg("@香草青茶 帮我搜索一下这个项目最近怎么样");
        BaniraCodeContext image = new BaniraCodeContext(null, List.of(new ArrayMsg().setType(MsgTypeEnum.image)), 1L, 2L, 2L)
                .msg("@香草青茶 看看这张图");

        Assertions.assertTrue(looksWorthFeedback(search));
        Assertions.assertTrue(looksWorthFeedback(image));
    }

    @Test
    void shouldDropMechanicalFeedbackText() throws Exception {
        Assertions.assertEquals("", sanitizeFeedback("收到，在处理", new ChatReplySettings()));
        Assertions.assertEquals("", sanitizeFeedback("正在处理", new ChatReplySettings()));
        Assertions.assertEquals("", sanitizeFeedback("先处理下", new ChatReplySettings()));
        Assertions.assertEquals("", sanitizeFeedback("请稍等一下", new ChatReplySettings()));
    }

    private static boolean looksWorthFeedback(BaniraCodeContext ctx) throws Exception {
        Method method = ChatThinkingFeedbackService.class.getDeclaredMethod("looksWorthFeedback", BaniraCodeContext.class);
        method.setAccessible(true);
        return (boolean) method.invoke(null, ctx);
    }

    private static String sanitizeFeedback(String text, ChatReplySettings settings) throws Exception {
        Method method = ChatThinkingFeedbackService.class.getDeclaredMethod("sanitizeFeedback", String.class, ChatReplySettings.class);
        method.setAccessible(true);
        return (String) method.invoke(null, text, settings);
    }
}
