package xin.vanilla.banira.plugin.chat;

import com.mikuac.shiro.common.utils.MessageConverser;
import com.mikuac.shiro.dto.action.response.MsgResp;
import com.mikuac.shiro.enums.MsgTypeEnum;
import com.mikuac.shiro.model.ArrayMsg;
import dev.langchain4j.data.message.Content;
import dev.langchain4j.data.message.ImageContent;
import dev.langchain4j.data.message.TextContent;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.List;
import java.util.Map;

class MessageConvertTest {

    private static final String ONE_PIXEL_PNG_DATA_URL = "data:image/png;base64,"
            + "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+/p9sAAAAASUVORK5CYII=";

    @Test
    void shouldExpandForwardImagesAsQuotedContent() {
        BaniraBot bot = Mockito.mock(BaniraBot.class);
        Mockito.when(bot.getSelfId()).thenReturn(999L);
        String image = "[CQ:image,file=test.jpg,url=https://example.com/test.jpg]";
        MsgResp node = new MsgResp();
        node.setMessage(image);
        node.setRawMessage(image);
        node.setArrayMsg(MessageConverser.stringToArray(image));
        ArrayMsg forward = BaniraUtils.packForwardMsg(null, List.of(node));

        List<Content> contents = MessageConvert.toContents(bot, 10001L, forward, false, new ChatMessageContextFormatter.UserInfoCache());

        String text = contents.stream()
                .filter(TextContent.class::isInstance)
                .map(TextContent.class::cast)
                .map(TextContent::text)
                .reduce("", String::concat);
        Assertions.assertTrue(text.contains("合并转发内容"));
        Assertions.assertTrue(text.contains("[图片链接] https://example.com/test.jpg"));
    }

    @Test
    void shouldExpandForwardImagesInPlainTextHistory() {
        BaniraBot bot = Mockito.mock(BaniraBot.class);
        Mockito.when(bot.getSelfId()).thenReturn(999L);
        String image = "[CQ:image,file=test.jpg,url=https://example.com/test.jpg]";
        MsgResp node = new MsgResp();
        node.setMessage(image);
        node.setArrayMsg(MessageConverser.stringToArray(image));
        ArrayMsg forward = BaniraUtils.packForwardMsg(null, List.of(node));

        String text = MessageConvert.toPlainText(bot, 10001L, forward, new ChatMessageContextFormatter.UserInfoCache());

        Assertions.assertTrue(text.contains("合并转发内容"));
        Assertions.assertTrue(text.contains("[图片链接] https://example.com/test.jpg"));
    }

    @Test
    void shouldConvertDataUrlImageToImageContentWhenMediaRetained() {
        BaniraBot bot = Mockito.mock(BaniraBot.class);
        Mockito.when(bot.getSelfId()).thenReturn(999L);
        ArrayMsg image = new ArrayMsg()
                .setType(MsgTypeEnum.image)
                .setData(Map.of("file", "test.png", "url", ONE_PIXEL_PNG_DATA_URL));

        List<Content> contents = MessageConvert.toContents(bot, 10001L, image, true, new ChatMessageContextFormatter.UserInfoCache());

        Assertions.assertTrue(contents.stream().anyMatch(ImageContent.class::isInstance));
    }
}
