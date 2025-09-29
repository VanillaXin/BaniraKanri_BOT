package xin.vanilla.banira.plugin.chat;

import com.mikuac.shiro.model.ArrayMsg;
import dev.langchain4j.data.message.Content;
import dev.langchain4j.data.message.ImageContent;
import dev.langchain4j.data.message.TextContent;
import dev.langchain4j.data.message.VideoContent;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.HttpUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.Base64;
import java.util.List;

public final class MessageConvert {

    public static Content toContent(BaniraBot bot, Long groupId, ArrayMsg arrayMsg, boolean retainMedia) {
        Content result = null;
        try {
            switch (arrayMsg.getType()) {
                case at: {
                    long qq = arrayMsg.getLongData("qq");
                    if (qq == 0) result = new TextContent("@全体成员");
                    else result = new TextContent(String.format("@%s ", bot.getUserNameEx(groupId, qq)));
                }
                break;
                case image: {
                    if (retainMedia) {
                        String url = arrayMsg.getStringData("url");
                        String base64 = Base64.getEncoder().encodeToString(HttpUtils.downloadBytes(url));
                        result = new ImageContent(base64, "image/gif");
                    } else {
                        result = new TextContent("[图片]");
                    }
                }
                break;
                case video: {
                    // if (retainMedia) {
                    //     String url = arrayMsg.getStringData("url");
                    //     String base64 = Base64.getEncoder().encodeToString(HttpUtils.downloadBytes(url));
                    //     result = new VideoContent(base64, "video/mp4");
                    // } else {
                    //     result = new TextContent("[视频]");
                    // }
                }
                case reply: {
                    String replyContent = BaniraUtils.getReplyContentString(bot, List.of(arrayMsg));
                    if (!StringUtils.isNullOrEmptyEx(replyContent)) {
                        result = new TextContent(String.format("引用[%s]", replyContent));
                    }
                }
                case text: {
                    String text = arrayMsg.toCQCode();
                    if (!StringUtils.isNullOrEmptyEx(text)) {
                        result = new TextContent(text);
                    }
                }
            }
        } catch (Exception ignored) {
        }
        return result;
    }

}
