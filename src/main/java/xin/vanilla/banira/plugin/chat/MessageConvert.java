package xin.vanilla.banira.plugin.chat;

import com.mikuac.shiro.model.ArrayMsg;
import dev.langchain4j.data.message.Content;
import dev.langchain4j.data.message.ImageContent;
import dev.langchain4j.data.message.TextContent;
import dev.langchain4j.data.message.VideoContent;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.List;

public final class MessageConvert {

    public static Content toContent(BaniraBot bot, Long groupId, ArrayMsg arrayMsg, boolean retainMedia) {
        switch (arrayMsg.getType()) {
            case at: {
                long qq = arrayMsg.getLongData("qq");
                if (qq == 0) return new TextContent("@全体成员");
                else return new TextContent("@" + bot.getUserNameEx(groupId, qq));
            }
            case image: {
                if (retainMedia) {
                    return new ImageContent(arrayMsg.getStringData("url"));
                } else {
                    return new TextContent("[图片]");
                }
            }
            case video: {
                if (retainMedia) {
                    return new VideoContent(arrayMsg.getStringData("url"));
                } else {
                    return new TextContent("[视频]");
                }
            }
            case reply: {
                return new TextContent(String.format("引用[%s]", BaniraUtils.getReplyContentString(bot, List.of(arrayMsg))));
            }
            case text: {
                return new TextContent(arrayMsg.toCQCode());
            }
            default:
                return null;
        }

    }

}
