package xin.vanilla.banira.plugin.filedownload;

import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.dto.action.common.ActionData;
import com.mikuac.shiro.dto.action.common.MsgId;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.util.StringUtils;

/**
 * 文件下载插件统一反馈
 */
public final class FileDownloadFeedback {

    private FileDownloadFeedback() {
    }

    public static boolean accept(BaniraBot bot, int messageId) {
        return bot.setMsgEmojiLikeOk(messageId);
    }

    public static boolean success(BaniraBot bot, int messageId) {
        return bot.setMsgEmojiLikeHeart(messageId);
    }

    public static boolean fail(BaniraBot bot, AnyMessageEvent event, String reason) {
        boolean emoji = bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
        ActionData<MsgId> msgId = bot.sendMsg(event,
                MsgUtils.builder().reply(event.getMessageId()).text(formatError(reason)).build(),
                false);
        return emoji || bot.isActionDataMsgIdNotEmpty(msgId);
    }

    public static String formatError(String reason) {
        if (StringUtils.isNullOrEmptyEx(reason)) {
            return "下载错误：未知错误";
        }
        String text = reason.trim();
        if (text.startsWith("下载错误：") || text.startsWith("下载错误:")) {
            return text.startsWith("下载错误:") ? text.replaceFirst("下载错误:", "下载错误：") : text;
        }
        return "下载错误：" + text;
    }

}
