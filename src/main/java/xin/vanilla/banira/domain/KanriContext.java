package xin.vanilla.banira.domain;

import com.mikuac.shiro.dto.event.message.MessageEvent;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.util.BaniraUtils;

/**
 * @param event      原事件
 * @param bot        BOT
 * @param group      指令作用群
 * @param sender     指令作用人
 * @param msgId      消息ID
 * @param guildMsgId 频道消息ID
 * @param content    去除前缀后的消息
 */
public record KanriContext(
        MessageEvent event,
        BaniraBot bot,
        long group,
        long sender,
        int msgId,
        String guildMsgId,
        String content
) {

    public static int getMsgId(MessageEvent event) {
        return BaniraUtils.getMsgId(event);
    }

    public static String getGuildMsgId(MessageEvent event) {
        return BaniraUtils.getGuildMsgId(event);
    }
}
