package xin.vanilla.banira.domain;

import com.mikuac.shiro.dto.event.message.MessageEvent;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.util.BaniraUtils;

public record KanriContext(
        MessageEvent event,
        BaniraBot bot,
        long group,
        long sender,
        int msgId,
        String guildMsgId
) {

    public static int getMsgId(MessageEvent event) {
        return BaniraUtils.getMsgId(event);
    }

    public static String getGuildMsgId(MessageEvent event) {
        return BaniraUtils.getGuildMsgId(event);
    }
}
