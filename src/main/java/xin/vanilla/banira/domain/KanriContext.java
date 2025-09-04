package xin.vanilla.banira.domain;

import com.mikuac.shiro.dto.event.message.MessageEvent;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.util.BaniraUtils;

@Setter
@Getter
@Accessors(chain = true, fluent = true)
public class KanriContext {

    /**
     * 原事件
     */
    private final MessageEvent event;
    /**
     * BOT
     */
    private final BaniraBot bot;
    /**
     * 指令作用群
     */
    private final long group;
    /**
     * 指令作用人
     */
    private final long sender;
    /**
     * 消息ID
     */
    private final int msgId;
    /**
     * 频道消息ID
     */
    private final String guildMsgId;
    /**
     * 去除前缀后的消息
     */
    private final String content;

    private boolean coder = false;

    public KanriContext(MessageEvent event, BaniraBot bot, long group, long sender, int msgId, String guildMsgId, String content) {
        this.event = event;
        this.bot = bot;
        this.group = group;
        this.sender = sender;
        this.msgId = msgId;
        this.guildMsgId = guildMsgId;
        this.content = content;
    }

    public static int getMsgId(MessageEvent event) {
        return BaniraUtils.getMsgId(event);
    }

    public static String getGuildMsgId(MessageEvent event) {
        return BaniraUtils.getGuildMsgId(event);
    }
}
