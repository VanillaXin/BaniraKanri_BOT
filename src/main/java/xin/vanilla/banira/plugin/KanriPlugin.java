package xin.vanilla.banira.plugin;

import com.mikuac.shiro.annotation.GroupMessageHandler;
import com.mikuac.shiro.annotation.PrivateMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import com.mikuac.shiro.dto.event.message.MessageEvent;
import com.mikuac.shiro.dto.event.message.PrivateMessageEvent;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.coder.ToGroupCode;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.plugin.kanri.KanriHandler;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

/**
 * 群管指令
 */
@Slf4j
@Shiro
@Component
public class KanriPlugin extends BasePlugin {
    @Autowired(required = false)
    private List<KanriHandler> handlers = new ArrayList<>();
    @Resource
    private ToGroupCode toGroupCode;

    @GroupMessageHandler
    public boolean group(Bot tob, GroupMessageEvent event) {
        BaniraBot bot = new BaniraBot(tob);

        BaniraCodeContext code = toGroupCode.execute(new BaniraCodeContext(bot, event.getArrayMsg())
                .setSender(event.getUserId())
                .setGroup(event.getGroupId())
                .setMsg(event.getMessage())
        );

        if (BaniraUtils.isGroupIdValid(code.getGroup())) {
            return execute(bot, event, code.getMsg(), code.getGroup(), event.getMessageId());
        }
        return false;
    }

    @PrivateMessageHandler
    public boolean friend(Bot tob, PrivateMessageEvent event) {
        BaniraBot bot = new BaniraBot(tob);

        BaniraCodeContext code = toGroupCode.execute(new BaniraCodeContext(bot, event.getArrayMsg())
                .setSender(event.getUserId())
                .setTarget(event.getSelfId())
                .setMsg(event.getMessage())
        );

        if (BaniraUtils.isGroupIdValid(code.getGroup())) {
            return execute(bot, event, code.getMsg(), code.getGroup(), event.getMessageId());
        }
        return false;
    }

    private boolean execute(BaniraBot bot, MessageEvent event, String message, long groupId, int msgId) {
        if (!super.isKanriCommand(message)) return false;
        message = super.replaceKanriCommand(message);

        String[] parts = message.split("\\s+");
        String kanriAction = parts[0].trim();

        KanriContext context = new KanriContext(event
                , bot
                , groupId
                , event.getUserId()
                , KanriContext.getMsgId(event)
                , KanriContext.getGuildMsgId(event)
                , message.replace(parts[0], "").stripLeading()
        );

        int result = KanriHandler.NIL;
        Optional<KanriHandler> handler = handlers.stream()
                .filter(h -> h.getAction().contains(kanriAction))
                .findFirst();
        if (handler.isPresent()) {
            if (!handler.get().botHasPermission(context)) {
                result = KanriHandler.BOT_NO_OP;
            } else if (handler.get().hasPermission(context)) {
                String[] args = Arrays.copyOfRange(parts, 1, parts.length);
                try {
                    result = handler.get().execute(context, args);
                } catch (Exception e) {
                    LOGGER.error("Kanri command parsing failed", e);
                    result = KanriHandler.FAIL;
                }
            } else {
                result = KanriHandler.NO_OP;
            }
        }

        String emoji;
        switch (result) {
            // no
            case KanriHandler.NO_OP -> emoji = "123";
            // sleep
            case KanriHandler.BOT_NO_OP -> emoji = "8";
            // broken heart
            case KanriHandler.FAIL -> emoji = "67";
            // ok
            case KanriHandler.SUCCESS -> emoji = "124";
            default -> emoji = null;
        }
        if (StringUtils.isNotNullOrEmpty(emoji)) {
            bot.setMsgEmojiLike(msgId, emoji, true);
        }

        return result == KanriHandler.SUCCESS;
    }

}
