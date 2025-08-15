package xin.vanilla.banira.plugin;

import com.mikuac.shiro.annotation.GroupMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.plugin.kanri.KanriHandler;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

@Slf4j
@Shiro
@Component
public class KanriPlugin extends BasePlugin {
    @Autowired(required = false)
    private List<KanriHandler> handlers = new ArrayList<>();

    @GroupMessageHandler
    public boolean kanri(Bot tob, GroupMessageEvent event) {
        BaniraBot bot = new BaniraBot(tob);

        String message = event.getMessage();
        if (!super.isKanriCommand(message)) return false;
        message = super.replaceKanriCommand(message);

        String[] parts = message.split("\\s+");
        String kanriAction = parts[0].trim();

        KanriContext context = new KanriContext(event
                , bot
                , event.getGroupId()
                , event.getSender().getUserId()
                , KanriContext.getMsgId(event)
                , KanriContext.getGuildMsgId(event)
                , message
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
            case KanriHandler.NO_OP -> emoji = String.valueOf(123);
            // sleep
            case KanriHandler.BOT_NO_OP -> emoji = String.valueOf(8);
            // broken heart
            case KanriHandler.FAIL -> emoji = String.valueOf(67);
            // ok
            case KanriHandler.SUCCESS -> emoji = String.valueOf(124);
            default -> emoji = null;
        }
        if (StringUtils.isNotNullOrEmpty(emoji)) {
            bot.setMsgEmojiLike(event.getMessageId(), emoji, true);
        }

        return result == KanriHandler.SUCCESS;
    }

}
