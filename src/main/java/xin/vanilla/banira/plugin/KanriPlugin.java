package xin.vanilla.banira.plugin;

import com.mikuac.shiro.annotation.GroupMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.common.utils.FaceUtils;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.plugin.kanri.KanriHandler;

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
        );

        int result = KanriHandler.FAIL;
        Optional<KanriHandler> handler = handlers.stream()
                .filter(h -> h.getAction().contains(kanriAction))
                .findFirst();
        if (handler.isPresent()) {
            if (handler.get().hasPermission(context)) {
                String[] args = Arrays.copyOfRange(parts, 1, parts.length);
                try {
                    result = handler.get().execute(context, args);
                } catch (Exception e) {
                    LOGGER.error("Kanri command parsing failed", e);
                    // bot.sendGroupMsg(event.getGroupId(), "指令解析失败", false);
                    bot.setMsgEmojiLike(event.getMessageId(), String.valueOf(FaceUtils.get(67)), true);
                }
            } else {
                result = KanriHandler.NO_PERMISSION;
            }
        }
        if (result == KanriHandler.NO_PERMISSION) {
            // bot.sendGroupMsg(event.getGroupId()
            //         , MsgUtils.builder()
            //                 .reply(event.getMessageId())
            //                 .text("你没有权限执行该操作")
            //                 .build()
            //         , false
            // );
            bot.setMsgEmojiLike(event.getMessageId(), String.valueOf(FaceUtils.get(123)), true);
        } else if (result == KanriHandler.FAIL) {
            bot.setMsgEmojiLike(event.getMessageId(), String.valueOf(FaceUtils.get(67)), true);
        }
        return result == KanriHandler.SUCCESS;
    }

}
