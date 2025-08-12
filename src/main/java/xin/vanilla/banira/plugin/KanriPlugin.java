package xin.vanilla.banira.plugin;

import com.mikuac.shiro.annotation.GroupMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.domain.BaniraBot;
import xin.vanilla.banira.domain.kanri.KanriContext;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.plugin.kanri.KanriHandler;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

@Slf4j
@Shiro
@Component
public class KanriPlugin extends BasePlugin {
    @Resource
    private List<KanriHandler> handlers;

    @GroupMessageHandler
    public boolean kanri(Bot tob, GroupMessageEvent event) {
        BaniraBot bot = new BaniraBot(tob);

        String message = event.getMessage();
        if (!super.isKanriCommand(message)) return false;
        message = super.replaceKanriCommand(message);

        String[] parts = message.split("\\s+");
        String kanriAction = parts[0].trim();

        KanriContext context = new KanriContext(event, bot, event.getGroupId(), event.getSender().getUserId());

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
                    bot.sendGroupMsg(event.getGroupId(), "指令解析失败", false);
                }
            } else {
                result = KanriHandler.NO_PERMISSION;
            }
        }
        if (result == KanriHandler.NO_PERMISSION) {
            bot.sendGroupMsg(event.getGroupId()
                    , MsgUtils.builder()
                            .reply(event.getMessageId())
                            .text("你没有权限执行该操作")
                            .build()
                    , false
            );
        }
        return result == KanriHandler.SUCCESS;
    }

}
