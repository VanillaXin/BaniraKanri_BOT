package xin.vanilla.banira.plugin;

import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.GroupPokeNoticeHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import com.mikuac.shiro.dto.event.notice.PokeNoticeEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Shiro
@Component
public class KanriPlugin {

    private static final Logger logger = LoggerFactory.getLogger(KanriPlugin.class);

    @AnyMessageHandler
    public void log(Bot bot, AnyMessageEvent event) {
        logger.debug(event.getMessage());
    }

    @GroupPokeNoticeHandler
    public void poke(Bot bot, PokeNoticeEvent event) {
        logger.debug(event.toString());
    }

}
