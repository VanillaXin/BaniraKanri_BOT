package xin.vanilla.banira.plugin;

import com.mikuac.shiro.annotation.GroupPokeNoticeHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.event.notice.PokeNoticeEvent;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Shiro
@Component
public class KanriPlugin {

    @GroupPokeNoticeHandler
    public boolean poke(Bot bot, PokeNoticeEvent event) {
        LOGGER.debug(event.toString());
        return false;
    }

}
