package xin.vanilla.banira.domain.kanri;

import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.event.message.MessageEvent;

public record KanriContext(
        MessageEvent event,
        Bot bot,
        long group,
        long sender
) {
}
