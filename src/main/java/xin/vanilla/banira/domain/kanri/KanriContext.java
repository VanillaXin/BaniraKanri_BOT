package xin.vanilla.banira.domain.kanri;

import com.mikuac.shiro.dto.event.message.MessageEvent;
import xin.vanilla.banira.domain.BaniraBot;

public record KanriContext(
        MessageEvent event,
        BaniraBot bot,
        long group,
        long sender
) {
}
