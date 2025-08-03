package xin.vanilla.banira.plugin;

import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.MessageHandlerFilter;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import com.mikuac.shiro.enums.AtEnum;
import org.springframework.stereotype.Component;

@Shiro
@Component
public class ExamplePlugin {

    @AnyMessageHandler
    @MessageHandlerFilter(at = AtEnum.NEED)
    public void hello(Bot bot, AnyMessageEvent event) {
        bot.sendMsg(event, "Hello Excel!", false);
    }

}
