package xin.vanilla.banira.plugin;

import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.MessageHandlerFilter;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import com.mikuac.shiro.enums.AtEnum;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.plugin.common.BasePlugin;

@Slf4j
@Shiro
@Component
public class ExamplePlugin extends BasePlugin {

    /**
     * 被AT时触发回复
     *
     * @param bot   机器人实例
     * @param event 消息事件
     * @return 是否拦截事件传递
     */
    @AnyMessageHandler
    @MessageHandlerFilter(at = AtEnum.NEED)
    public boolean hello(Bot bot, AnyMessageEvent event) {
        // LOGGER.debug(event.getMessage());
        bot.sendMsg(event, "Hello Excel!", false);
        return false;
    }

}
