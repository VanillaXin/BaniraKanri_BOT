package xin.vanilla.banira.plugin;

import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.enums.EnumMessageType;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.service.IMessageRecordManager;

/**
 * 消息记录
 */
@Slf4j
@Shiro
@Component
public class RecorderPlugin extends BasePlugin {

    @Resource
    private IMessageRecordManager messageRecordManager;

    @AnyMessageHandler
    public void recorder(Bot bot, AnyMessageEvent event) {
        LOGGER.debug("String: {}", event.getMessage());
        LOGGER.debug("Raw: {}", event.getRawMessage());
        LOGGER.debug("Array: {}", event.getArrayMsg());
        MessageRecord record = new MessageRecord()
                .setNos(String.valueOf(event.getMessageId()))
                .setBot(bot.getSelfId())
                .setSender(event.getUserId())
                .setTarget(event.getGroupId())
                .setTime(event.getTime())
                .setMsgRaw(event.getMessage())
                .setMsgType(EnumMessageType.getType(event));

        messageRecordManager.addMessageRecord(record);
    }
}
