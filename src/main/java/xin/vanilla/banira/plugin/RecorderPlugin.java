package xin.vanilla.banira.plugin;

import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.common.utils.JsonUtils;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.action.common.ActionData;
import com.mikuac.shiro.dto.action.response.GetForwardMsgResp;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.enums.EnumMessageType;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.service.IMessageRecordManager;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.List;
import java.util.Set;

/**
 * 消息记录
 */
@Slf4j
@Shiro
@Component
public class RecorderPlugin extends BasePlugin {

    @Resource
    private IMessageRecordManager messageRecordManager;

    private static final Set<String> helpType = BaniraUtils.mutableSetOf(
            "msgrecord"
    );

    /**
     * 获取帮助信息
     *
     * @param groupId 群组ID
     * @param types   帮助类型
     */
    @Nonnull
    @Override
    public List<String> getHelpInfo(Long groupId, @Nonnull String... types) {
        return List.of();
    }

    @AnyMessageHandler
    public void recorder(Bot tob, AnyMessageEvent event) {
        BaniraBot bot = new BaniraBot(tob);

        LOGGER.debug("String: {}", event.getMessage());
        LOGGER.debug("Raw: {}", event.getRawMessage());
        LOGGER.debug("Array: {}", event.getArrayMsg());
        MessageRecord record = new MessageRecord()
                .setMsgId(event.getMessageId())
                .setBotId(bot.getSelfId())
                .setSenderId(event.getUserId())
                .setGroupId(event.getGroupId())
                .setTime(event.getTime())
                .setMsgRaw(JsonUtils.toJSONString(event.getArrayMsg()))
                .setMsgRecode(event.getMessage())
                .setMsgType(EnumMessageType.getType(event));
        if (record.getMsgType() == EnumMessageType.MEMBER) {
            record.setTargetId(event.getUserId());
        }

        // 解析转发消息
        if (BaniraUtils.hasForward(event.getArrayMsg())) {
            ActionData<GetForwardMsgResp> forwardMsg = bot.getForwardMsg(event.getMessageId());
            if (bot.isActionDataNotEmpty(forwardMsg)) {
                record.setMsgRecode(
                        BaniraUtils.encodeForwardMsg(
                                BaniraUtils.getForwardId(event.getArrayMsg())
                                , forwardMsg.getData().getMessages()
                        )
                );
            }
        }

        messageRecordManager.addMessageRecord(record);
    }
}
