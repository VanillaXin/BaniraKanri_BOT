package xin.vanilla.banira.plugin;

import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.action.common.ActionData;
import com.mikuac.shiro.dto.action.common.MsgId;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import com.mikuac.shiro.enums.MsgTypeEnum;
import com.mikuac.shiro.model.ArrayMsg;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.List;

/**
 * 图片表情转图片
 */
@Slf4j
@Shiro
@Component
public class ImageFaceToImagePlugin extends BasePlugin {

    @AnyMessageHandler
    public boolean convert(Bot tob, AnyMessageEvent event) {
        BaniraBot bot = new BaniraBot(tob);
        String message = event.getMessage();
        if (super.isCommand(message)
                && globalConfig.get().otherConfig().imageFaceToImage() != null
                && globalConfig.get().otherConfig().imageFaceToImage().contains(super.replaceCommand(message))
        ) {
            if (BaniraUtils.hasReply(event.getArrayMsg())) {
                List<ArrayMsg> replayContent = bot.getReplayContent(event.getArrayMsg());
                MsgUtils builder = MsgUtils.builder()
                        .reply(event.getMessageId());
                replayContent.stream()
                        .filter(msg -> msg.getType() == MsgTypeEnum.image)
                        .map(msg -> msg.getStringData("url"))
                        .forEach(url -> builder.text(url).img(url));
                ActionData<MsgId> msgId = bot.sendMsg(event, builder.build(), false);
                return bot.isActionDataMsgIdNotEmpty(msgId);
            }
        }
        return false;
    }
}
