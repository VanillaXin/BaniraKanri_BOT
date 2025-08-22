package xin.vanilla.banira.plugin;

import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.common.utils.ShiroUtils;
import com.mikuac.shiro.dto.action.common.ActionData;
import com.mikuac.shiro.dto.action.common.MsgId;
import com.mikuac.shiro.dto.action.response.LoginInfoResp;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import com.mikuac.shiro.enums.MsgTypeEnum;
import com.mikuac.shiro.model.ArrayMsg;
import jakarta.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * 图片表情转图片
 */
@Slf4j
@Shiro
@Component
public class ImageFaceToImagePlugin extends BasePlugin {

    private static final Set<String> helpType = BaniraUtils.mutableSetOf(
            "getface", "getimage", "获取表情", "获取图片"
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
        List<String> result = new ArrayList<>();
        String type = CollectionUtils.getFirst(types);
        if (helpType.stream().anyMatch(s -> StringUtils.isNullOrEmptyEx(type) || s.equalsIgnoreCase(type))) {
            result.add("表情转图片：\n" +
                    "使用指令回复带有图片的消息以获得其中的图片详情。\n\n" +
                    BaniraUtils.getInsPrefixWithSpace() +
                    globalConfig.get().otherConfig().imageFaceToImage()
            );
        }
        return result;
    }

    @AnyMessageHandler
    public boolean convert(BaniraBot bot, AnyMessageEvent event) {
        String message = event.getMessage();
        if (super.isCommand(message)
                && globalConfig.get().otherConfig().imageFaceToImage() != null
                && globalConfig.get().otherConfig().imageFaceToImage().contains(super.replaceCommand(message))
        ) {
            if (BaniraUtils.hasReply(event.getArrayMsg())) {
                List<ArrayMsg> replyContent = bot.getReplyContent(event.getArrayMsg());
                List<String> urls = replyContent.stream()
                        .filter(msg -> msg.getType() == MsgTypeEnum.image)
                        .map(msg -> msg.getStringData("url"))
                        .toList();
                if (!urls.isEmpty()) {
                    LoginInfoResp loginInfoEx = bot.getLoginInfoEx();
                    List<Map<String, Object>> msg = new ArrayList<>();
                    msg.add(ShiroUtils.generateSingleMsg(event.getUserId(), event.getSender().getNickname(), event.getMessage()));
                    urls.forEach(url -> msg.add(
                            ShiroUtils.generateSingleMsg(
                                    bot.getSelfId()
                                    , loginInfoEx.getNickname()
                                    , MsgUtils.builder().text(url).img(url).build()
                            )
                    ));
                    ActionData<MsgId> msgId = bot.sendForwardMsg(event, msg);
                    return bot.isActionDataMsgIdNotEmpty(msgId);
                } else {
                    return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
                }
            }
        }
        return false;
    }
}
