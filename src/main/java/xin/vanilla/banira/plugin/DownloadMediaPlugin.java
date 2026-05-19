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
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.KeyValue;
import xin.vanilla.banira.enums.EnumCacheFileType;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.plugin.help.HelpTopic;
import xin.vanilla.banira.plugin.help.HelpTopics;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * 媒体资源下载
 */
@Slf4j
@Shiro
@Component
public class DownloadMediaPlugin extends BasePlugin {

    @Override
    public void registerHelpTopics(@Nonnull List<HelpTopic> topics, Long groupId) {
        topics.add(HelpTopics.of("媒体资源下载", "回复带有媒体资源的消息以获取其中的媒体详情。", 99, insConfig.get().media())
                .detail(BaniraUtils.getInsPrefixWithSpace() + insConfig.get().media()));
    }

    @AnyMessageHandler
    public boolean convert(BaniraBot bot, AnyMessageEvent event) {
        BaniraCodeContext context = new BaniraCodeContext(bot, event);
        if (super.isCommand(context)
                && insConfig.get().media().contains(super.deleteCommandPrefix(context))
        ) {
            if (BaniraUtils.hasReply(event.getArrayMsg())) {
                List<ArrayMsg> replyContent = bot.getReplyContent(event.getArrayMsg());
                List<KeyValue<Boolean, String>> urls = replyContent.stream()
                        .filter(msg -> msg.getType() == MsgTypeEnum.image
                                || msg.getType() == MsgTypeEnum.video
                                || msg.getType() == MsgTypeEnum.record
                                || (msg.getType() == MsgTypeEnum.unknown && StringUtils.isNotNullOrEmpty(msg.getStringData("file_id")))
                        )
                        .map(msg -> new KeyValue<>(msg.getType() == MsgTypeEnum.image, msg.getStringData("url")))
                        .toList();
                if (!urls.isEmpty()) {
                    LoginInfoResp loginInfoEx = bot.getLoginInfoEx();
                    List<Map<String, Object>> msg = new ArrayList<>();
                    msg.add(ShiroUtils.generateSingleMsg(event.getUserId(), event.getSender().getNickname(), event.getMessage()));
                    urls.forEach(url -> {
                        MsgUtils msgUtils = MsgUtils.builder().text(url.getValue());
                        if (url.getKey()) msgUtils.img(url.getValue());
                        else {
                            String fileName = BaniraUtils.downloadFileToCachePath(url.getValue(), EnumCacheFileType.file);
                            String filePath = BaniraUtils.getCacheAbsolutePath(fileName, EnumCacheFileType.file);
                            if (BaniraUtils.isGroupIdValid(event.getGroupId())) {
                                bot.uploadGroupFile(event.getGroupId(), filePath, StringUtils.md5(fileName), "");
                            } else {
                                bot.uploadPrivateFile(event.getSender().getUserId(), filePath, StringUtils.md5(fileName));
                            }
                        }
                        msg.add(
                                ShiroUtils.generateSingleMsg(
                                        bot.getSelfId()
                                        , loginInfoEx.getNickname()
                                        , msgUtils.build()
                                )
                        );
                    });
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
