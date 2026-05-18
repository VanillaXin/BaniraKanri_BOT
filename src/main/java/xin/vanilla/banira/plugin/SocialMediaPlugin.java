package xin.vanilla.banira.plugin;

import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.common.utils.ShiroUtils;
import com.mikuac.shiro.dto.action.common.ActionData;
import com.mikuac.shiro.dto.action.common.MsgId;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.basic.OtherConfig;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.plugin.socialmedia.SocialMediaApiService;
import xin.vanilla.banira.plugin.socialmedia.SocialMediaContent;
import xin.vanilla.banira.plugin.socialmedia.SocialMediaParser;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * 社交媒体解析
 */
@Slf4j
@Shiro
@Component
public class SocialMediaPlugin extends BasePlugin {

    @Autowired(required = false)
    private List<SocialMediaParser> parsers = new ArrayList<>();
    @Autowired
    private SocialMediaApiService socialMediaApiService;

    /**
     * 获取帮助信息
     *
     * @param groupId 群组ID
     * @param types   帮助类型
     */
    @Nonnull
    @Override
    public List<String> getHelpInfo(@Nullable Long groupId, @Nonnull String... types) {
        List<String> result = new ArrayList<>();
        String type = CollectionUtils.getFirst(types);
        List<String> socialMedia = insConfig.get().socialMedia();
        if (socialMedia.stream().anyMatch(s -> StringUtils.isNullOrEmptyEx(type) || s.equalsIgnoreCase(type))) {
            result.add("社交媒体解析：\n" +
                    "自动解析设精媒体链接。\n\n" +
                    "启用：\n" +
                    BaniraUtils.getInsPrefixWithSpace() +
                    socialMedia + " " +
                    insConfig.get().base().enable() + "\n\n" +
                    "禁用：\n" +
                    BaniraUtils.getInsPrefixWithSpace() +
                    socialMedia + " " +
                    insConfig.get().base().disable()
            );
        }
        return result;
    }

    @AnyMessageHandler
    public boolean config(BaniraBot bot, AnyMessageEvent event) {
        BaniraCodeContext context = new BaniraCodeContext(bot, event);
        if (super.isCommand(context)
                && insConfig.get().socialMedia().stream().anyMatch(ins -> super.deleteCommandPrefix(context).startsWith(ins + " "))
        ) {
            String argString = super.deleteCommandPrefix(context);
            String[] split = argString.split("\\s+");
            if (split.length < 2) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());

            String operate = split[1];
            // 启用
            if (insConfig.get().base().enable().contains(operate)) {
                if (!bot.isAdmin(event.getGroupId(), event.getUserId()))
                    return bot.setMsgEmojiLikeNo(event.getMessageId());
                BaniraUtils.getOthersConfig(event.getGroupId()).socialMedia(true);
                BaniraUtils.saveGroupConfig();
                return bot.setMsgEmojiLikeOk(event.getMessageId());
            }
            // 禁用
            else if (insConfig.get().base().disable().contains(operate)) {
                if (!bot.isAdmin(event.getGroupId(), event.getUserId()))
                    return bot.setMsgEmojiLikeNo(event.getMessageId());
                BaniraUtils.getOthersConfig(event.getGroupId()).socialMedia(false);
                BaniraUtils.saveGroupConfig();
                return bot.setMsgEmojiLikeOk(event.getMessageId());
            }
            // 未知
            else {
                return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            }
        }
        return false;
    }

    @AnyMessageHandler
    public boolean parse(BaniraBot bot, AnyMessageEvent event) {
        if (!isEnable(event.getGroupId())) return false;

        List<SocialMediaContent> contents = new ArrayList<>();
        for (SocialMediaParser parser : parsers) {
            contents.addAll(socialMediaApiService.parse(parser, event.getMessage()));
        }
        if (CollectionUtils.isNotNullOrEmpty(contents)) {
            bot.setMsgEmojiLike(event.getMessageId(), 162);
            List<Map<String, Object>> forwardMsg = buildForwardMsg(bot, contents);
            if (CollectionUtils.isNullOrEmpty(forwardMsg)) {
                return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            }
            ActionData<MsgId> msgId = bot.sendForwardMsg(event, forwardMsg);
            return bot.isActionDataMsgIdNotEmpty(msgId)
                    ? bot.setMsgEmojiLikeHeart(event.getMessageId())
                    : bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
        }
        return false;
    }

    private List<Map<String, Object>> buildForwardMsg(BaniraBot bot, List<SocialMediaContent> contents) {
        List<Map<String, Object>> forwardMsg = new ArrayList<>();
        Long botId = bot.getSelfId();
        String botName = bot.getLoginInfoEx().getNickname();

        for (SocialMediaContent content : contents) {
            if (StringUtils.isNotNullOrEmpty(content.msg())) {
                forwardMsg.add(ShiroUtils.generateSingleMsg(botId, botName, content.msg()));
            }
            if (StringUtils.isNotNullOrEmpty(content.video())) {
                forwardMsg.add(ShiroUtils.generateSingleMsg(
                        botId,
                        botName,
                        MsgUtils.builder().video(content.video(), content.cover()).build()
                ));
            }
        }
        return forwardMsg;
    }

    private boolean isEnable(Long groupId) {
        // 群组配置
        if (BaniraUtils.hasGroupOthersConfig(groupId)) {
            OtherConfig config = BaniraUtils.getOthersConfig(groupId);
            if (config != null) return config.socialMedia();
        }
        // 全局配置
        return BaniraUtils.getOthersConfig().socialMedia();
    }
}
