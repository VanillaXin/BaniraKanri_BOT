package xin.vanilla.banira.plugin;

import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.common.utils.MsgUtils;
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
import xin.vanilla.banira.plugin.socialmedia.SocialMediaContent;
import xin.vanilla.banira.plugin.socialmedia.SocialMediaParser;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * 社交媒体解析
 */
@Slf4j
@Shiro
@Component
public class SocialMediaPlugin extends BasePlugin {

    @Autowired(required = false)
    private List<SocialMediaParser> parsers = new ArrayList<>();

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
                groupConfig.get().otherConfig()
                        .computeIfAbsent(event.getGroupId(), k -> new OtherConfig())
                        .socialMedia(true);
                BaniraUtils.saveGroupConfig();
                return bot.setMsgEmojiLikeOk(event.getMessageId());
            }
            // 禁用
            else if (insConfig.get().base().disable().contains(operate)) {
                if (!bot.isAdmin(event.getGroupId(), event.getUserId()))
                    return bot.setMsgEmojiLikeNo(event.getMessageId());
                groupConfig.get().otherConfig()
                        .computeIfAbsent(event.getGroupId(), k -> new OtherConfig())
                        .socialMedia(false);
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
            if (parser.hasSocialMedia(event.getMessage())) {
                contents.addAll(parser.parse(event.getMessage()));
            }
        }
        if (CollectionUtils.isNotNullOrEmpty(contents)) {
            bot.setMsgEmojiLike(event.getMessageId(), 162);
            // 普通消息
            if (contents.size() == 1) {
                SocialMediaContent content = contents.getFirst();
                ActionData<MsgId> msgId = bot.sendMsg(event, content.msg(), false);

                if (!StringUtils.isNullOrEmptyEx(content.video())) {
                    bot.sendMsg(event, new MsgUtils().video(content.video(), content.cover()).build(), false);
                    // bot.uploadGroupFile(event.getGroupId(), content.video(), content.title() + ".mp4");
                }
                if (!StringUtils.isNullOrEmptyEx(content.audio()) && BaniraUtils.isGroupIdValid(event.getGroupId())) {
                    bot.uploadGroupFile(event.getGroupId(), content.audio(), content.title() + ".mp3");
                }

                return bot.isActionDataMsgIdNotEmpty(msgId)
                        ? bot.setMsgEmojiLikeHeart(event.getMessageId())
                        : bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            }
            // 合并转发
            else {
                // 一次解析这么多干嘛
                return bot.setMsgEmojiLikeNo(event.getMessageId());
            }
        }
        return false;
    }

    private boolean isEnable(Long groupId) {
        // 群组配置
        if (BaniraUtils.isGroupIdValid(groupId)) {
            OtherConfig config = groupConfig.get().otherConfig().getOrDefault(groupId, null);
            if (config != null) return config.socialMedia();
        }
        // 全局配置
        return groupConfig.get().otherConfig().computeIfAbsent(0L, k -> new OtherConfig()).socialMedia();
    }
}
