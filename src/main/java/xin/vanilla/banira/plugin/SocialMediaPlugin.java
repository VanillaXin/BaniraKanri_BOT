package xin.vanilla.banira.plugin;

import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.MessageEmojiLikeNoticeHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.common.utils.ShiroUtils;
import com.mikuac.shiro.dto.action.common.ActionData;
import com.mikuac.shiro.dto.action.common.MsgId;
import com.mikuac.shiro.dto.action.response.MsgResp;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import com.mikuac.shiro.dto.event.notice.MessageEmojiLikeNoticeEvent;
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
import xin.vanilla.banira.plugin.socialmedia.SocialMediaGroupSettings;
import xin.vanilla.banira.plugin.socialmedia.SocialMediaParser;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.*;

/**
 * 社交媒体解析
 */
@Slf4j
@Shiro
@Component
public class SocialMediaPlugin extends BasePlugin {
    private static final String REPLY_MODE_FORWARD = "forward";
    private static final String REPLY_MODE_DETAIL = "detail";
    private static final String REPLY_MODE_VIDEO = "video";

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

        SocialMediaGroupSettings settings = getGroupSettings(event.getGroupId());
        boolean directTrigger = settings.triggerDirect() && hasAnyParseTarget(event.getMessage());
        boolean replyTrigger = false;
        String replyMsg = "";
        if (settings.triggerReplyInvoke() && isReplyInvokeTrigger(bot, event)) {
            replyMsg = BaniraUtils.getReplyContentString(bot, event.getMessage());
            replyTrigger = hasAnyParseTarget(replyMsg);
        }
        if (!directTrigger && !replyTrigger) {
            return false;
        }
        bot.setMsgEmojiLike(event.getMessageId(), 162);

        List<SocialMediaContent> contents = new ArrayList<>();
        if (directTrigger) {
            contents.addAll(parseContents(event.getMessage()));
        }
        if (replyTrigger) {
            contents.addAll(parseContents(replyMsg));
        }
        contents = distinctContents(contents);
        if (CollectionUtils.isNotNullOrEmpty(contents)) {
            boolean success = sendByReplyMode(bot, event, contents, settings.replyMode());
            return success
                    ? bot.setMsgEmojiLikeHeart(event.getMessageId())
                    : bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
        }
        return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
    }

    @MessageEmojiLikeNoticeHandler
    public void parse(BaniraBot bot, MessageEmojiLikeNoticeEvent event) {
        if (!isEnable(event.getGroupId())) return;

        SocialMediaGroupSettings settings = getGroupSettings(event.getGroupId());
        if (!settings.triggerEmojiLikeNotice()) {
            return;
        }
        if (!Boolean.TRUE.equals(event.getIsAdd())) {
            return;
        }
        if (!isEmojiMatched(event, settings)) {
            return;
        }
        if (event.getMessageId() == null || event.getMessageId() <= 0) {
            return;
        }
        String likedMsg = getMessageContentById(bot, event.getMessageId());
        if (StringUtils.isNullOrEmptyEx(likedMsg)) {
            return;
        }
        if (!hasAnyParseTarget(likedMsg)) {
            return;
        }
        bot.setMsgEmojiLike(event.getMessageId(), 162);
        List<SocialMediaContent> contents = distinctContents(parseContents(likedMsg));
        if (CollectionUtils.isNullOrEmpty(contents)) {
            bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            return;
        }
        boolean success = sendByReplyMode(bot, event, contents, settings.replyMode());
        if (success) {
            bot.setMsgEmojiLikeHeart(event.getMessageId());
        } else {
            bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
        }
    }

    private List<SocialMediaContent> parseContents(String msg) {
        List<SocialMediaContent> contents = new ArrayList<>();
        if (StringUtils.isNullOrEmptyEx(msg)) {
            return contents;
        }
        for (SocialMediaParser parser : parsers) {
            contents.addAll(socialMediaApiService.parse(parser, msg));
        }
        return contents;
    }

    private boolean hasAnyParseTarget(String msg) {
        if (StringUtils.isNullOrEmptyEx(msg)) {
            return false;
        }
        for (SocialMediaParser parser : parsers) {
            if (parser.hasSocialMedia(msg)) {
                return true;
            }
        }
        return false;
    }

    private List<SocialMediaContent> distinctContents(List<SocialMediaContent> contents) {
        Map<String, SocialMediaContent> unique = new LinkedHashMap<>();
        for (SocialMediaContent content : contents) {
            if (content == null) continue;
            String key = String.join("|",
                    Objects.toString(content.url(), ""),
                    Objects.toString(content.video(), ""),
                    Objects.toString(content.msg(), "")
            );
            unique.putIfAbsent(key, content);
        }
        return new ArrayList<>(unique.values());
    }

    private boolean isReplyInvokeTrigger(BaniraBot bot, AnyMessageEvent event) {
        if (!BaniraUtils.hasReply(event.getMessage())) {
            return false;
        }
        boolean isMentioned = bot.isMentioned(event.getMessage());
        boolean isParseCommand = isParseInvokeCommand(bot, event);
        return isMentioned || isParseCommand;
    }

    private boolean isParseInvokeCommand(BaniraBot bot, AnyMessageEvent event) {
        BaniraCodeContext context = new BaniraCodeContext(bot, event);
        if (!super.isCommand(context)) {
            return false;
        }
        String content = super.deleteCommandPrefix(context).trim();
        if (StringUtils.isNullOrEmptyEx(content)) {
            return false;
        }
        for (String ins : insConfig.get().socialMedia()) {
            if (StringUtils.isNullOrEmptyEx(ins)) continue;
            if (content.equalsIgnoreCase(ins)) {
                return true;
            }
            if (content.startsWith(ins + " ")) {
                String[] split = content.split("\\s+");
                return split.length == 2 && insConfig.get().base().list().contains(split[1]);
            }
        }
        return false;
    }

    private boolean sendByReplyMode(BaniraBot bot, AnyMessageEvent event, List<SocialMediaContent> contents, String mode) {
        String normalizedMode = normalizeReplyMode(mode);
        return switch (normalizedMode) {
            case REPLY_MODE_DETAIL -> sendDetails(bot, event, contents);
            case REPLY_MODE_VIDEO -> sendVideos(bot, event, contents);
            default -> sendForward(bot, event, contents);
        };
    }

    private boolean sendByReplyMode(BaniraBot bot, MessageEmojiLikeNoticeEvent event, List<SocialMediaContent> contents, String mode) {
        String normalizedMode = normalizeReplyMode(mode);
        return switch (normalizedMode) {
            case REPLY_MODE_DETAIL -> sendDetails(bot, event, contents);
            case REPLY_MODE_VIDEO -> sendVideos(bot, event, contents);
            default -> sendForward(bot, event, contents);
        };
    }

    private boolean sendForward(BaniraBot bot, AnyMessageEvent event, List<SocialMediaContent> contents) {
        List<Map<String, Object>> forwardMsg = buildForwardMsg(bot, contents);
        if (CollectionUtils.isNullOrEmpty(forwardMsg)) {
            return false;
        }
        ActionData<MsgId> msgId = bot.sendForwardMsg(event, forwardMsg);
        return bot.isActionDataMsgIdNotEmpty(msgId);
    }

    private boolean sendForward(BaniraBot bot, MessageEmojiLikeNoticeEvent event, List<SocialMediaContent> contents) {
        List<Map<String, Object>> forwardMsg = buildForwardMsg(bot, contents);
        if (CollectionUtils.isNullOrEmpty(forwardMsg)) {
            return false;
        }
        Long groupId = event.getGroupId();
        Long userId = event.getOperatorId();
        if (BaniraUtils.isGroupIdValid(groupId)) {
            ActionData<MsgId> msgId = bot.sendGroupForwardMsg(groupId, forwardMsg);
            return bot.isActionDataMsgIdNotEmpty(msgId);
        } else if (BaniraUtils.isUserIdValid(userId)) {
            ActionData<MsgId> msgId = bot.sendPrivateForwardMsg(userId, forwardMsg);
            return bot.isActionDataMsgIdNotEmpty(msgId);
        }
        return false;
    }

    private boolean sendDetails(BaniraBot bot, AnyMessageEvent event, List<SocialMediaContent> contents) {
        StringBuilder sb = new StringBuilder();
        for (SocialMediaContent content : contents) {
            if (StringUtils.isNullOrEmptyEx(content.msg())) continue;
            if (!sb.isEmpty()) {
                sb.append("\n\n");
            }
            sb.append(content.msg());
        }
        if (sb.isEmpty()) {
            return false;
        }
        ActionData<MsgId> msgId = bot.sendMsg(event, sb.toString(), false);
        return bot.isActionDataMsgIdNotEmpty(msgId);
    }

    private boolean sendDetails(BaniraBot bot, MessageEmojiLikeNoticeEvent event, List<SocialMediaContent> contents) {
        StringBuilder sb = new StringBuilder();
        for (SocialMediaContent content : contents) {
            if (StringUtils.isNullOrEmptyEx(content.msg())) continue;
            if (!sb.isEmpty()) {
                sb.append("\n\n");
            }
            sb.append(content.msg());
        }
        if (sb.isEmpty()) {
            return false;
        }
        return sendNoticeMsg(bot, event, sb.toString());
    }

    private boolean sendVideos(BaniraBot bot, AnyMessageEvent event, List<SocialMediaContent> contents) {
        boolean sent = false;
        for (SocialMediaContent content : contents) {
            if (StringUtils.isNullOrEmptyEx(content.video())) continue;
            ActionData<MsgId> msgId = bot.sendMsg(event, MsgUtils.builder().video(content.video(), content.cover()).build(), false);
            if (bot.isActionDataMsgIdNotEmpty(msgId)) {
                sent = true;
            }
        }
        return sent;
    }

    private boolean sendVideos(BaniraBot bot, MessageEmojiLikeNoticeEvent event, List<SocialMediaContent> contents) {
        boolean sent = false;
        for (SocialMediaContent content : contents) {
            if (StringUtils.isNullOrEmptyEx(content.video())) continue;
            if (sendNoticeMsg(bot, event, MsgUtils.builder().video(content.video(), content.cover()).build())) {
                sent = true;
            }
        }
        return sent;
    }

    private boolean sendNoticeMsg(BaniraBot bot, MessageEmojiLikeNoticeEvent event, String msg) {
        Long groupId = event.getGroupId();
        Long userId = event.getOperatorId();
        if (BaniraUtils.isGroupIdValid(groupId)) {
            ActionData<MsgId> msgId = bot.sendGroupMsg(groupId, msg, false);
            return bot.isActionDataMsgIdNotEmpty(msgId);
        } else if (BaniraUtils.isUserIdValid(userId)) {
            ActionData<MsgId> msgId = bot.sendPrivateMsg(userId, msg, false);
            return bot.isActionDataMsgIdNotEmpty(msgId);
        }
        return false;
    }

    private boolean isEmojiMatched(MessageEmojiLikeNoticeEvent event, SocialMediaGroupSettings settings) {
        if (CollectionUtils.isNullOrEmpty(event.getLikes()) || CollectionUtils.isNullOrEmpty(settings.emojiIds())) {
            return false;
        }
        List<String> allowedEmojiIds = settings.emojiIds().stream()
                .filter(Objects::nonNull)
                .map(String::valueOf)
                .toList();
        return event.getLikes().stream()
                .filter(Objects::nonNull)
                .map(MessageEmojiLikeNoticeEvent.Like::getEmojiId)
                .filter(StringUtils::isNotNullOrEmpty)
                .anyMatch(allowedEmojiIds::contains);
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

    private String getMessageContentById(BaniraBot bot, int msgId) {
        ActionData<MsgResp> msgData = bot.getMsg(msgId);
        if (!bot.isActionDataNotEmpty(msgData)) {
            return "";
        }
        String message = msgData.getData().getMessage();
        if (StringUtils.isNotNullOrEmpty(message)) {
            return message;
        }
        return Objects.toString(msgData.getData().getRawMessage(), "");
    }

    private String normalizeReplyMode(String mode) {
        if (StringUtils.isNullOrEmptyEx(mode)) {
            return REPLY_MODE_FORWARD;
        }
        String normalized = mode.trim().toLowerCase();
        if (REPLY_MODE_DETAIL.equals(normalized) || REPLY_MODE_VIDEO.equals(normalized) || REPLY_MODE_FORWARD.equals(normalized)) {
            return normalized;
        }
        return REPLY_MODE_FORWARD;
    }

    private SocialMediaGroupSettings getGroupSettings(Long groupId) {
        OtherConfig config = BaniraUtils.getOthersConfig(groupId);
        if (config == null || config.socialMediaSettings() == null) {
            return new SocialMediaGroupSettings();
        }
        return config.socialMediaSettings();
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
