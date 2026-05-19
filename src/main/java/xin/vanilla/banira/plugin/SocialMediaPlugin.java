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
    private static final int PROCESSED_MSG_CACHE_MAX = 5000;

    @Autowired(required = false)
    private List<SocialMediaParser> parsers = new ArrayList<>();
    @Autowired
    private SocialMediaApiService socialMediaApiService;
    private final LinkedHashSet<Integer> processedMsgIds = new LinkedHashSet<>();
    private final Object processedLock = new Object();

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

    /**
     * 处理社交媒体插件的启用/禁用指令
     */
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

    /**
     * 处理普通消息触发的社交媒体解析
     */
    @AnyMessageHandler
    public boolean parse(BaniraBot bot, AnyMessageEvent event) {
        if (!isEnable(event.getGroupId())) return false;

        SocialMediaGroupSettings settings = getGroupSettings(event.getGroupId());

        // region 识别阶段
        boolean directRecognized = hasAnyParseTarget(event.getMessage());
        boolean replyRecognized = false;
        String replyMsg = "";
        if (isReplyInvokeTrigger(bot, event)) {
            replyMsg = BaniraUtils.getReplyContentString(bot, event.getMessage());
            replyRecognized = hasAnyParseTarget(replyMsg);
        }
        Integer recognizedMessageId = resolveRecognizedMessageId(event, directRecognized);
        if ((directRecognized || replyRecognized) && recognizedMessageId != null && recognizedMessageId > 0) {
            maybeReplyRecognizeEmoji(bot, recognizedMessageId, settings);
        }
        // endregion 识别阶段

        // region 正式解析阶段
        boolean directTrigger = settings.triggerDirect() && directRecognized;
        boolean replyTrigger = settings.triggerReplyInvoke() && replyRecognized;
        if (!directTrigger && !replyTrigger) {
            return false;
        }
        Integer targetMessageId = directTrigger
                ? event.getMessageId()
                : Optional.ofNullable(BaniraUtils.getReplyId(event.getMessage())).map(Long::intValue).orElse(event.getMessageId());
        String targetMsg = directTrigger ? event.getMessage() : replyMsg;
        if (targetMessageId == null || targetMessageId <= 0) {
            return false;
        }
        if (!tryMarkProcessing(targetMessageId)) {
            return false;
        }
        bot.setMsgEmojiLike(targetMessageId, 162);

        List<SocialMediaContent> contents = parseContents(targetMsg);
        contents = distinctContents(contents);
        if (CollectionUtils.isNotNullOrEmpty(contents)) {
            boolean success = sendByReplyMode(bot, event, contents, settings.replyMode());
            return success
                    ? bot.setMsgEmojiLikeHeart(targetMessageId)
                    : bot.setMsgEmojiLikeBrokenHeart(targetMessageId);
        }
        return bot.setMsgEmojiLikeBrokenHeart(targetMessageId);
        // endregion 正式解析阶段
    }

    /**
     * 处理消息表情通知触发的社交媒体解析
     */
    @MessageEmojiLikeNoticeHandler
    public void parse(BaniraBot bot, MessageEmojiLikeNoticeEvent event) {
        if (!isEnable(event.getGroupId())) return;
        if (event.getSelfId().equals(event.getOperatorId())) return;

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
        if (!tryMarkProcessing(event.getMessageId())) {
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

    /**
     * 调用各解析器解析消息中的媒体内容
     */
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

    /**
     * 判断消息中是否包含可解析的媒体目标
     */
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

    /**
     * 对解析结果按关键字段去重
     */
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

    /**
     * 判断当前消息是否满足“回复触发”条件
     */
    private boolean isReplyInvokeTrigger(BaniraBot bot, AnyMessageEvent event) {
        if (!BaniraUtils.hasReply(event.getMessage())) {
            return false;
        }
        boolean isMentioned = bot.isMentioned(event.getMessage());
        boolean isParseCommand = isParseInvokeCommand(bot, event);
        return isMentioned || isParseCommand;
    }

    /**
     * 判断当前消息是否为社交媒体解析指令调用
     */
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

    /**
     * 按配置的回复模式发送解析结果（普通消息事件）
     */
    private boolean sendByReplyMode(BaniraBot bot, AnyMessageEvent event, List<SocialMediaContent> contents, String mode) {
        String normalizedMode = normalizeReplyMode(mode);
        return switch (normalizedMode) {
            case REPLY_MODE_DETAIL -> sendDetails(bot, event, contents);
            case REPLY_MODE_VIDEO -> sendVideos(bot, event, contents);
            default -> sendForward(bot, event, contents);
        };
    }

    /**
     * 按配置的回复模式发送解析结果（表情点赞通知事件）
     */
    private boolean sendByReplyMode(BaniraBot bot, MessageEmojiLikeNoticeEvent event, List<SocialMediaContent> contents, String mode) {
        String normalizedMode = normalizeReplyMode(mode);
        return switch (normalizedMode) {
            case REPLY_MODE_DETAIL -> sendDetails(bot, event, contents);
            case REPLY_MODE_VIDEO -> sendVideos(bot, event, contents);
            default -> sendForward(bot, event, contents);
        };
    }

    /**
     * 以合并转发方式发送解析结果（普通消息事件）
     */
    private boolean sendForward(BaniraBot bot, AnyMessageEvent event, List<SocialMediaContent> contents) {
        List<Map<String, Object>> forwardMsg = buildForwardMsg(bot, contents);
        if (CollectionUtils.isNullOrEmpty(forwardMsg)) {
            return false;
        }
        ActionData<MsgId> msgId = bot.sendForwardMsg(event, forwardMsg);
        return bot.isActionDataMsgIdNotEmpty(msgId);
    }

    /**
     * 以合并转发方式发送解析结果（表情点赞通知事件）
     */
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

    /**
     * 仅发送详情文本（普通消息事件）
     */
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

    /**
     * 仅发送详情文本（表情点赞通知事件）
     */
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

    /**
     * 仅发送视频（普通消息事件）
     */
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

    /**
     * 仅发送视频（表情点赞通知事件）
     */
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

    /**
     * 根据通知来源发送群聊或私聊消息
     */
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

    /**
     * 判断点赞通知中的 emoji 是否命中允许触发列表
     */
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

    /**
     * 标记消息正在/已经处理，避免重复解析
     */
    private boolean tryMarkProcessing(Integer messageId) {
        if (messageId == null || messageId <= 0) {
            return false;
        }
        synchronized (processedLock) {
            if (processedMsgIds.contains(messageId)) {
                return false;
            }
            processedMsgIds.add(messageId);
            if (processedMsgIds.size() > PROCESSED_MSG_CACHE_MAX) {
                Iterator<Integer> iterator = processedMsgIds.iterator();
                if (iterator.hasNext()) {
                    iterator.next();
                    iterator.remove();
                }
            }
            return true;
        }
    }

    /**
     * 在识别阶段按配置回复可点击表情
     */
    private void maybeReplyRecognizeEmoji(BaniraBot bot, Integer messageId, SocialMediaGroupSettings settings) {
        if (!shouldReplyRecognizeEmoji(settings)) {
            return;
        }
        if (messageId == null || messageId <= 0 || CollectionUtils.isNullOrEmpty(settings.emojiIds())) {
            return;
        }
        settings.emojiIds().stream().filter(Objects::nonNull).findFirst().ifPresent(emojiId -> bot.setMsgEmojiLike(messageId, emojiId));
    }

    /**
     * 判断是否允许回复识别提示表情
     */
    private boolean shouldReplyRecognizeEmoji(SocialMediaGroupSettings settings) {
        return settings.triggerEmojiLikeNotice() && settings.replyEmojiOnRecognize();
    }

    /**
     * 根据识别结果确定应该反馈表情的目标消息ID
     */
    private Integer resolveRecognizedMessageId(AnyMessageEvent event, boolean directRecognized) {
        if (directRecognized) {
            return event.getMessageId();
        }
        return Optional.ofNullable(BaniraUtils.getReplyId(event.getMessage()))
                .map(Long::intValue)
                .orElse(event.getMessageId());
    }

    /**
     * 构建合并转发消息节点（详情与视频分条）
     */
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

    /**
     * 按消息ID读取原始消息内容
     */
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

    /**
     * 归一化回复模式，非法值回退为 forward
     */
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

    /**
     * 获取群组级社交媒体配置
     */
    private SocialMediaGroupSettings getGroupSettings(Long groupId) {
        OtherConfig config = BaniraUtils.getOthersConfig(groupId);
        if (config == null || config.socialMediaSettings() == null) {
            return new SocialMediaGroupSettings();
        }
        return config.socialMediaSettings();
    }

    /**
     * 判断当前群是否启用了社交媒体解析
     */
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
