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
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.basic.BaseInstructionsConfig;
import xin.vanilla.banira.config.entity.group.SocialMediaGroupConfig;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.plugin.help.HelpTopic;
import xin.vanilla.banira.plugin.help.HelpTopics;
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

    @Override
    public void registerHelpTopics(@Nonnull List<HelpTopic> topics, Long groupId) {
        List<String> socialMedia = insConfig.get().socialMedia();
        BaseInstructionsConfig base = BaniraUtils.getBaseIns();
        String prefix = BaniraUtils.getInsPrefixWithSpace();
        String cmd = socialMedia.getFirst();
        String socialCmd = prefix + cmd;

        HelpTopic topic = HelpTopics.of("社交媒体解析", "自动解析消息中的社交媒体链接。", 99, socialMedia)
                .detail("启用插件并配置触发方式后，可通过直接发送链接、回复含链接的消息，或对消息点赞指定表情来触发解析。");

        topic.child(HelpTopics.opEnable(base,
                "启用插件：\n" + socialCmd + " " + base.enable().getFirst() + "\n\n"
                        + "启用某项触发（管理员）：\n" + socialCmd + " " + base.enable().getFirst() + " <触发项>\n"
                        + "触发项：direct(直接触发)、reply(回复触发)、likeNotice(表情点赞触发)、recognizeEmoji(识别提示表情)"));
        topic.child(HelpTopics.opDisable(base,
                "禁用插件：\n" + socialCmd + " " + base.disable().getFirst() + "\n\n"
                        + "禁用某项触发（管理员）：\n" + socialCmd + " " + base.disable().getFirst() + " <触发项>"));
        topic.child(HelpTopics.opAdd(base,
                socialCmd + " " + base.add().getFirst() + " emoji <表情ID>\n\n"
                        + "添加 likeNotice 触发用的表情 ID。"));
        topic.child(HelpTopics.opDel(base,
                socialCmd + " " + base.del().getFirst() + " emoji <表情ID>\n\n"
                        + "移除已配置的触发表情 ID。"));
        topic.child(HelpTopics.opList(base,
                socialCmd + " " + base.list().getFirst() + " / " + base.status().getFirst() + "\n\n"
                        + "查看当前群的社交媒体解析配置。"));
        topic.child(HelpTopics.sub("回复方式", "设置解析结果的回复形式。", 6, List.of("mode", "回复方式"),
                socialCmd + " mode <forward|detail|video>\n\n"
                        + "forward：合并转发（默认）\n"
                        + "detail：详情文本\n"
                        + "video：视频直链"));

        topics.add(topic);
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
            SocialMediaGroupConfig groupConfig = BaniraUtils.getGroupConfigForEdit(SocialMediaGroupConfig.class, event.getGroupId());
            SocialMediaGroupSettings settings = groupConfig.socialMediaSettings();
            if (settings == null) {
                settings = new SocialMediaGroupSettings();
                groupConfig.socialMediaSettings(settings);
            }

            // 启用
            if (insConfig.get().base().enable().contains(operate)) {
                if (!bot.isAdmin(event.getGroupId(), event.getUserId())) {
                    return bot.setMsgEmojiLikeNo(event.getMessageId());
                }
                if (split.length <= 2) {
                    groupConfig.socialMedia(true);
                } else {
                    if (!applyBooleanSetting(settings, split[2], true)) {
                        return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
                    }
                }
                BaniraUtils.saveGroupConfig();
                return bot.setMsgEmojiLikeOk(event.getMessageId());
            }
            // 禁用
            else if (insConfig.get().base().disable().contains(operate)) {
                if (!bot.isAdmin(event.getGroupId(), event.getUserId())) {
                    return bot.setMsgEmojiLikeNo(event.getMessageId());
                }
                if (split.length <= 2) {
                    groupConfig.socialMedia(false);
                } else {
                    if (!applyBooleanSetting(settings, split[2], false)) {
                        return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
                    }
                }
                BaniraUtils.saveGroupConfig();
                return bot.setMsgEmojiLikeOk(event.getMessageId());
            }
            // 添加
            else if (insConfig.get().base().add().contains(operate)) {
                if (!bot.isAdmin(event.getGroupId(), event.getUserId())) {
                    return bot.setMsgEmojiLikeNo(event.getMessageId());
                }
                if (split.length < 4 || !"emoji".equalsIgnoreCase(split[2])) {
                    return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
                }
                Long emojiId = StringUtils.toLong(split[3], -1L);
                if (emojiId <= 0) {
                    return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
                }
                if (CollectionUtils.isNullOrEmpty(settings.emojiIds())) {
                    settings.emojiIds(BaniraUtils.mutableListOf());
                }
                if (!settings.emojiIds().contains(emojiId)) {
                    settings.emojiIds().add(emojiId);
                }
                BaniraUtils.saveGroupConfig();
                return bot.setMsgEmojiLikeOk(event.getMessageId());
            }
            // 删除
            else if (insConfig.get().base().del().contains(operate)) {
                if (!bot.isAdmin(event.getGroupId(), event.getUserId())) {
                    return bot.setMsgEmojiLikeNo(event.getMessageId());
                }
                if (split.length < 4 || !"emoji".equalsIgnoreCase(split[2])) {
                    return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
                }
                Long emojiId = StringUtils.toLong(split[3], -1L);
                if (emojiId <= 0 || CollectionUtils.isNullOrEmpty(settings.emojiIds())) {
                    return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
                }
                settings.emojiIds().removeIf(id -> Objects.equals(id, emojiId));
                BaniraUtils.saveGroupConfig();
                return bot.setMsgEmojiLikeOk(event.getMessageId());
            }
            // 查询
            else if (insConfig.get().base().list().contains(operate) || insConfig.get().base().status().contains(operate)) {
                ActionData<MsgId> msgId = bot.sendMsg(event, buildSettingsText(groupConfig), false);
                return bot.isActionDataMsgIdNotEmpty(msgId);
            }
            // 设置回复方式
            else if ("mode".equalsIgnoreCase(operate)) {
                if (!bot.isAdmin(event.getGroupId(), event.getUserId())) {
                    return bot.setMsgEmojiLikeNo(event.getMessageId());
                }
                if (split.length < 3) {
                    return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
                }
                String mode = normalizeReplyMode(split[2]);
                settings.replyMode(mode);
                BaniraUtils.saveGroupConfig();
                return bot.setMsgEmojiLikeOk(event.getMessageId());
            } else {
                return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            }
        }
        return false;
    }

    /**
     * 应用布尔类型的群组设置项。
     */
    private boolean applyBooleanSetting(SocialMediaGroupSettings settings, String key, boolean value) {
        String normalized = normalizeSettingKey(key);
        switch (normalized) {
            case "direct" -> settings.triggerDirect(value);
            case "reply" -> settings.triggerReplyInvoke(value);
            case "likeNotice" -> settings.triggerEmojiLikeNotice(value);
            case "recognizeEmoji" -> settings.replyEmojiOnRecognize(value);
            default -> {
                return false;
            }
        }
        return true;
    }

    /**
     * 归一化设置项别名。
     */
    private String normalizeSettingKey(String key) {
        if (StringUtils.isNullOrEmptyEx(key)) {
            return "";
        }
        String k = key.trim().toLowerCase();
        return switch (k) {
            case "direct", "直接触发", "1" -> "direct";
            case "reply", "replyinvoke", "回复触发", "2" -> "reply";
            case "likenotice", "emoji", "emojinotice", "表情触发", "3" -> "likeNotice";
            case "recognizeemoji", "promptemoji", "识别提示", "提示表情" -> "recognizeEmoji";
            default -> "";
        };
    }

    /**
     * 构建当前群社交媒体配置文本。
     */
    private String buildSettingsText(SocialMediaGroupConfig config) {
        SocialMediaGroupSettings settings = config.socialMediaSettings();
        if (settings == null) {
            settings = new SocialMediaGroupSettings();
        }
        return MsgUtils.builder()
                .text("社交媒体配置\n")
                .text("启用：" + config.socialMedia()).text("\n")
                .text("direct触发：" + settings.triggerDirect()).text("\n")
                .text("reply触发：" + settings.triggerReplyInvoke()).text("\n")
                .text("emojiNotice触发：" + settings.triggerEmojiLikeNotice()).text("\n")
                .text("识别提示表情：" + settings.replyEmojiOnRecognize()).text("\n")
                .text("emojiIds：" + settings.emojiIds()).text("\n")
                .text("回复方式：" + normalizeReplyMode(settings.replyMode())).text("\n\n")
                .text("示例：\n")
                .text("enable direct / disable direct\n")
                .text("enable reply / disable reply\n")
                .text("enable likeNotice / disable likeNotice\n")
                .text("enable recognizeEmoji / disable recognizeEmoji\n")
                .text("add emoji 10068 / del emoji 10068\n")
                .text("mode forward|detail|video")
                .build();
    }

    /**
     * 处理普通消息触发的社交媒体解析
     */
    @AnyMessageHandler
    public boolean parse(BaniraBot bot, AnyMessageEvent event) {
        if (!isEnable(event.getGroupId())) return false;

        SocialMediaGroupSettings settings = getGroupSettings(event.getGroupId());

        // region 识别阶段
        String detectableMsg = collectDetectableText(event.getMessage());
        boolean directRecognized = hasAnyParseTarget(detectableMsg);
        boolean replyRecognized = false;
        String replyMsg = "";
        if (isReplyInvokeTrigger(bot, event)) {
            replyMsg = collectDetectableText(BaniraUtils.getReplyContentString(bot, event.getMessage()));
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
        String targetMsg = directTrigger ? detectableMsg : replyMsg;
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
        if (Objects.equals(bot.getSelfId(), event.getUserId())) return;
        if (Objects.equals(bot.getSelfId(), event.getOperatorId())) return;

        SocialMediaGroupSettings settings = getGroupSettings(event.getGroupId());
        if (!settings.triggerEmojiLikeNotice()) {
            return;
        }
        if (!event.isAdd()) {
            return;
        }
        if (!isEmojiMatched(event, settings)) {
            return;
        }
        if (event.getMessageId() == null || event.getMessageId() <= 0) {
            return;
        }
        String likedMsg = collectDetectableText(getMessageContentById(bot, event.getMessageId()));
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
     * 汇总用于识别/解析的文本：合并转发内层、去掉图片/视频 CQ 中的 url 以降低误匹配
     */
    private String collectDetectableText(String msg) {
        if (StringUtils.isNullOrEmptyEx(msg)) {
            return "";
        }
        StringBuilder sb = new StringBuilder(stripMediaCqUrls(msg));
        if (BaniraUtils.hasForward(msg)) {
            for (List<MsgResp> block : BaniraUtils.getForwardContent(msg)) {
                if (CollectionUtils.isNullOrEmpty(block)) {
                    continue;
                }
                for (MsgResp resp : block) {
                    if (resp == null) {
                        continue;
                    }
                    String inner = resp.getMessage();
                    if (StringUtils.isNullOrEmptyEx(inner)) {
                        inner = Objects.toString(resp.getRawMessage(), "");
                    }
                    if (StringUtils.isNotNullOrEmpty(inner)) {
                        sb.append('\n').append(collectDetectableText(inner));
                    }
                }
            }
        }
        return sb.toString();
    }

    /**
     * 识别阶段不扫描图片/视频 CQ 的 url 字段，避免 QQ 图床链接误命中 BV/av 等规则
     */
    private String stripMediaCqUrls(String msg) {
        if (StringUtils.isNullOrEmptyEx(msg)) {
            return "";
        }
        return msg.replaceAll("(?i)(\\[CQ:(?:image|video)[^\\]]*?),url=[^,\\]]+", "$1");
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
        Long emojiId = settings.recognizeEmojiId();
        if (emojiId != null && emojiId > 0) {
            bot.setMsgEmojiLike(messageId, emojiId);
        }
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
        SocialMediaGroupConfig config = BaniraUtils.getGroupConfigOrGlobal(SocialMediaGroupConfig.class, groupId);
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
        if (BaniraUtils.hasGroupConfig(SocialMediaGroupConfig.class, groupId)) {
            SocialMediaGroupConfig config = BaniraUtils.getGroupConfigOrGlobal(SocialMediaGroupConfig.class, groupId);
            if (config != null) return config.socialMedia();
        }
        // 全局配置
        return BaniraUtils.getGroupConfigOrGlobal(SocialMediaGroupConfig.class, 0L).socialMedia();
    }
}
