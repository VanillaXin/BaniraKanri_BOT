package xin.vanilla.banira.plugin;

import com.google.gson.JsonObject;
import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.GroupMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.constant.ActionParams;
import com.mikuac.shiro.dto.action.common.ActionData;
import com.mikuac.shiro.dto.action.common.MsgId;
import com.mikuac.shiro.dto.action.response.LoginInfoResp;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import com.mikuac.shiro.model.ArrayMsg;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.basic.BaseInstructionsConfig;
import xin.vanilla.banira.config.entity.extended.McModCommentConfig;
import xin.vanilla.banira.config.entity.extended.ModWatchInfo;
import xin.vanilla.banira.config.entity.group.McModGroupConfig;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.plugin.chat.agent.AgentContext;
import xin.vanilla.banira.plugin.chat.capability.*;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.plugin.help.HelpTopic;
import xin.vanilla.banira.plugin.help.HelpTopics;
import xin.vanilla.banira.plugin.mcmod.*;
import xin.vanilla.banira.service.IMessageRecordManager;
import xin.vanilla.banira.util.*;
import xin.vanilla.banira.util.mcmod.*;

import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.regex.Matcher;

/**
 * MCMod百科插件
 */
@Slf4j
@Shiro
@Component
public class McModPlugin extends BasePlugin implements AiCapabilityProvider {

    @Resource
    private IMessageRecordManager messageRecordManager;

    @Resource
    private McModCommentScheduler mcModCommentScheduler;
    @Resource
    private McModCommentService mcModCommentService;

    @Resource
    private McModSearchListStore mcModSearchListStore;

    private static final long SEARCH_LIST_SESSION_TTL_MS = TimeUnit.MINUTES.toMillis(10);

    private static final Set<String> MOD_INS = BaniraUtils.mutableSetOf("mod", "模组");
    private static final Set<String> MOD_PACK_INS = BaniraUtils.mutableSetOf("modpack", "pack", "整合包");
    private static final Set<String> AUTHOR_INS = BaniraUtils.mutableSetOf("author", "作者");
    private static final Set<String> USER_INS = BaniraUtils.mutableSetOf("user", "center", "用户");
    private static final Set<String> ITEM_INS = BaniraUtils.mutableSetOf("item", "data", "资料", "物品");
    private static final Set<String> TUTORIAL_INS = BaniraUtils.mutableSetOf("tutorial", "教程");
    private static final Set<String> MOD_RANDOM_INS = BaniraUtils.mutableSetOf("random", "随便看看");
    private static final Set<String> CATEGORY_INS = BaniraUtils.mutableSetOf("category", "分类");
    private static final Set<String> USER_CARD_INS = BaniraUtils.mutableSetOf("card", "用户卡片");
    private static final Set<String> COMMENT_INS = BaniraUtils.mutableSetOf("comment", "发布评论", "评论");
    private static final Set<String> DEL_COMMENT_INS = BaniraUtils.mutableSetOf("delcomment", "删除评论");
    private static final Set<String> COMMENT_REPLY_INS = BaniraUtils.mutableSetOf("reply", "回复");
    private static final Set<String> PUSH_INS = BaniraUtils.mutableSetOf("push", "recommend", "推荐");
    private static final Set<String> VOTE_INS = BaniraUtils.mutableSetOf("vote", "投票");
    private static final Set<String> RED_VOTE_INS = BaniraUtils.mutableSetOf("red", "红票", "红");
    private static final Set<String> BLACK_VOTE_INS = BaniraUtils.mutableSetOf("black", "黑票", "黑");


    @Override
    public void registerHelpTopics(@Nonnull List<HelpTopic> topics, @Nullable Long groupId) {
        List<String> command = insConfig.get().mcMod();
        BaseInstructionsConfig base = BaniraUtils.getBaseIns();
        String prefix = BaniraUtils.getInsPrefixWithSpace();
        Set<String> commentTypes = new HashSet<>();
        commentTypes.addAll(MOD_INS);
        commentTypes.addAll(MOD_PACK_INS);
        commentTypes.addAll(AUTHOR_INS);
        commentTypes.addAll(USER_INS);

        String slashCmd = "/" + command.getFirst();
        String baniraCmd = prefix + command.getFirst();

        HelpTopic topic = HelpTopics.of("MC百科", "MCMod 百科检索、互动与评论检测。", 99, command);
        HelpTopic search = HelpTopics.of("检索", "搜索模组、整合包、作者、用户、资料或教程。", 1, List.of("search", "检索"));
        search.child(HelpTopics.sub("模组", "按关键词搜索模组。", 1, MOD_INS,
                buildSearchDetail(slashCmd, MOD_INS, true)));
        search.child(HelpTopics.sub("整合包", "按关键词搜索整合包。", 2, MOD_PACK_INS,
                buildSearchDetail(slashCmd, MOD_PACK_INS, true)));
        search.child(HelpTopics.sub("作者", "按关键词搜索作者。", 3, AUTHOR_INS,
                buildSearchDetail(slashCmd, AUTHOR_INS, true)));
        search.child(HelpTopics.sub("用户", "按关键词搜索用户。", 4, USER_INS,
                buildSearchDetail(slashCmd, USER_INS, false)));
        search.child(HelpTopics.sub("资料", "按关键词搜索资料。", 5, ITEM_INS,
                buildSearchDetail(slashCmd, ITEM_INS, false)));
        search.child(HelpTopics.sub("教程", "按关键词搜索教程。", 6, TUTORIAL_INS,
                buildSearchDetail(slashCmd, TUTORIAL_INS, false)));
        topic.child(search);
        topic.child(HelpTopics.sub("随便看看", "随机显示 MOD。", 2, MOD_RANDOM_INS,
                slashCmd + " " + HelpTopics.formatAliasChoices(MOD_RANDOM_INS)));
        topic.child(HelpTopics.sub("分类浏览", "按分类浏览首页模组。", 3, CATEGORY_INS,
                slashCmd + " " + HelpTopics.formatAliasChoices(CATEGORY_INS) + " <分类名>\n"
                        + "例如：" + slashCmd + " category 科技"));
        topic.child(HelpTopics.sub("用户卡片", "展示用户卡片信息。", 4, USER_CARD_INS,
                slashCmd + " " + HelpTopics.formatAliasChoices(USER_CARD_INS) + " <用户ID>"));
        HelpTopic interact = HelpTopics.of("互动", "推荐与投票。", 5, List.of("push", "vote", "互动", "推荐", "投票"));
        interact.child(HelpTopics.sub("推荐", "点推荐。", 1, PUSH_INS, buildPushHelp(slashCmd)));
        interact.child(HelpTopics.sub("投票", "投红票或黑票。", 2, VOTE_INS, buildVoteHelp(slashCmd)));
        topic.child(interact);
        topic.child(HelpTopics.sub("评论管理", "管理员发布、回复或删除 MC 百科评论（仅群聊）。", 6, List.of("delcomment", "reply", "comment", "评论管理"),
                "发布评论：\n" + slashCmd + " " + HelpTopics.formatAliasChoices(COMMENT_INS) + " <类型> <容器ID> <内容>\n\n"
                        + "回复评论（需回复含评论信息的消息）：\n" + slashCmd + " " + HelpTopics.formatAliasChoices(COMMENT_REPLY_INS) + " <回复内容>\n\n"
                        + "删除评论：\n" + slashCmd + " " + HelpTopics.formatAliasChoices(DEL_COMMENT_INS) + " <评论ID>\n\n"
                        + "需要群管理员权限。"));
        HelpTopic commentWatch = HelpTopics.of("评论检测", "检测 MC 百科评论变化并提示。", 7, List.of("commentWatch", "评论检测"));
        commentWatch.child(HelpTopics.opAdd(base,
                baniraCmd + " " + base.add().getFirst() + " <类型> <容器ID>\n"
                        + "类型：" + HelpTopics.joinAliases(commentTypes)));
        commentWatch.child(HelpTopics.opDel(base,
                baniraCmd + " " + base.del().getFirst() + " <类型> <容器ID>\n"
                        + "类型：" + HelpTopics.joinAliases(commentTypes)));
        commentWatch.child(HelpTopics.opList(base, baniraCmd + " " + base.list().getFirst()));
        commentWatch.child(HelpTopics.opEnable(base, baniraCmd + " " + base.enable().getFirst()));
        commentWatch.child(HelpTopics.opDisable(base, baniraCmd + " " + base.disable().getFirst()));
        topic.child(commentWatch);
        topics.add(topic);
    }

    @Override
    public void registerAiCapabilities(@Nonnull List<AiCapability> capabilities, Long groupId) {
        capabilities.add(new AiCapability()
                .name("search_mcmod")
                .description("搜索 MCMod 百科内容。")
                .parameterHint("type=mod|modpack|author|user|item|tutorial|random, keyword=关键词")
                .parameters(List.of(
                        AiCapabilityParameter.required("type", "检索类型：mod、modpack、author、user、item、tutorial 或 random"),
                        AiCapabilityParameter.optional("keyword", "关键词；random 类型可为空")
                ))
                .resultMode(AiCapabilityResultMode.DIRECT_SEND)
                .allowQuotedContext(true)
                .executor((ctx, args) -> {
                    try {
                        String type = AiCapabilityArgs.require(args, "type");
                        String keyword = args.getOrDefault("keyword", "");
                        return executeAiSearch(ctx, type, keyword);
                    } catch (IllegalArgumentException e) {
                        return e.getMessage();
                    }
                }));
        capabilities.add(new AiCapability()
                .name("vote_mcmod")
                .description("给 MCMod 模组或整合包点红票/黑票。只在用户当前消息明确要求投票时使用。")
                .parameterHint("type=mod|modpack, keywordOrId=模组/整合包名称或ID, vote=red|black")
                .parameters(List.of(
                        AiCapabilityParameter.required("type", "目标类型：mod 或 modpack"),
                        AiCapabilityParameter.required("keywordOrId", "模组/整合包名称或 ID，必须来自当前消息"),
                        AiCapabilityParameter.required("vote", "red 或 black")
                ))
                .resultMode(AiCapabilityResultMode.DIRECT_SEND)
                .mutationPolicy(AiMutationPolicy.CURRENT_MESSAGE_INTENT)
                .mutating(true)
                .executor((ctx, args) -> {
                    try {
                        String type = AiCapabilityArgs.require(args, "type");
                        String keywordOrId = AiCapabilityArgs.require(args, "keywordOrId");
                        String vote = AiCapabilityArgs.require(args, "vote");
                        return executeAiVote(ctx, type, keywordOrId, vote);
                    } catch (IllegalArgumentException e) {
                        return e.getMessage();
                    }
                }));
        capabilities.add(new AiCapability()
                .name("push_mcmod")
                .description("给 MCMod 模组或整合包点推荐。只在用户当前消息明确要求点推荐时使用。")
                .parameterHint("type=mod|modpack, keywordOrId=模组/整合包名称或ID")
                .parameters(List.of(
                        AiCapabilityParameter.required("type", "目标类型：mod 或 modpack"),
                        AiCapabilityParameter.required("keywordOrId", "模组/整合包名称或 ID，必须来自当前消息")
                ))
                .resultMode(AiCapabilityResultMode.DIRECT_SEND)
                .mutationPolicy(AiMutationPolicy.CURRENT_MESSAGE_INTENT)
                .mutating(true)
                .executor((ctx, args) -> {
                    try {
                        String type = AiCapabilityArgs.require(args, "type");
                        String keywordOrId = AiCapabilityArgs.require(args, "keywordOrId");
                        return executeAiPush(ctx, type, keywordOrId);
                    } catch (IllegalArgumentException e) {
                        return e.getMessage();
                    }
                }));
        capabilities.add(new AiCapability()
                .name("comment_mcmod")
                .description("在 MCMod 模组、整合包、作者或用户中心下方发布评论/留言。只在用户当前消息明确给出评论内容时使用。")
                .parameterHint("type=mod|modpack|author|user, keywordOrId=目标名称或ID, content=评论内容")
                .parameters(List.of(
                        AiCapabilityParameter.required("type", "目标类型：mod、modpack、author 或 user"),
                        AiCapabilityParameter.required("keywordOrId", "目标名称或 ID，必须来自当前消息"),
                        AiCapabilityParameter.required("content", "评论内容，必须来自当前消息")
                ))
                .resultMode(AiCapabilityResultMode.DIRECT_SEND)
                .mutationPolicy(AiMutationPolicy.CURRENT_MESSAGE_INTENT)
                .access(AiCapabilityAccess.ADMIN)
                .mutating(true)
                .executor((ctx, args) -> {
                    try {
                        String type = AiCapabilityArgs.require(args, "type");
                        String keywordOrId = AiCapabilityArgs.require(args, "keywordOrId");
                        String content = AiCapabilityArgs.require(args, "content");
                        return executeAiComment(ctx, type, keywordOrId, content);
                    } catch (IllegalArgumentException e) {
                        return e.getMessage();
                    }
                }));
        capabilities.add(new AiCapability()
                .name("reply_mcmod_comment")
                .description("回复 MCMod 评论。只在用户当前消息明确要求回复某条评论并给出内容时使用。")
                .parameterHint("type=mod|modpack|author|user, containerId=容器ID, commentId=评论ID, content=回复内容")
                .parameters(List.of(
                        AiCapabilityParameter.required("type", "评论所在目标类型"),
                        AiCapabilityParameter.required("containerId", "容器 ID"),
                        AiCapabilityParameter.required("commentId", "评论 ID"),
                        AiCapabilityParameter.required("content", "回复内容，必须来自当前消息")
                ))
                .resultMode(AiCapabilityResultMode.DIRECT_SEND)
                .mutationPolicy(AiMutationPolicy.CURRENT_MESSAGE_INTENT)
                .access(AiCapabilityAccess.ADMIN)
                .mutating(true)
                .executor((ctx, args) -> {
                    try {
                        String type = AiCapabilityArgs.require(args, "type");
                        String containerId = AiCapabilityArgs.require(args, "containerId");
                        String commentId = AiCapabilityArgs.require(args, "commentId");
                        String content = AiCapabilityArgs.require(args, "content");
                        return executeAiReplyComment(ctx, type, containerId, commentId, content);
                    } catch (IllegalArgumentException e) {
                        return e.getMessage();
                    }
                }));
        capabilities.add(new AiCapability()
                .name("delete_mcmod_comment")
                .description("删除 MCMod 评论。只在用户当前消息明确要求删除指定评论 ID 时使用。")
                .parameterHint("commentId=评论ID")
                .parameters(List.of(
                        AiCapabilityParameter.required("commentId", "要删除的评论 ID，必须来自当前消息")
                ))
                .resultMode(AiCapabilityResultMode.DIRECT_SEND)
                .mutationPolicy(AiMutationPolicy.CURRENT_MESSAGE_INTENT)
                .access(AiCapabilityAccess.ADMIN)
                .mutating(true)
                .executor((ctx, args) -> {
                    try {
                        String commentId = AiCapabilityArgs.require(args, "commentId");
                        return executeAiDeleteComment(ctx, commentId);
                    } catch (IllegalArgumentException e) {
                        return e.getMessage();
                    }
                }));
    }

    @Nonnull
    private String executeAiSearch(@Nonnull AgentContext ctx, @Nonnull String type, @Nullable String keyword) {
        String normalized = normalizeAiSearchType(type);
        if (StringUtils.isNullOrEmptyEx(normalized)) {
            return "不支持的 MC百科检索类型：" + type + "。";
        }
        if (!"random".equals(normalized) && StringUtils.isNullOrEmptyEx(keyword)) {
            return "MC百科检索关键词不能为空。";
        }

        AnyMessageEvent event = buildAiMessageEvent(ctx, "/mcmod " + normalized + " " + (keyword != null ? keyword : ""));
        Long groupId = ctx.msgType() == xin.vanilla.banira.enums.EnumMessageType.GROUP ? ctx.groupId() : 0L;
        int msgId = StringUtils.toInt(ctx.msgId());
        boolean handled = switch (normalized) {
            case "mod" -> {
                if (isNumericId(keyword)) {
                    McModContent direct = McModUtils.getModName(keyword);
                    if (direct != null) {
                        yield handleContentResults(ctx.bot(), event, List.of(direct), "模组", keyword, groupId, msgId);
                    }
                }
                yield handleContentResults(ctx.bot(), event, searchContentWithKeywordFallback(keyword, McModUtils::searchMod), "模组", keyword, groupId, msgId);
            }
            case "modpack" -> {
                if (isNumericId(keyword)) {
                    McModContent direct = McModUtils.getModpackName(keyword);
                    if (direct != null) {
                        yield handleContentResults(ctx.bot(), event, List.of(direct), "整合包", keyword, groupId, msgId);
                    }
                }
                yield handleContentResults(ctx.bot(), event, searchContentWithKeywordFallback(keyword, McModUtils::searchModpack), "整合包", keyword, groupId, msgId);
            }
            case "author" -> {
                if (isNumericId(keyword)) {
                    McModContent direct = McModUtils.getAuthorName(keyword);
                    if (direct != null) {
                        yield handleContentResults(ctx.bot(), event, List.of(direct), "作者", keyword, groupId, msgId);
                    }
                }
                yield handleContentResults(ctx.bot(), event, searchContentWithKeywordFallback(keyword, McModUtils::searchAuthor), "作者", keyword, groupId, msgId);
            }
            case "user" -> {
                if (isNumericId(keyword)) {
                    McModSearchResult direct = buildUserSearchResult(keyword);
                    if (direct != null) {
                        yield handleSearchResults(ctx.bot(), event, List.of(direct), "用户", keyword, groupId, msgId);
                    }
                }
                yield handleSearchResults(ctx.bot(), event, McModUtils.searchUserBySearchPage(keyword), "用户", keyword, groupId, msgId);
            }
            case "item" ->
                    handleSearchResults(ctx.bot(), event, McModUtils.searchDataBySearchPage(keyword), "资料", keyword, groupId, msgId);
            case "tutorial" ->
                    handleSearchResults(ctx.bot(), event, McModUtils.searchTutorialBySearchPage(keyword), "教程", keyword, groupId, msgId);
            case "random" -> {
                List<McModContent> randomMods = McModUtils.getRandomMods();
                if (CollectionUtils.isNullOrEmpty(randomMods)) {
                    randomMods = loadRandomModsFromCategory();
                }
                yield handleContentResults(ctx.bot(), event, randomMods, "模组", "", groupId, msgId);
            }
            default -> false;
        };
        return handled ? AiDirectResult.sent("已发送 MC百科查询结果。") : "MC百科查询失败。";
    }

    @Nonnull
    private String executeAiVote(@Nonnull AgentContext ctx, @Nonnull String type,
                                 @Nullable String keywordOrId, @Nonnull String vote) {
        InteractionTarget interactionTarget = parseInteractionTarget(type.trim().toLowerCase(Locale.ROOT));
        if (interactionTarget == null) {
            return "MC百科投票只支持模组或整合包。";
        }
        EnumCardVoteType voteType = parseVoteType(vote.trim().toLowerCase(Locale.ROOT));
        if (voteType == null) {
            return "投票类型必须是 red/black、红票或黑票。";
        }
        if (StringUtils.isNullOrEmptyEx(keywordOrId)) {
            return "缺少要投票的 MC百科目标。";
        }

        AnyMessageEvent event = buildAiMessageEvent(ctx, "/mcmod vote " + type + " " + keywordOrId + " " + vote);
        Long groupId = ctx.msgType() == xin.vanilla.banira.enums.EnumMessageType.GROUP ? ctx.groupId() : 0L;
        int msgId = StringUtils.toInt(ctx.msgId());
        EnumContentType contentType = interactionTarget == InteractionTarget.MOD ? EnumContentType.MOD : EnumContentType.MODPACK;
        ResolvedTarget resolved = resolveAiContentTarget(ctx, event, groupId, msgId, contentType, keywordOrId);
        if (!resolved.resolved()) {
            return StringUtils.isNotNullOrEmpty(resolved.result()) ? resolved.result() : "未找到要投票的 MC百科目标。";
        }

        McModCardVoteEnsureResult result = interactionTarget == InteractionTarget.MOD
                ? McModUtils.ensureModVote(groupId, resolved.id(), voteType)
                : McModUtils.ensureModpackVote(groupId, resolved.id(), voteType);
        String voteName = voteType == EnumCardVoteType.RED ? "红票" : "黑票";
        String message;
        boolean success = false;
        if (result != null && result.isCooldownBlocked()) {
            message = "投票失败：" + EnumStateCode.C109.message();
        } else if (result != null && result.isLoginRequired()) {
            message = "投票失败：请先登录 MC百科账号。";
        } else if (result != null && result.isTooFrequent()) {
            message = "投票失败：" + EnumStateCode.C109.message();
        } else if (result != null && result.isFromCache()) {
            message = "之前已经给「" + resolved.displayName() + "」点过" + voteName + "了。";
            success = true;
        } else if (result != null && result.isSuccess()) {
            message = result.isUnchanged()
                    ? "之前已经给「" + resolved.displayName() + "」点过" + voteName + "了。"
                    : "已给「" + resolved.displayName() + "」点" + voteName + "。";
            success = true;
        } else {
            Integer state = result != null ? result.getErrorState() : null;
            message = "投票失败：" + EnumStateCode.messageOf(state);
        }
        sendMessage(ctx.bot(), event, groupId, message);
        if (success) {
            ctx.bot().setMsgEmojiLikeHeart(msgId);
        } else {
            ctx.bot().setMsgEmojiLikeBrokenHeart(msgId);
        }
        return AiDirectResult.sent(message);
    }

    @Nonnull
    private String executeAiPush(@Nonnull AgentContext ctx, @Nonnull String type, @Nullable String keywordOrId) {
        InteractionTarget interactionTarget = parseInteractionTarget(type.trim().toLowerCase(Locale.ROOT));
        if (interactionTarget == null) {
            return "MC百科推荐只支持模组或整合包。";
        }
        if (StringUtils.isNullOrEmptyEx(keywordOrId)) {
            return "缺少要推荐的 MC百科目标。";
        }

        AnyMessageEvent event = buildAiMessageEvent(ctx, "/mcmod push " + type + " " + keywordOrId);
        Long groupId = ctx.msgType() == xin.vanilla.banira.enums.EnumMessageType.GROUP ? ctx.groupId() : 0L;
        int msgId = StringUtils.toInt(ctx.msgId());
        EnumContentType contentType = interactionTarget == InteractionTarget.MOD ? EnumContentType.MOD : EnumContentType.MODPACK;
        ResolvedTarget resolved = resolveAiContentTarget(ctx, event, groupId, msgId, contentType, keywordOrId);
        if (!resolved.resolved()) {
            return StringUtils.isNotNullOrEmpty(resolved.result()) ? resolved.result() : "未找到要推荐的 MC百科目标。";
        }

        McModPushResponse response = interactionTarget == InteractionTarget.MOD
                ? McModUtils.pushMod(groupId, resolved.id())
                : McModUtils.pushModpack(groupId, resolved.id());
        String message;
        boolean success = false;
        if (response != null && response.isSuccess()) {
            message = "已给「" + resolved.displayName() + "」点推荐。";
            success = true;
        } else {
            Integer state = response != null ? response.getState() : null;
            message = "推荐失败：" + EnumStateCode.messageOf(state);
        }
        sendMessage(ctx.bot(), event, groupId, message);
        if (success) {
            ctx.bot().setMsgEmojiLikeHeart(msgId);
        } else {
            ctx.bot().setMsgEmojiLikeBrokenHeart(msgId);
        }
        return AiDirectResult.sent(message);
    }

    @Nonnull
    private String executeAiComment(@Nonnull AgentContext ctx, @Nonnull String type,
                                    @Nullable String keywordOrId, @Nullable String content) {
        EnumContentType contentType = normalizeAiCommentType(type);
        if (contentType == null) {
            return "MC百科留言只支持模组、整合包、作者或用户中心。";
        }
        if (StringUtils.isNullOrEmptyEx(keywordOrId)) {
            return "缺少要留言的 MC百科目标。";
        }
        if (StringUtils.isNullOrEmptyEx(content)) {
            return "缺少要发布的留言内容。";
        }

        AnyMessageEvent event = buildAiMessageEvent(ctx, "/mcmod comment " + type + " " + keywordOrId + " " + content);
        Long groupId = ctx.msgType() == xin.vanilla.banira.enums.EnumMessageType.GROUP ? ctx.groupId() : 0L;
        int msgId = StringUtils.toInt(ctx.msgId());
        if (McModUtils.resolveOptionalCookie(groupId) == null) {
            String message = "发布评论失败：请先登录 MC百科账号。";
            sendMessage(ctx.bot(), event, groupId, message);
            ctx.bot().setMsgEmojiLikeBrokenHeart(msgId);
            return AiDirectResult.sent(message);
        }

        ResolvedTarget resolved = resolveAiContentTarget(ctx, event, groupId, msgId, contentType, keywordOrId);
        if (!resolved.resolved()) {
            return StringUtils.isNotNullOrEmpty(resolved.result()) ? resolved.result() : "未找到要留言的 MC百科目标。";
        }

        String plainText = org.jsoup.Jsoup.parse(content).text();
        String htmlText = "<p>" + plainText + "</p>";
        McModCommentResponse response = McModUtils.comment(groupId, contentType, resolved.id(), htmlText);
        String message;
        boolean success = false;
        if (response != null && response.isSuccess()) {
            message = "已在「" + resolved.displayName() + "」下方留言：" + plainText;
            success = true;
        } else {
            Integer state = response != null ? response.getState() : null;
            message = "发布评论失败：" + EnumStateCode.messageOf(state);
        }
        sendMessage(ctx.bot(), event, groupId, message);
        if (success) {
            ctx.bot().setMsgEmojiLikeHeart(msgId);
        } else {
            ctx.bot().setMsgEmojiLikeBrokenHeart(msgId);
        }
        return AiDirectResult.sent(message);
    }

    @Nonnull
    private String executeAiReplyComment(@Nonnull AgentContext ctx, @Nonnull String type,
                                         @Nullable String containerId, @Nullable String commentId,
                                         @Nullable String content) {
        EnumContentType contentType = normalizeAiCommentType(type);
        if (contentType == null) {
            return "MC百科评论回复只支持模组、整合包、作者或用户中心。";
        }
        if (!isValidTargetId(containerId)) {
            return "缺少有效的 MC百科容器 ID。";
        }
        if (!isValidTargetId(commentId)) {
            return "缺少有效的评论 ID。";
        }
        if (StringUtils.isNullOrEmptyEx(content)) {
            return "缺少要回复的内容。";
        }

        AnyMessageEvent event = buildAiMessageEvent(ctx, "/mcmod reply " + content);
        Long groupId = ctx.msgType() == xin.vanilla.banira.enums.EnumMessageType.GROUP ? ctx.groupId() : 0L;
        int msgId = StringUtils.toInt(ctx.msgId());
        if (McModUtils.resolveOptionalCookie(groupId) == null) {
            String message = "回复评论失败：请先登录 MC百科账号。";
            sendMessage(ctx.bot(), event, groupId, message);
            ctx.bot().setMsgEmojiLikeBrokenHeart(msgId);
            return AiDirectResult.sent(message);
        }

        String plainText = org.jsoup.Jsoup.parse(content).text();
        McModCommentResponse response = McModUtils.replyComment(groupId, contentType, containerId, commentId, plainText);
        String message;
        boolean success = false;
        if (response != null && response.isSuccess()) {
            message = "已回复评论 #" + commentId + "：" + plainText;
            success = true;
        } else {
            Integer state = response != null ? response.getState() : null;
            message = "回复评论失败：" + EnumStateCode.messageOf(state);
        }
        sendMessage(ctx.bot(), event, groupId, message);
        if (success) {
            ctx.bot().setMsgEmojiLikeHeart(msgId);
        } else {
            ctx.bot().setMsgEmojiLikeBrokenHeart(msgId);
        }
        return AiDirectResult.sent(message);
    }

    @Nonnull
    private String executeAiDeleteComment(@Nonnull AgentContext ctx, @Nullable String commentId) {
        if (!isValidTargetId(commentId)) {
            return "缺少有效的评论 ID。";
        }

        AnyMessageEvent event = buildAiMessageEvent(ctx, "/mcmod delcomment " + commentId);
        Long groupId = ctx.msgType() == xin.vanilla.banira.enums.EnumMessageType.GROUP ? ctx.groupId() : 0L;
        int msgId = StringUtils.toInt(ctx.msgId());
        if (McModUtils.resolveOptionalCookie(groupId) == null) {
            String message = "删除评论失败：请先登录 MC百科账号。";
            sendMessage(ctx.bot(), event, groupId, message);
            ctx.bot().setMsgEmojiLikeBrokenHeart(msgId);
            return AiDirectResult.sent(message);
        }

        McModCommentResponse response = McModUtils.deleteComment(groupId, commentId);
        String message;
        boolean success = false;
        if (response != null && response.isSuccess()) {
            McModCommentService.COMMENT_CACHE.values().forEach(comments ->
                    comments.removeIf(comment -> comment.getId().equals(commentId)
                            || (comment.getReplies() != null && comment.getReplies().stream()
                            .anyMatch(reply -> reply.getId().equals(commentId))))
            );
            message = "已删除评论 #" + commentId + "。";
            success = true;
        } else {
            Integer state = response != null ? response.getState() : null;
            message = "删除评论失败：" + EnumStateCode.messageOf(state);
        }
        sendMessage(ctx.bot(), event, groupId, message);
        if (success) {
            ctx.bot().setMsgEmojiLikeHeart(msgId);
        } else {
            ctx.bot().setMsgEmojiLikeBrokenHeart(msgId);
        }
        return AiDirectResult.sent(message);
    }

    @Nonnull
    private ResolvedTarget resolveAiContentTarget(@Nonnull AgentContext ctx, @Nonnull AnyMessageEvent event,
                                                  Long groupId, int msgId, @Nonnull EnumContentType type,
                                                  @Nullable String keywordOrId) {
        String keyword = keywordOrId != null ? keywordOrId.trim() : "";
        String typeName = contentTypeLabel(type);
        if (StringUtils.isNullOrEmptyEx(keyword)) {
            return ResolvedTarget.message("缺少" + typeName + "名称或 ID。");
        }
        if (isNumericId(keyword)) {
            McModContent content = loadContentById(type, keyword);
            return new ResolvedTarget(keyword,
                    content != null ? content.getFormattedName() : typeName + " #" + keyword,
                    null);
        }
        List<McModContent> results = searchContentByKeyword(type, keyword);
        if (CollectionUtils.isNullOrEmpty(results)) {
            return ResolvedTarget.message("未找到相关" + typeName + "信息。");
        }
        McModContent exact = pickBestContentMatch(results, keyword);
        if (exact != null) {
            return new ResolvedTarget(String.valueOf(exact.getId()), exact.getFormattedName(), null);
        }
        if (results.size() == 1) {
            McModContent first = results.getFirst();
            return new ResolvedTarget(String.valueOf(first.getId()), first.getFormattedName(), null);
        }

        handleContentResults(ctx.bot(), event, results, typeName, keyword, groupId, msgId);
        sendMessage(ctx.bot(), event, groupId, "找到多个匹配项，先告诉我要操作的具体 ID。");
        return ResolvedTarget.direct("找到多个匹配项，已发送列表。");
    }

    @Nullable
    private static McModContent loadContentById(@Nonnull EnumContentType type, @Nonnull String id) {
        return switch (type) {
            case MOD -> McModUtils.getModName(id);
            case MODPACK -> McModUtils.getModpackName(id);
            case AUTHOR -> McModUtils.getAuthorName(id);
            case USER_CENTER -> null;
        };
    }

    @Nonnull
    private static List<McModContent> searchContentByKeyword(@Nonnull EnumContentType type, @Nonnull String keyword) {
        return switch (type) {
            case MOD -> searchContentWithKeywordFallback(keyword, McModUtils::searchMod);
            case MODPACK -> searchContentWithKeywordFallback(keyword, McModUtils::searchModpack);
            case AUTHOR -> searchContentWithKeywordFallback(keyword, McModUtils::searchAuthor);
            case USER_CENTER -> List.of();
        };
    }

    @Nonnull
    private static List<McModContent> searchContentWithKeywordFallback(@Nullable String keyword,
                                                                       @Nonnull Function<String, List<McModContent>> searcher) {
        for (String variant : mcModKeywordVariants(keyword)) {
            List<McModContent> results = searcher.apply(variant);
            if (!CollectionUtils.isNullOrEmpty(results)) {
                if (!Objects.equals(variant, keyword)) {
                    LOGGER.debug("MCMod keyword fallback matched original={} variant={} results={}",
                            keyword, variant, results.size());
                }
                return results;
            }
        }
        return List.of();
    }

    @Nonnull
    private static List<String> mcModKeywordVariants(@Nullable String keyword) {
        String normalized = keyword != null ? keyword.trim() : "";
        if (StringUtils.isNullOrEmptyEx(normalized)) {
            return List.of();
        }
        LinkedHashSet<String> variants = new LinkedHashSet<>();
        variants.add(normalized);
        addKeywordVariant(variants, normalized, "清", "青");
        addKeywordVariant(variants, normalized, "青", "清");
        addKeywordVariant(variants, normalized, "葉", "叶");
        addKeywordVariant(variants, normalized, "叶", "葉");
        return List.copyOf(variants);
    }

    private static void addKeywordVariant(@Nonnull LinkedHashSet<String> variants,
                                          @Nonnull String keyword,
                                          @Nonnull String from,
                                          @Nonnull String to) {
        if (keyword.contains(from)) {
            variants.add(keyword.replace(from, to));
        }
    }

    @Nullable
    private static McModContent pickBestContentMatch(@Nonnull List<McModContent> results, @Nonnull String keyword) {
        String normalizedKeyword = normalizeMcModName(keyword);
        return results.stream()
                .filter(content -> normalizeMcModName(content.getShortName()).equals(normalizedKeyword)
                        || normalizeMcModName(content.getMainName()).equals(normalizedKeyword)
                        || normalizeMcModName(content.getSecondaryName()).equals(normalizedKeyword)
                        || normalizeMcModName(content.getFormattedName()).equals(normalizedKeyword))
                .findFirst()
                .orElse(null);
    }

    @Nonnull
    private static String normalizeMcModName(@Nullable String value) {
        if (value == null) {
            return "";
        }
        return value.replaceAll("[\\s\\[\\]（）()【】]", "").toLowerCase(Locale.ROOT);
    }

    @Nonnull
    private static String contentTypeLabel(@Nonnull EnumContentType type) {
        return switch (type) {
            case MOD -> "模组";
            case MODPACK -> "整合包";
            case AUTHOR -> "作者";
            case USER_CENTER -> "用户";
        };
    }

    @Nullable
    private static EnumContentType normalizeAiCommentType(@Nullable String type) {
        if (type == null) {
            return null;
        }
        return switch (type.trim().toLowerCase(Locale.ROOT)) {
            case "mod", "模组" -> EnumContentType.MOD;
            case "modpack", "pack", "整合包" -> EnumContentType.MODPACK;
            case "author", "作者" -> EnumContentType.AUTHOR;
            case "user", "center", "用户", "用户中心" -> EnumContentType.USER_CENTER;
            default -> null;
        };
    }

    private record ResolvedTarget(@Nullable String id, @Nonnull String displayName, @Nullable String result) {
        static ResolvedTarget message(@Nonnull String result) {
            return new ResolvedTarget(null, "", result);
        }

        static ResolvedTarget direct(@Nonnull String result) {
            return new ResolvedTarget(null, "", AiDirectResult.sent(result));
        }

        boolean resolved() {
            return StringUtils.isNotNullOrEmpty(id);
        }
    }

    @Nonnull
    private static AnyMessageEvent buildAiMessageEvent(@Nonnull AgentContext ctx, @Nonnull String message) {
        AnyMessageEvent event = new AnyMessageEvent();
        BaniraCodeContext messageContext = ctx.messageContext();
        event.setArrayMsg(messageContext != null && messageContext.originalMsg() != null ? messageContext.originalMsg() : List.of());
        event.setGroupId(ctx.msgType() == xin.vanilla.banira.enums.EnumMessageType.GROUP ? ctx.groupId() : 0L);
        event.setUserId(ctx.senderId());
        event.setMessageId(messageContext != null && messageContext.msgId() != null ? messageContext.msgId() : 0);
        event.setMessage(message);
        event.setRawMessage(message);
        event.setPlainText(message);
        event.setTime(messageContext != null && messageContext.time() != null ? messageContext.time() : System.currentTimeMillis() / 1000);
        event.setSelfId(ctx.botId());
        event.setMessageType(ctx.msgType() == xin.vanilla.banira.enums.EnumMessageType.GROUP ? ActionParams.GROUP : ActionParams.PRIVATE);
        GroupMessageEvent.GroupSender sender = new GroupMessageEvent.GroupSender();
        sender.setUserId(ctx.senderId());
        sender.setNickname(ctx.bot().getUserNameEx(ctx.groupId(), ctx.senderId()));
        event.setSender(sender);
        return event;
    }

    @Nonnull
    private static String normalizeAiSearchType(@Nullable String type) {
        if (type == null) {
            return "";
        }
        return switch (type.trim().toLowerCase(Locale.ROOT)) {
            case "mod", "模组" -> "mod";
            case "modpack", "pack", "整合包" -> "modpack";
            case "author", "作者" -> "author";
            case "user", "用户" -> "user";
            case "item", "data", "资料", "物品" -> "item";
            case "tutorial", "教程" -> "tutorial";
            case "random", "随机", "随便看看" -> "random";
            default -> "";
        };
    }

    @Nonnull
    private static String buildSearchDetail(@Nonnull String slashCmd, @Nonnull Collection<String> aliases, boolean supportPaging) {
        String ins = HelpTopics.formatAliasChoices(aliases);
        StringBuilder sb = new StringBuilder();
        sb.append(slashCmd).append(' ').append(ins).append(" <关键词或ID>");
        sb.append('\n').append("多结果时回复列表图片并发送序号查看详情，也可直接使用 ID 查询");
        if (supportPaging) {
            sb.append('\n')
                    .append(slashCmd).append(' ').append(ins).append(" -a <关键词>\n")
                    .append("（分页搜索，结果较多时使用）");
        }
        return sb.toString();
    }

    @Nonnull
    private static String buildPushHelp(@Nonnull String slashCmd) {
        String push = HelpTopics.formatAliasChoices(PUSH_INS);
        String modType = HelpTopics.formatAliasChoices(MOD_INS);
        String modpackType = HelpTopics.formatAliasChoices(MOD_PACK_INS);
        return slashCmd + " " + push + " " + modType + " <模组ID>\n"
                + slashCmd + " " + push + " " + modpackType + " <整合包ID>";
    }

    @Nonnull
    private static String buildVoteHelp(@Nonnull String slashCmd) {
        String vote = HelpTopics.formatAliasChoices(VOTE_INS);
        String modType = HelpTopics.formatAliasChoices(MOD_INS);
        String modpackType = HelpTopics.formatAliasChoices(MOD_PACK_INS);
        String red = HelpTopics.formatAliasChoices(RED_VOTE_INS);
        String black = HelpTopics.formatAliasChoices(BLACK_VOTE_INS);
        return slashCmd + " " + vote + " " + modType + " <模组ID> " + red + "|" + black + "\n"
                + slashCmd + " " + vote + " " + modpackType + " <整合包ID> " + red + "|" + black;
    }

    @AnyMessageHandler
    public boolean selectSearchListItem(BaniraBot bot, AnyMessageEvent event) {
        if (!BaniraUtils.hasReply(event.getArrayMsg())) {
            return false;
        }
        Long replyId = BaniraUtils.getReplyId(event.getArrayMsg());
        Optional<McModSearchListSession> sessionOpt = mcModSearchListStore.findByMessageId(replyId);
        if (sessionOpt.isEmpty()) {
            return false;
        }
        McModSearchListSession session = sessionOpt.get();
        if (!Objects.equals(session.userId(), event.getUserId())) {
            return bot.setMsgEmojiLikeNo(event.getMessageId());
        }
        Integer index = parseListSelectionIndex(event.getMessage());
        if (index == null || index < 1 || index > session.itemCount()) {
            sendMessage(bot, event, event.getGroupId(),
                    "请输入 1-" + session.itemCount() + " 之间的序号");
            return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
        }
        bot.setMsgEmojiLikeOk(event.getMessageId());
        Long groupId = event.getGroupId();
        String typeName = session.typeName();
        if (session.source() == McModSearchListSource.CONTENT) {
            McModContent content = session.contents().get(index - 1);
            sendContentDetail(bot, event, groupId, typeName, content);
        } else {
            McModSearchResult result = session.searchResults().get(index - 1);
            sendSearchDetail(bot, event, groupId, typeName, result);
        }
        return true;
    }

    @AnyMessageHandler
    public boolean query(BaniraBot bot, AnyMessageEvent event) {
        String message = event.getMessage();
        if (insConfig.get().mcMod().stream().noneMatch(ins -> message.startsWith("/" + ins + " "))) {
            return false;
        }
        String[] split = message.split("\\s+");
        if (split.length < 2) {
            return false;
        }
        String ins = split[1];
        Long groupId = event.getGroupId();
        int msgId = event.getMessageId();

        bot.setMsgEmojiLikeOk(msgId);

        // 搜索模组
        if (MOD_INS.contains(ins)) {
            if (split.length < 3) {
                return bot.setMsgEmojiLikeBrokenHeart(msgId);
            }
            if (split.length > 3 && split[2].equals("-a")) {
                String keyword = String.join(" ", Arrays.copyOfRange(split, 3, split.length));
                if (isNumericId(keyword)) {
                    McModContent direct = McModUtils.getModName(keyword);
                    if (direct != null) {
                        return handleContentResults(bot, event, List.of(direct), "模组", keyword, groupId, msgId);
                    }
                }
                List<McModSearchResult> results = McModUtils.searchModBySearchPage(keyword);
                return handleSearchResults(bot, event, results, "模组", keyword, groupId, msgId);
            } else {
                String keyword = String.join(" ", Arrays.copyOfRange(split, 2, split.length));
                if (isNumericId(keyword)) {
                    McModContent direct = McModUtils.getModName(keyword);
                    if (direct != null) {
                        return handleContentResults(bot, event, List.of(direct), "模组", keyword, groupId, msgId);
                    }
                }
                List<McModContent> results = McModUtils.searchMod(keyword);
                return handleContentResults(bot, event, results, "模组", keyword, groupId, msgId);
            }
        }
        // 搜索整合包
        else if (MOD_PACK_INS.contains(ins)) {
            if (split.length < 3) {
                return bot.setMsgEmojiLikeBrokenHeart(msgId);
            }
            if (split.length > 3 && split[2].equals("-a")) {
                String keyword = String.join(" ", Arrays.copyOfRange(split, 3, split.length));
                if (isNumericId(keyword)) {
                    McModContent direct = McModUtils.getModpackName(keyword);
                    if (direct != null) {
                        return handleContentResults(bot, event, List.of(direct), "整合包", keyword, groupId, msgId);
                    }
                }
                List<McModSearchResult> results = McModUtils.searchModpackBySearchPage(keyword);
                return handleSearchResults(bot, event, results, "整合包", keyword, groupId, msgId);
            } else {
                String keyword = String.join(" ", Arrays.copyOfRange(split, 2, split.length));
                if (isNumericId(keyword)) {
                    McModContent direct = McModUtils.getModpackName(keyword);
                    if (direct != null) {
                        return handleContentResults(bot, event, List.of(direct), "整合包", keyword, groupId, msgId);
                    }
                }
                List<McModContent> results = McModUtils.searchModpack(keyword);
                return handleContentResults(bot, event, results, "整合包", keyword, groupId, msgId);
            }
        }
        // 搜索作者
        else if (AUTHOR_INS.contains(ins)) {
            if (split.length < 3) {
                return bot.setMsgEmojiLikeBrokenHeart(msgId);
            }
            if (split.length > 3 && split[2].equals("-a")) {
                String keyword = String.join(" ", Arrays.copyOfRange(split, 3, split.length));
                if (isNumericId(keyword)) {
                    McModContent direct = McModUtils.getAuthorName(keyword);
                    if (direct != null) {
                        return handleContentResults(bot, event, List.of(direct), "作者", keyword, groupId, msgId);
                    }
                }
                List<McModSearchResult> results = McModUtils.searchAuthorBySearchPage(keyword);
                return handleSearchResults(bot, event, results, "作者", keyword, groupId, msgId);
            } else {
                String keyword = String.join(" ", Arrays.copyOfRange(split, 2, split.length));
                if (isNumericId(keyword)) {
                    McModContent direct = McModUtils.getAuthorName(keyword);
                    if (direct != null) {
                        return handleContentResults(bot, event, List.of(direct), "作者", keyword, groupId, msgId);
                    }
                }
                List<McModContent> results = McModUtils.searchAuthor(keyword);
                return handleContentResults(bot, event, results, "作者", keyword, groupId, msgId);
            }
        }
        // 搜索用户
        else if (USER_INS.contains(ins)) {
            if (split.length < 3) {
                return bot.setMsgEmojiLikeBrokenHeart(msgId);
            }
            String keyword = String.join(" ", Arrays.copyOfRange(split, 2, split.length));
            if (isNumericId(keyword)) {
                McModSearchResult direct = buildUserSearchResult(keyword);
                if (direct != null) {
                    return handleSearchResults(bot, event, List.of(direct), "用户", keyword, groupId, msgId);
                }
            }
            List<McModSearchResult> results = McModUtils.searchUserBySearchPage(keyword);
            return handleSearchResults(bot, event, results, "用户", keyword, groupId, msgId);
        }
        // 搜索资料
        else if (ITEM_INS.contains(ins)) {
            if (split.length < 3) {
                return bot.setMsgEmojiLikeBrokenHeart(msgId);
            }
            String keyword = String.join(" ", Arrays.copyOfRange(split, 2, split.length));
            List<McModSearchResult> results = McModUtils.searchDataBySearchPage(keyword);
            return handleSearchResults(bot, event, results, "资料", keyword, groupId, msgId);
        }
        // 搜索教程
        else if (TUTORIAL_INS.contains(ins)) {
            if (split.length < 3) {
                return bot.setMsgEmojiLikeBrokenHeart(msgId);
            }
            String keyword = String.join(" ", Arrays.copyOfRange(split, 2, split.length));
            List<McModSearchResult> results = McModUtils.searchTutorialBySearchPage(keyword);
            return handleSearchResults(bot, event, results, "教程", keyword, groupId, msgId);
        }
        // 随便看看
        else if (MOD_RANDOM_INS.contains(ins)) {
            List<McModContent> randomMods = McModUtils.getRandomMods();
            if (CollectionUtils.isNullOrEmpty(randomMods)) {
                randomMods = loadRandomModsFromCategory();
            }
            return handleContentResults(bot, event, randomMods, "模组", "", groupId, msgId);
        }
        // 分类浏览
        else if (CATEGORY_INS.contains(ins)) {
            if (split.length < 3) {
                return bot.setMsgEmojiLikeBrokenHeart(msgId);
            }
            String categoryStr = String.join(" ", Arrays.copyOfRange(split, 2, split.length));
            EnumModCategory category = EnumModCategory.valueOfEx(categoryStr);
            if (category == null) {
                return bot.setMsgEmojiLikeBrokenHeart(msgId);
            }
            McModIndexCategoryResult indexResult = McModUtils.getIndexCategory(category);
            List<McModContent> contents = convertIndexCategoryToContents(indexResult);
            return handleContentResults(bot, event, contents, category.name(), category.name(), groupId, msgId);
        }
        // 用户卡片
        else if (USER_CARD_INS.contains(ins)) {
            if (split.length < 3) {
                return bot.setMsgEmojiLikeBrokenHeart(msgId);
            }
            String userId = split[2];
            McModUserCardResult userCard = McModUtils.getUserCard(userId);
            if (userCard == null) {
                return bot.setMsgEmojiLikeBrokenHeart(msgId);
            }
            String imageMsg = McModRenderHelper.renderUserCard(userCard, userId);
            if (StringUtils.isNullOrEmptyEx(imageMsg)) {
                String result = "【用户卡片】\n" +
                        "用户名: " + userCard.getUsername() + "\n" +
                        "用户ID: " + userId + "\n" +
                        "等级: " + (userCard.getRank() != null ? userCard.getRank() : "未知") + "\n" +
                        "在线状态: " + (userCard.getOnline() != null ?
                        (userCard.getOnline() == 1 ? "在线" : userCard.getOnline() == 0 ? "离线" : "隐身") : "未知");
                if (StringUtils.isNotNullOrEmpty(userCard.getSign())) {
                    result += "\n签名: " + userCard.getSign();
                }
                sendMessage(bot, event, groupId, result);
            } else {
                sendMessage(bot, event, groupId,
                        McModRenderHelper.wrapCardMessage(McModUtils.getUserCenterUrl(userId), imageMsg));
            }
            return bot.setMsgEmojiLikeOk(msgId);
        }
        // 推荐
        else if (PUSH_INS.contains(ins)) {
            return handlePush(bot, event, split, groupId, msgId);
        }
        // 投票
        else if (VOTE_INS.contains(ins)) {
            return handleVote(bot, event, split, groupId, msgId);
        } else {
            return bot.setMsgEmojiLikeBrokenHeart(msgId);
        }
    }

    // region 互动

    private boolean handlePush(BaniraBot bot, AnyMessageEvent event, String[] split, Long groupId, int msgId) {
        if (split.length < 4) {
            return bot.setMsgEmojiLikeBrokenHeart(msgId);
        }
        InteractionTarget target = parseInteractionTarget(split[2]);
        String targetId = split[3];
        if (target == null || !isValidTargetId(targetId)) {
            return bot.setMsgEmojiLikeBrokenHeart(msgId);
        }

        McModPushResponse response = target == InteractionTarget.MOD
                ? McModUtils.pushMod(groupId, targetId)
                : McModUtils.pushModpack(groupId, targetId);
        if (response != null && response.isSuccess()) {
            return bot.setMsgEmojiLikeHeart(msgId);
        }
        Integer state = response != null ? response.getState() : null;
        LOGGER.error("Failed to push {}, id: {}, state: {}", target.label(), targetId, state);
        sendMcModFailure(bot, event, groupId, "推荐", state);
        return bot.setMsgEmojiLikeBrokenHeart(msgId);
    }

    private boolean handleVote(BaniraBot bot, AnyMessageEvent event, String[] split, Long groupId, int msgId) {
        if (split.length < 5) {
            return bot.setMsgEmojiLikeBrokenHeart(msgId);
        }
        InteractionTarget target = parseInteractionTarget(split[2]);
        String targetId = split[3];
        EnumCardVoteType voteType = parseVoteType(split[4]);
        if (target == null || !isValidTargetId(targetId) || voteType == null) {
            return bot.setMsgEmojiLikeBrokenHeart(msgId);
        }

        McModCardVoteEnsureResult result = target == InteractionTarget.MOD
                ? McModUtils.ensureModVote(groupId, targetId, voteType)
                : McModUtils.ensureModpackVote(groupId, targetId, voteType);
        if (result != null && result.isCooldownBlocked()) {
            sendMessage(bot, event, groupId, EnumStateCode.C109.message());
            return bot.setMsgEmojiLikeBrokenHeart(msgId);
        }
        if (result != null && result.isFromCache()) {
            sendMessage(bot, event, groupId, EnumStateCode.C106.message());
            return bot.setMsgEmojiLikeBrokenHeart(msgId);
        }
        if (result != null && result.isLoginRequired()) {
            sendMcModFailure(bot, event, groupId, "投票", EnumStateCode.C108.code());
            return bot.setMsgEmojiLikeBrokenHeart(msgId);
        }
        if (result != null && result.isTooFrequent()) {
            sendMcModFailure(bot, event, groupId, "投票", EnumStateCode.C109.code());
            return bot.setMsgEmojiLikeBrokenHeart(msgId);
        }
        if (result != null && result.isSuccess()) {
            if (result.isUnchanged() && !result.isFromCache()) {
                sendMessage(bot, event, groupId, EnumStateCode.C106.message());
                return bot.setMsgEmojiLikeBrokenHeart(msgId);
            }
            return bot.setMsgEmojiLikeHeart(msgId);
        }
        Integer errorState = result != null ? result.getErrorState() : null;
        LOGGER.error("Failed to vote {}, id: {}, type: {}, errorState: {}", target.label(), targetId, voteType, errorState);
        sendMcModFailure(bot, event, groupId, "投票", errorState);
        return bot.setMsgEmojiLikeBrokenHeart(msgId);
    }

    private enum InteractionTarget {
        MOD("模组"),
        MODPACK("整合包");

        private final String label;

        InteractionTarget(String label) {
            this.label = label;
        }

        String label() {
            return label;
        }
    }

    @Nullable
    private static InteractionTarget parseInteractionTarget(@Nullable String typeStr) {
        if (typeStr == null) {
            return null;
        }
        if (MOD_INS.contains(typeStr)) {
            return InteractionTarget.MOD;
        }
        if (MOD_PACK_INS.contains(typeStr)) {
            return InteractionTarget.MODPACK;
        }
        return null;
    }

    @Nullable
    private static EnumCardVoteType parseVoteType(@Nullable String typeStr) {
        if (typeStr == null) {
            return null;
        }
        if (RED_VOTE_INS.contains(typeStr)) {
            return EnumCardVoteType.RED;
        }
        if (BLACK_VOTE_INS.contains(typeStr)) {
            return EnumCardVoteType.BLACK;
        }
        return null;
    }

    private static boolean isValidTargetId(@Nullable String targetId) {
        return StringUtils.isNotNullOrEmpty(targetId) && targetId.matches("\\d+");
    }

    // endregion 互动

    /**
     * 处理搜索结果
     */
    private boolean handleSearchResults(BaniraBot bot, AnyMessageEvent event, List<McModSearchResult> results,
                                        String type, String keyword, Long groupId, int msgId) {
        if (CollectionUtils.isNullOrEmpty(results)) {
            sendMessage(bot, event, groupId, "未找到相关" + type + "信息，可能是被百科娘吃掉了");
            return bot.setMsgEmojiLikeBrokenHeart(msgId);
        }

        if (results.size() == 1) {
            McModSearchResult result = results.getFirst();
            sendSearchDetail(bot, event, groupId, type, result);
            return bot.setMsgEmojiLikeOk(msgId);
        }

        int maxResults = Math.min(results.size(), 12);
        List<McModSearchResult> displayResults = results.subList(0, maxResults);
        List<JsonObject> listItems = displayResults.stream().map(McModRenderHelper::toListItem).toList();
        String imageMsg = McModRenderHelper.renderList(type, keyword, listItems);
        if (StringUtils.isNotNullOrEmpty(imageMsg)) {
            Integer listMsgId = sendMessageAndGetId(bot, event, groupId, imageMsg);
            saveSearchListSession(listMsgId, event, groupId, type, McModSearchListSource.SEARCH, displayResults, null);
            if (results.size() > maxResults) {
                sendMessage(bot, event, groupId, "... 还有 " + (results.size() - maxResults) + " 条结果未显示");
            }
            return bot.setMsgEmojiLikeOk(msgId);
        }

        return sendSearchForward(bot, event, results, type, groupId, msgId);
    }

    /**
     * 处理内容搜索结果
     */
    private boolean handleContentResults(BaniraBot bot, AnyMessageEvent event, List<McModContent> results,
                                         String type, String keyword, Long groupId, int msgId) {
        if (CollectionUtils.isNullOrEmpty(results)) {
            sendMessage(bot, event, groupId, "未找到相关" + type + "信息，可能是被百科娘吃掉了");
            return bot.setMsgEmojiLikeBrokenHeart(msgId);
        }

        if (results.size() == 1) {
            McModContent result = results.getFirst();
            sendContentDetail(bot, event, groupId, type, result);
            return bot.setMsgEmojiLikeOk(msgId);
        }

        int maxResults = Math.min(results.size(), 12);
        List<McModContent> displayResults = results.subList(0, maxResults);
        List<JsonObject> listItems = displayResults.stream().map(McModRenderHelper::toListItem).toList();
        String imageMsg = McModRenderHelper.renderList(type, keyword, listItems);
        if (StringUtils.isNotNullOrEmpty(imageMsg)) {
            Integer listMsgId = sendMessageAndGetId(bot, event, groupId, imageMsg);
            saveSearchListSession(listMsgId, event, groupId, type, McModSearchListSource.CONTENT, null, displayResults);
            if (results.size() > maxResults) {
                sendMessage(bot, event, groupId, "... 还有 " + (results.size() - maxResults) + " 条结果未显示");
            }
            return bot.setMsgEmojiLikeOk(msgId);
        }

        return sendContentForward(bot, event, results, type, groupId, msgId);
    }

    private boolean sendSearchForward(BaniraBot bot, AnyMessageEvent event, List<McModSearchResult> results,
                                      String type, Long groupId, int msgId) {
        LoginInfoResp loginInfoEx = bot.getLoginInfoEx();
        List<Map<String, Object>> forwardMsg = new ArrayList<>();
        forwardMsg.add(com.mikuac.shiro.common.utils.ShiroUtils.generateSingleMsg(
                event.getUserId(),
                event.getSender().getNickname(),
                event.getMessage()
        ));
        forwardMsg.add(com.mikuac.shiro.common.utils.ShiroUtils.generateSingleMsg(
                bot.getSelfId(),
                loginInfoEx.getNickname(),
                "找到 " + results.size() + " 条" + type + "搜索结果："
        ));

        int maxResults = Math.min(results.size(), 20);
        for (int i = 0; i < maxResults; i++) {
            McModSearchResult result = results.get(i);
            String msg = (i + 1) + ". " + buildSearchResultMessage(result);
            forwardMsg.add(com.mikuac.shiro.common.utils.ShiroUtils.generateSingleMsg(
                    bot.getSelfId(),
                    loginInfoEx.getNickname(),
                    msg
            ));
        }

        if (results.size() > maxResults) {
            forwardMsg.add(com.mikuac.shiro.common.utils.ShiroUtils.generateSingleMsg(
                    bot.getSelfId(),
                    loginInfoEx.getNickname(),
                    "... 还有 " + (results.size() - maxResults) + " 条结果未显示"
            ));
        }

        sendForward(bot, event, groupId, forwardMsg);
        return bot.setMsgEmojiLikeOk(msgId);
    }

    private boolean sendContentForward(BaniraBot bot, AnyMessageEvent event, List<McModContent> results,
                                       String type, Long groupId, int msgId) {
        LoginInfoResp loginInfoEx = bot.getLoginInfoEx();
        List<Map<String, Object>> forwardMsg = new ArrayList<>();
        forwardMsg.add(com.mikuac.shiro.common.utils.ShiroUtils.generateSingleMsg(
                event.getUserId(),
                event.getSender().getNickname(),
                event.getMessage()
        ));
        forwardMsg.add(com.mikuac.shiro.common.utils.ShiroUtils.generateSingleMsg(
                bot.getSelfId(),
                loginInfoEx.getNickname(),
                "找到 " + results.size() + " 条" + type + "搜索结果："
        ));

        int maxResults = Math.min(results.size(), 20);
        for (int i = 0; i < maxResults; i++) {
            McModContent result = results.get(i);
            forwardMsg.add(com.mikuac.shiro.common.utils.ShiroUtils.generateSingleMsg(
                    bot.getSelfId(),
                    loginInfoEx.getNickname(),
                    buildContentMessage(result)
            ));
        }

        if (results.size() > maxResults) {
            forwardMsg.add(com.mikuac.shiro.common.utils.ShiroUtils.generateSingleMsg(
                    bot.getSelfId(),
                    loginInfoEx.getNickname(),
                    "... 还有 " + (results.size() - maxResults) + " 条结果未显示"
            ));
        }

        sendForward(bot, event, groupId, forwardMsg);
        return bot.setMsgEmojiLikeOk(msgId);
    }

    private static void sendMessage(BaniraBot bot, AnyMessageEvent event, Long groupId, String message) {
        if (BaniraUtils.isGroupIdValid(groupId)) {
            bot.sendGroupMsg(groupId, message, false);
        } else {
            bot.sendPrivateMsg(event.getUserId(), message, false);
        }
    }

    private static void sendMcModFailure(BaniraBot bot, AnyMessageEvent event, Long groupId, String action,
                                         @Nullable Integer state) {
        sendMessage(bot, event, groupId, action + "失败，" + EnumStateCode.messageOf(state));
    }

    private static void sendMcModFailure(BaniraBot bot, GroupMessageEvent event, String action,
                                         @Nullable Integer state) {
        bot.sendGroupMsg(event.getGroupId(), action + "失败，" + EnumStateCode.messageOf(state), false);
    }

    @Nullable
    private static Integer sendMessageAndGetId(BaniraBot bot, AnyMessageEvent event, Long groupId, String message) {
        ActionData<MsgId> msgIdData;
        if (BaniraUtils.isGroupIdValid(groupId)) {
            msgIdData = bot.sendGroupMsg(groupId, message, false);
        } else {
            msgIdData = bot.sendPrivateMsg(event.getUserId(), message, false);
        }
        return bot.isActionDataMsgIdNotEmpty(msgIdData) ? bot.getActionDataMsgId(msgIdData) : null;
    }

    private void saveSearchListSession(@Nullable Integer listMsgId, @Nonnull AnyMessageEvent event, @Nullable Long groupId,
                                       @Nonnull String typeName, @Nonnull McModSearchListSource source,
                                       @Nullable List<McModSearchResult> searchResults,
                                       @Nullable List<McModContent> contents) {
        if (listMsgId == null || listMsgId <= 0) {
            return;
        }
        McModSearchListSession session = new McModSearchListSession()
                .messageId(listMsgId.longValue())
                .userId(event.getUserId())
                .groupId(BaniraUtils.isGroupIdValid(groupId) ? groupId : 0L)
                .typeName(typeName)
                .source(source)
                .expireAt(System.currentTimeMillis() + SEARCH_LIST_SESSION_TTL_MS);
        if (source == McModSearchListSource.SEARCH && searchResults != null) {
            session.searchResults(new ArrayList<>(searchResults));
        } else if (source == McModSearchListSource.CONTENT && contents != null) {
            session.contents(new ArrayList<>(contents));
        }
        mcModSearchListStore.save(session);
    }

    private static void sendSearchDetail(BaniraBot bot, AnyMessageEvent event, Long groupId,
                                         String typeName, McModSearchResult result) {
        String imageMsg = McModRenderHelper.renderSearchResult(result, typeName, groupId);
        if (StringUtils.isNotNullOrEmpty(imageMsg)) {
            String message = McModRenderHelper.shouldPrefixCardLink(typeName)
                    ? McModRenderHelper.wrapCardMessage(result.getLink(), imageMsg)
                    : imageMsg;
            sendMessage(bot, event, groupId, message);
        } else {
            sendMessage(bot, event, groupId, buildSearchResultMessage(result));
        }
    }

    private static void sendContentDetail(BaniraBot bot, AnyMessageEvent event, Long groupId,
                                          String typeName, McModContent content) {
        String imageMsg = McModRenderHelper.renderContent(content, typeName, groupId);
        if (StringUtils.isNotNullOrEmpty(imageMsg)) {
            String message = McModRenderHelper.shouldPrefixCardLink(typeName)
                    ? McModRenderHelper.wrapCardMessage(content.getDetailUrl(), imageMsg)
                    : imageMsg;
            sendMessage(bot, event, groupId, message);
        } else {
            sendMessage(bot, event, groupId, buildContentMessage(content));
        }
    }

    @Nullable
    private static McModSearchResult buildUserSearchResult(@Nonnull String userId) {
        McModUserCardResult card = McModUtils.getUserCard(userId);
        if (card == null) {
            return null;
        }
        return new McModSearchResult()
                .setTitle(card.getUsername())
                .setLink(McModUtils.getUserCenterUrl(userId));
    }

    private static boolean isNumericId(@Nullable String value) {
        return StringUtils.isNotNullOrEmpty(value) && value.matches("\\d+");
    }

    @Nullable
    private static Integer parseListSelectionIndex(@Nullable String message) {
        if (StringUtils.isNullOrEmptyEx(message)) {
            return null;
        }
        String text = BaniraUtils.replaceReply(message).replaceAll("\\[CQ:[^\\]]+]", "").trim();
        if (!text.matches("\\d+")) {
            return null;
        }
        try {
            return Integer.parseInt(text);
        } catch (NumberFormatException e) {
            return null;
        }
    }

    private static void sendForward(BaniraBot bot, AnyMessageEvent event, Long groupId, List<Map<String, Object>> forwardMsg) {
        if (BaniraUtils.isGroupIdValid(groupId)) {
            bot.sendGroupForwardMsg(groupId, forwardMsg);
        } else {
            bot.sendPrivateForwardMsg(event.getUserId(), forwardMsg);
        }
    }

    @Nonnull
    private static String buildContentMessage(@Nonnull McModContent result) {
        MsgUtils msg = MsgUtils.builder()
                .text(result.getFormattedName()).text("\n")
                .text("编号: " + result.getId()).text("\n")
                .text("链接: " + result.getDetailUrl());
        if (StringUtils.isNotNullOrEmpty(result.getCoverImageUrl())) {
            msg.text("\n").img(result.getCoverImageUrl());
        }
        return msg.build();
    }

    @Nonnull
    private static List<McModContent> convertIndexCategoryToContents(@Nullable McModIndexCategoryResult indexResult) {
        if (indexResult == null || indexResult.getLeft() == null || CollectionUtils.isNullOrEmpty(indexResult.getLeft().getModFrames())) {
            return List.of();
        }
        List<McModContent> contents = new ArrayList<>();
        for (McModIndexCategoryModFrame frame : indexResult.getLeft().getModFrames()) {
            contents.add(new McModContent(EnumContentType.MOD, frame.getModId(), frame.getShortName(),
                    frame.getMainName(), frame.getSecondaryName(), frame.getCoverImageUrl(), null));
        }
        return contents;
    }

    @Nonnull
    private static List<McModContent> loadRandomModsFromCategory() {
        EnumModCategory[] categories = EnumModCategory.values();
        EnumModCategory category = categories[new Random().nextInt(categories.length)];
        McModIndexCategoryResult indexResult = McModUtils.getIndexCategory(category);
        return convertIndexCategoryToContents(indexResult);
    }

    /**
     * 构建搜索结果消息
     */
    @Nonnull
    private static String buildSearchResultMessage(@Nonnull McModSearchResult result) {
        MsgUtils msg = MsgUtils.builder();

        // 副标题
        if (StringUtils.isNotNullOrEmpty(result.getSubtitle())) {
            msg.text(result.getSubtitle()).text(" ");
        }

        // 标题
        if (StringUtils.isNotNullOrEmpty(result.getTitle())) {
            msg.text(result.getTitle());
        }

        // 摘要
        if (StringUtils.isNotNullOrEmpty(result.getSummary())) {
            msg.text("\n").text(result.getSummary());
        }

        // 链接
        if (StringUtils.isNotNullOrEmpty(result.getLink())) {
            msg.text("\n链接: ").text(result.getLink());
        }

        //  时间
        if (result.getSnapshotTime() != null) {
            msg.text("\n时间: ").text(DateUtils.toString(result.getSnapshotTime()));
        }

        return msg.build();
    }

    @GroupMessageHandler
    public boolean postComment(BaniraBot bot, GroupMessageEvent event) {
        if (!bot.isAdmin(event.getGroupId(), event.getSender().getUserId())) {
            return false;
        }

        String message = event.getMessage();
        String[] split = message.split("\\s+");
        if (split.length < 5) {
            return false;
        }

        if (insConfig.get().mcMod().stream().noneMatch(ins -> ("/" + ins).equals(split[0]))) {
            return false;
        }

        if (!COMMENT_INS.contains(split[1])) {
            return false;
        }

        bot.setMsgEmojiLikeOk(event.getMessageId());

        EnumContentType commentType = parseCommentType(split[2]);
        String containerId = split[3];
        if (commentType == null || StringUtils.isNullOrEmptyEx(containerId) || !containerId.matches("\\d+")) {
            return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
        }

        String commentText = String.join(" ", Arrays.copyOfRange(split, 4, split.length));
        if (StringUtils.isNullOrEmptyEx(commentText)) {
            return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
        }

        String htmlText = "<p>" + org.jsoup.Jsoup.parse(commentText).text() + "</p>";
        McModCommentResponse response = McModUtils.comment(event.getGroupId(), commentType, containerId, htmlText);
        if (response != null && response.isSuccess()) {
            return bot.setMsgEmojiLikeHeart(event.getMessageId());
        }
        Integer state = response != null ? response.getState() : null;
        LOGGER.error("Failed to post comment, state: {}", state);
        sendMcModFailure(bot, event, "发布评论", state);
        return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
    }


    @GroupMessageHandler
    public boolean commentReply(BaniraBot bot, GroupMessageEvent event) {
        if (!BaniraUtils.hasReply(event.getArrayMsg())) {
            return false;
        }

        if (!bot.isAdmin(event.getGroupId(), event.getSender().getUserId())) {
            return false;
        }

        String msg = BaniraUtils.replaceReply(event.getMessage());
        String[] split = msg.split("\\s+");
        if (split.length < 2) {
            return false;
        }

        // 检查是否是mcmod命令
        if (insConfig.get().mcMod().stream().noneMatch(ins -> ("/" + ins).equals(split[0]))) {
            return false;
        }

        // 检查是否是回复命令
        if (!COMMENT_REPLY_INS.contains(split[1])) {
            return false;
        }

        bot.setMsgEmojiLikeOk(event.getMessageId());

        // 获取被回复的消息内容
        Long replyId = BaniraUtils.getReplyId(event.getArrayMsg());
        if (replyId == null) {
            LOGGER.error("Failed to get reply id");
            return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
        }

        // 获取被回复的消息内容
        String repliedMessage = getRepliedMessage(bot, event.getGroupId(), replyId);
        if (StringUtils.isNullOrEmptyEx(repliedMessage)) {
            LOGGER.error("Failed to get replied message");
            return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
        }

        // 从消息内容中解析评论类型、容器ID和评论ID
        CommentInfo parsedInfo = parseCommentInfoFromMessage(repliedMessage);
        if (parsedInfo == null) {
            LOGGER.error("Failed to parse comment info from message");
            return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
        }

        // 获取回复内容（从split[2]开始的所有内容）
        String replyContent = split.length >= 3 ? String.join(" ", Arrays.copyOfRange(split, 2, split.length)) : "";
        if (StringUtils.isNullOrEmptyEx(replyContent)) {
            LOGGER.error("Failed to get reply content");
            return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
        }

        // 回复评论
        McModCommentResponse response = McModUtils.replyComment(event.getGroupId(), parsedInfo.commentType(),
                parsedInfo.containerId(), parsedInfo.commentId(), replyContent);
        if (response != null && response.isSuccess()) {
            return bot.setMsgEmojiLikeHeart(event.getMessageId());
        }
        Integer state = response != null ? response.getState() : null;
        LOGGER.error("Failed to reply comment, state: {}", state);
        sendMcModFailure(bot, event, "回复评论", state);
        return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
    }

    /**
     * 获取被回复的消息内容
     */
    private String getRepliedMessage(BaniraBot bot, Long groupId, Long replyId) {
        try {
            List<ArrayMsg> replyContent = BaniraUtils.getReplyContentById(bot, replyId);
            if (replyContent != null && !replyContent.isEmpty()) {
                return com.mikuac.shiro.common.utils.MessageConverser.arraysToString(replyContent);
            }

            MessageRecord record = messageRecordManager.getGroupMessageRecord(groupId, replyId.intValue());
            if (record != null && StringUtils.isNotNullOrEmpty(record.getMsgRecode())) {
                return record.getMsgRecode();
            }

            return null;
        } catch (Exception e) {
            LOGGER.error("Error getting replied message", e);
            return null;
        }
    }

    /**
     * 从消息内容中解析评论信息
     */
    private CommentInfo parseCommentInfoFromMessage(String message) {
        if (StringUtils.isNullOrEmptyEx(message)) {
            return null;
        }

        try {
            Matcher compactMatcher = McModCommentService.COMMENT_METADATA_PATTERN.matcher(message);
            if (compactMatcher.find()) {
                EnumContentType commentType = EnumContentType.valueOfEx(compactMatcher.group(1));
                if (commentType == null) {
                    return null;
                }
                return new CommentInfo(commentType, compactMatcher.group(2), compactMatcher.group(3));
            }

            // 兼容旧格式：类型/容器/编号
            String[] lines = message.split("\n");
            String commentTypeStr = null;
            String containerId = null;
            String commentId = null;

            for (int i = 0; i < lines.length && i < 8; i++) {
                String line = lines[i].trim();
                if (line.startsWith("类型:")) {
                    commentTypeStr = line.substring("类型:".length()).trim();
                } else if (line.startsWith("容器:")) {
                    containerId = line.substring("容器:".length()).trim();
                } else if (line.startsWith("编号:")) {
                    commentId = line.substring("编号:".length()).trim();
                }
            }

            if (commentTypeStr == null || containerId == null || commentId == null) {
                LOGGER.warn("Failed to parse comment info: type={}, container={}, id={}", commentTypeStr, containerId, commentId);
                return null;
            }

            EnumContentType commentType = EnumContentType.valueOfEx(commentTypeStr);
            if (commentType == null) {
                LOGGER.warn("Unknown comment type: {}", commentTypeStr);
                return null;
            }

            return new CommentInfo(commentType, containerId, commentId);
        } catch (Exception e) {
            LOGGER.error("Error parsing comment info from message", e);
            return null;
        }
    }

    /**
     * 评论信息
     */
    private record CommentInfo(EnumContentType commentType, String containerId, String commentId) {
    }

    /**
     * 处理删除评论
     */
    @GroupMessageHandler
    public boolean deleteComment(BaniraBot bot, GroupMessageEvent event) {
        if (!bot.isAdmin(event.getGroupId(), event.getSender().getUserId())) {
            return false;
        }

        String message = event.getMessage();
        String[] split = message.split("\\s+");
        if (split.length < 3) {
            return false;
        }

        // 检查是否是mcmod命令
        if (insConfig.get().mcMod().stream().noneMatch(ins -> ("/" + ins).equals(split[0]))) {
            return false;
        }

        // 检查是否是删除评论命令
        if (!DEL_COMMENT_INS.contains(split[1])) {
            return false;
        }

        bot.setMsgEmojiLikeOk(event.getMessageId());

        // 获取评论ID
        String commentId = split[2];
        if (StringUtils.isNullOrEmptyEx(commentId) || !commentId.matches("\\d+")) {
            LOGGER.error("Failed to get comment id");
            return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
        }

        // 删除评论
        McModCommentResponse response = McModUtils.deleteComment(event.getGroupId(), commentId);
        if (response != null && response.isSuccess()) {
            // 从缓存中移除该评论
            McModCommentService.COMMENT_CACHE.values().forEach(comments ->
                    comments.removeIf(comment -> comment.getId().equals(commentId)
                            || (comment.getReplies() != null && comment.getReplies().stream()
                            .anyMatch(reply -> reply.getId().equals(commentId))))
            );
            return bot.setMsgEmojiLikeHeart(event.getMessageId());
        }
        Integer state = response != null ? response.getState() : null;
        LOGGER.error("Failed to delete comment, state: {}", state);
        sendMcModFailure(bot, event, "删除评论", state);
        return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
    }


    @GroupMessageHandler
    public boolean config(BaniraBot bot, GroupMessageEvent event) {
        BaniraCodeContext context = new BaniraCodeContext(bot, event);

        if (!super.isCommand(context)) {
            return false;
        }

        String message = super.deleteCommandPrefix(context);
        message = message.trim();

        String[] split = message.split("\\s+");
        if (split.length < 1) {
            return false;
        }

        String firstWord = split[0];
        if (insConfig.get().mcMod().stream().noneMatch(firstWord::equalsIgnoreCase)) {
            return false;
        }

        Integer msgId = event.getMessageId();
        if (split.length < 2) {
            return bot.setMsgEmojiLikeBrokenHeart(msgId);
        }

        if (!bot.isAdmin(event.getGroupId(), event.getSender().getUserId())) {
            return bot.setMsgEmojiLikeNo(msgId);
        }
        bot.setMsgEmojiLikeOk(msgId);

        BaseInstructionsConfig baseIns = BaniraUtils.getBaseIns();
        String operate = split[1];
        Long groupId = event.getGroupId();

        // 添加
        if (baseIns.add().contains(operate)) {
            if (split.length < 4) {
                return bot.setMsgEmojiLikeBrokenHeart(msgId);
            }
            String typeStr = split[2];
            String containerId = split[3];
            new Thread(() -> handleAdd(bot, groupId, msgId, typeStr, containerId)).start();
            return true;
        }
        // 删除
        else if (baseIns.del().contains(operate)) {
            if (split.length < 4) {
                return bot.setMsgEmojiLikeBrokenHeart(msgId);
            }
            String typeStr = split[2];
            String containerId = split[3];
            return handleDelete(bot, groupId, msgId, typeStr, containerId);
        }
        // 查询
        else if (baseIns.list().contains(operate)) {
            return handleList(bot, groupId, msgId);
        }
        // 启用
        else if (baseIns.enable().contains(operate)) {
            BaniraUtils.getGroupConfigForEdit(McModGroupConfig.class, groupId).mcModCommentConfig().enable(true);
            BaniraUtils.saveGroupConfig();
            return bot.setMsgEmojiLikeHeart(msgId);
        }
        // 禁用
        else if (baseIns.disable().contains(operate)) {
            BaniraUtils.getGroupConfigForEdit(McModGroupConfig.class, groupId).mcModCommentConfig().enable(false);
            BaniraUtils.saveGroupConfig();
            return bot.setMsgEmojiLikeHeart(msgId);
        }
        // 未知操作
        else {
            return bot.setMsgEmojiLikeBrokenHeart(msgId);
        }
    }

    /**
     * 解析评论类型
     */
    private EnumContentType parseCommentType(String typeStr) {
        if (typeStr == null) {
            return null;
        }
        if (MOD_INS.contains(typeStr)) {
            return EnumContentType.MOD;
        } else if (MOD_PACK_INS.contains(typeStr)) {
            return EnumContentType.MODPACK;
        } else if (AUTHOR_INS.contains(typeStr)) {
            return EnumContentType.AUTHOR;
        } else if (USER_INS.contains(typeStr)) {
            return EnumContentType.USER_CENTER;
        } else {
            return null;
        }
    }

    /**
     * 处理添加检测目标
     */
    private boolean handleAdd(BaniraBot bot, Long groupId, int msgId, String typeStr, String containerId) {
        if (StringUtils.isNullOrEmptyEx(containerId) || !containerId.matches("\\d+")) {
            return bot.setMsgEmojiLikeBrokenHeart(msgId);
        }

        // 解析评论类型
        EnumContentType commentType = parseCommentType(typeStr);
        if (commentType == null) {
            return bot.setMsgEmojiLikeBrokenHeart(msgId);
        }

        try {
            McModGroupConfig otherConfig = BaniraUtils.getGroupConfigForEdit(McModGroupConfig.class, groupId);
            McModCommentConfig config = otherConfig.mcModCommentConfig();
            if (config == null) {
                config = new McModCommentConfig();
                otherConfig.mcModCommentConfig(config);
            }

            if (config.modWatchMap() == null) {
                config.modWatchMap(new LinkedHashMap<>());
            }

            // 检查是否已经存在该容器的监控
            if (config.isWatching(commentType, containerId, groupId)) {
                return bot.setMsgEmojiLikeBrokenHeart(msgId);
            }

            // 添加监控配置，记录群号和botId
            Long botId = bot.getSelfId();
            config.addModWatch(commentType, containerId, groupId, botId);
            BaniraUtils.saveGroupConfig();

            // 首次添加记录则获取全量评论列表
            mcModCommentService.initializeCacheIfAbsent(commentType, containerId);

            // 重新调度任务
            mcModCommentScheduler.scheduleTask();

            return bot.setMsgEmojiLikeHeart(msgId);
        } catch (Exception e) {
            LOGGER.error("Error adding container to watch list", e);
            return bot.setMsgEmojiLikeBrokenHeart(msgId);
        }
    }

    /**
     * 处理删除检测目标
     */
    private boolean handleDelete(BaniraBot bot, Long groupId, int msgId, String typeStr, String containerId) {
        if (StringUtils.isNullOrEmptyEx(containerId) || !containerId.matches("\\d+")) {
            return bot.setMsgEmojiLikeBrokenHeart(msgId);
        }

        // 解析评论类型
        EnumContentType commentType = parseCommentType(typeStr);
        if (commentType == null) {
            return bot.setMsgEmojiLikeBrokenHeart(msgId);
        }

        try {
            McModGroupConfig otherConfig = BaniraUtils.getGroupConfigForEdit(McModGroupConfig.class, groupId);
            McModCommentConfig config = otherConfig.mcModCommentConfig();
            if (config == null || !config.isWatching(commentType, containerId, groupId)) {
                return bot.setMsgEmojiLikeBrokenHeart(msgId);
            }

            // 删除该群对该容器的监控
            config.removeModWatch(commentType, containerId, groupId);
            BaniraUtils.saveGroupConfig();

            // 重新调度任务
            mcModCommentScheduler.scheduleTask();

            return bot.setMsgEmojiLikeHeart(msgId);
        } catch (Exception e) {
            LOGGER.error("Error removing container from watch list", e);
            return bot.setMsgEmojiLikeBrokenHeart(msgId);
        }
    }

    /**
     * 处理查看列表
     */
    private boolean handleList(BaniraBot bot, Long groupId, int msgId) {
        try {
            McModGroupConfig otherConfig = BaniraUtils.getGroupConfigOrGlobal(McModGroupConfig.class, groupId);
            McModCommentConfig config = otherConfig.mcModCommentConfig();
            if (config == null || config.modWatchMap() == null || config.modWatchMap().isEmpty()) {
                return bot.setMsgEmojiLikeBrokenHeart(msgId);
            }

            // 筛选出当前群的监控目标
            List<ModWatchInfo> watchInfos = config.modWatchMap().values().stream()
                    .flatMap(List::stream)
                    .filter(info -> info.groupId().equals(groupId))
                    .toList();

            if (watchInfos.isEmpty()) {
                return bot.setMsgEmojiLikeBrokenHeart(msgId);
            }

            StringBuilder sb = new StringBuilder();
            sb.append("当前群的检测目标列表：\n");
            for (int i = 0; i < watchInfos.size(); i++) {
                ModWatchInfo info = watchInfos.get(i);
                String typeName = McModCommentService.getCommentTypeName(info.commentType());
                sb.append(i + 1).append(". ").append(typeName).append(" - ").append(info.containerId());

                // 根据类型添加链接
                if (info.commentType() == EnumContentType.MOD) {
                    sb.append(McModUtils.getModUrl(info.containerId()));
                } else if (info.commentType() == EnumContentType.MODPACK) {
                    sb.append(McModUtils.getModpackUrl(info.containerId()));
                } else if (info.commentType() == EnumContentType.AUTHOR) {
                    sb.append(McModUtils.getAuthorUrl(info.containerId()));
                } else if (info.commentType() == EnumContentType.USER_CENTER) {
                    sb.append(McModUtils.getUserCenterUrl(info.containerId()));
                }
                sb.append("\n");
            }

            ActionData<MsgId> msgDataId = bot.sendGroupMsg(groupId, sb.toString(), false);
            return bot.isActionDataMsgIdNotEmpty(msgDataId)
                    ? bot.setMsgEmojiLikeHeart(msgId)
                    : bot.setMsgEmojiLikeBrokenHeart(msgId);
        } catch (Exception e) {
            LOGGER.error("Error listing containers", e);
            return bot.setMsgEmojiLikeBrokenHeart(msgId);
        }
    }

}
