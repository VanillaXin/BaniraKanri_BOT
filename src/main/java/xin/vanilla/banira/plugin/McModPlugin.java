package xin.vanilla.banira.plugin;

import com.google.gson.JsonObject;
import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.GroupMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.common.utils.MsgUtils;
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
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.plugin.help.HelpTopic;
import xin.vanilla.banira.plugin.help.HelpTopics;
import xin.vanilla.banira.plugin.mcmod.McModCommentScheduler;
import xin.vanilla.banira.plugin.mcmod.McModCommentService;
import xin.vanilla.banira.plugin.mcmod.McModSearchListSession;
import xin.vanilla.banira.plugin.mcmod.McModSearchListSource;
import xin.vanilla.banira.plugin.mcmod.McModSearchListStore;
import xin.vanilla.banira.service.IMessageRecordManager;
import xin.vanilla.banira.util.*;
import xin.vanilla.banira.util.mcmod.*;

import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;

/**
 * MCMod百科插件
 */
@Slf4j
@Shiro
@Component
public class McModPlugin extends BasePlugin {

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
        LOGGER.error("Failed to push {}, id: {}", target.label(), targetId);
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
            sendMessage(bot, event, groupId, "操作频繁，请稍后再试");
            return bot.setMsgEmojiLikeBrokenHeart(msgId);
        }
        if (result != null && result.isFromCache()) {
            sendMessage(bot, event, groupId, "请勿重复投票");
            return bot.setMsgEmojiLikeBrokenHeart(msgId);
        }
        if (result != null && result.isSuccess()) {
            return bot.setMsgEmojiLikeHeart(msgId);
        }
        LOGGER.error("Failed to vote {}, id: {}, type: {}", target.label(), targetId, voteType);
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
        String imageMsg = McModRenderHelper.renderSearchResult(result, typeName);
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
        String imageMsg = McModRenderHelper.renderContent(content, typeName);
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
        LOGGER.error("Failed to post comment");
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
        } else {
            LOGGER.error("Failed to reply comment");
            return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
        }
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
        } else {
            LOGGER.error("Failed to delete comment");
            return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
        }
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
