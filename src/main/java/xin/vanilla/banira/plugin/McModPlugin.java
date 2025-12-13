package xin.vanilla.banira.plugin;

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
import xin.vanilla.banira.config.entity.GroupConfig;
import xin.vanilla.banira.config.entity.basic.BaseInstructionsConfig;
import xin.vanilla.banira.config.entity.basic.OtherConfig;
import xin.vanilla.banira.config.entity.extended.McModCommentConfig;
import xin.vanilla.banira.config.entity.extended.ModWatchInfo;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.plugin.mcmod.McModCommentScheduler;
import xin.vanilla.banira.plugin.mcmod.McModCommentService;
import xin.vanilla.banira.service.impl.MessageRecordManager;
import xin.vanilla.banira.util.*;
import xin.vanilla.banira.util.mcmod.*;

import java.util.*;
import java.util.function.Supplier;

/**
 * MCMod百科插件
 */
@Slf4j
@Shiro
@Component
public class McModPlugin extends BasePlugin {

    @Resource
    private MessageRecordManager messageRecordManager;
    @Resource
    private Supplier<GroupConfig> groupConfig;

    @Resource
    private McModCommentScheduler mcModCommentScheduler;

    private static final Set<String> MOD_INS = BaniraUtils.mutableSetOf("mod", "模组");
    private static final Set<String> MOD_PACK_INS = BaniraUtils.mutableSetOf("modpack", "pack", "整合包");
    private static final Set<String> AUTHOR_INS = BaniraUtils.mutableSetOf("author", "作者");
    private static final Set<String> USER_INS = BaniraUtils.mutableSetOf("user", "center", "用户");
    private static final Set<String> ITEM_INS = BaniraUtils.mutableSetOf("item", "data", "资料", "物品");
    private static final Set<String> TUTORIAL_INS = BaniraUtils.mutableSetOf("tutorial", "教程");
    private static final Set<String> MOD_RANDOM_INS = BaniraUtils.mutableSetOf("random", "随便看看");
    private static final Set<String> USER_CARD_INS = BaniraUtils.mutableSetOf("card", "用户卡片");
    private static final Set<String> COMMENT_INS = BaniraUtils.mutableSetOf("comment", "发布评论", "评论");
    private static final Set<String> DEL_COMMENT_INS = BaniraUtils.mutableSetOf("delcomment", "删除评论");
    private static final Set<String> COMMENT_REPLY_INS = BaniraUtils.mutableSetOf("reply", "回复");


    @Nonnull
    @Override
    public List<String> getHelpInfo(@Nullable Long groupId, @Nonnull String... types) {
        List<String> result = new ArrayList<>();
        String type = types.length > 0 ? types[0] : "";
        List<String> command = insConfig.get().mcMod();
        if (command.stream().anyMatch(type::equalsIgnoreCase) || types.length == 0) {
            BaseInstructionsConfig baseIns = BaniraUtils.getBaseIns();
            result.add("MC百科插件 - 检索：\n" +
                    "提供简单的百科信息搜索。" + "\n\n" +
                    "搜索模组：\n" +
                    "/" +
                    command + " " +
                    MOD_INS + " " +
                    "<关键词>" + "\n\n" +
                    "搜索整合包：\n" +
                    "/" +
                    command + " " +
                    MOD_PACK_INS + " " +
                    "<关键词>" + "\n\n" +
                    "搜索作者：\n" +
                    "/" +
                    command + " " +
                    AUTHOR_INS + " " +
                    "<关键词>"
            );
            result.add("MC百科插件 - 随便看看：\n" +
                    "随机显示MOD。" + "\n\n" +
                    "/" +
                    command + " " +
                    MOD_RANDOM_INS
            );
            result.add("MC百科插件 - 用户卡片：\n" +
                    "展示用户卡片信息。" + "\n\n" +
                    "/" +
                    command + " " +
                    USER_CARD_INS + " " +
                    "<用户ID>"
            );
            Set<String> commentTypes = new HashSet<>();
            commentTypes.addAll(MOD_INS);
            commentTypes.addAll(MOD_PACK_INS);
            commentTypes.addAll(AUTHOR_INS);
            commentTypes.addAll(USER_INS);
            result.add("MC百科插件 - 评论检测：\n" +
                    "检测MC百科评论变化并进行提示。" + "\n\n" +
                    "添加评论检测：\n" +
                    BaniraUtils.getInsPrefixWithSpace() +
                    command + " " +
                    baseIns.add() + " " +
                    commentTypes + " " +
                    "<容器ID>" + "\n\n" +
                    "删除评论检测：\n" +
                    BaniraUtils.getInsPrefixWithSpace() +
                    command + " " +
                    baseIns.del() + " " +
                    commentTypes + " " +
                    "<容器ID>" + "\n\n" +
                    "查询评论检测：\n" +
                    BaniraUtils.getInsPrefixWithSpace() +
                    command + " " +
                    baseIns.list() + "\n\n" +
                    "启用评论检测：\n" +
                    BaniraUtils.getInsPrefixWithSpace() +
                    command + " " +
                    baseIns.enable() + "\n\n" +
                    "禁用评论检测：\n" +
                    BaniraUtils.getInsPrefixWithSpace() +
                    command + " " +
                    baseIns.disable()
            );
        }
        return result;
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

        // 搜索模组
        if (MOD_INS.contains(ins)) {
            if (split.length < 3) {
                return bot.setMsgEmojiLikeBrokenHeart(msgId);
            }
            if (split.length > 3 && split[2].equals("-a")) {
                String keyword = String.join(" ", Arrays.copyOfRange(split, 3, split.length));
                List<McModSearchResult> results = McModUtils.searchModBySearchPage(keyword);
                return handleSearchResults(bot, event, results, "模组", groupId, msgId);
            } else {
                String keyword = String.join(" ", Arrays.copyOfRange(split, 2, split.length));
                List<McModContent> results = McModUtils.searchMod(keyword);
                return handleSearchContents(bot, event, results, "模组", groupId, msgId);
            }
        }
        // 搜索整合包
        else if (MOD_PACK_INS.contains(ins)) {
            if (split.length < 3) {
                return bot.setMsgEmojiLikeBrokenHeart(msgId);
            }
            if (split.length > 3 && split[2].equals("-a")) {
                String keyword = String.join(" ", Arrays.copyOfRange(split, 3, split.length));
                List<McModSearchResult> results = McModUtils.searchModpackBySearchPage(keyword);
                return handleSearchResults(bot, event, results, "整合包", groupId, msgId);
            } else {
                String keyword = String.join(" ", Arrays.copyOfRange(split, 2, split.length));
                List<McModContent> results = McModUtils.searchModpack(keyword);
                return handleSearchContents(bot, event, results, "整合包", groupId, msgId);
            }
        }
        // 搜索作者
        else if (AUTHOR_INS.contains(ins)) {
            if (split.length < 3) {
                return bot.setMsgEmojiLikeBrokenHeart(msgId);
            }
            if (split.length > 3 && split[2].equals("-a")) {
                String keyword = String.join(" ", Arrays.copyOfRange(split, 3, split.length));
                List<McModSearchResult> results = McModUtils.searchAuthorBySearchPage(keyword);
                return handleSearchResults(bot, event, results, "作者", groupId, msgId);
            } else {
                String keyword = String.join(" ", Arrays.copyOfRange(split, 2, split.length));
                List<McModContent> results = McModUtils.searchAuthor(keyword);
                return handleSearchContents(bot, event, results, "作者", groupId, msgId);
            }
        }
        // 搜索用户
        else if (USER_INS.contains(ins)) {
            if (split.length < 3) {
                return bot.setMsgEmojiLikeBrokenHeart(msgId);
            }
            String keyword = String.join(" ", Arrays.copyOfRange(split, 2, split.length));
            List<McModSearchResult> results = McModUtils.searchUserBySearchPage(keyword);
            return handleSearchResults(bot, event, results, "用户", groupId, msgId);
        }
        // 搜索资料
        else if (ITEM_INS.contains(ins)) {
            if (split.length < 3) {
                return bot.setMsgEmojiLikeBrokenHeart(msgId);
            }
            String keyword = String.join(" ", Arrays.copyOfRange(split, 2, split.length));
            List<McModSearchResult> results = McModUtils.searchDataBySearchPage(keyword);
            return handleSearchResults(bot, event, results, "资料", groupId, msgId);
        }
        // 搜索教程
        else if (TUTORIAL_INS.contains(ins)) {
            if (split.length < 3) {
                return bot.setMsgEmojiLikeBrokenHeart(msgId);
            }
            String keyword = String.join(" ", Arrays.copyOfRange(split, 2, split.length));
            List<McModSearchResult> results = McModUtils.searchTutorialBySearchPage(keyword);
            return handleSearchResults(bot, event, results, "教程", groupId, msgId);
        }
        // 随便看看
        else if (MOD_RANDOM_INS.contains(ins)) {
            List<McModContent> randomMods = McModUtils.getRandomMods();
            if (CollectionUtils.isNullOrEmpty(randomMods)) {
                String msg = "未找到相关信息，可能是被百科娘吃掉了";
                if (BaniraUtils.isGroupIdValid(groupId)) {
                    bot.sendGroupMsg(groupId, msg, false);
                } else {
                    bot.sendPrivateMsg(event.getUserId(), msg, false);
                }
                return bot.setMsgEmojiLikeBrokenHeart(msgId);
            } else {
                LoginInfoResp loginInfoEx = bot.getLoginInfoEx();
                List<Map<String, Object>> forwardMsg = new ArrayList<>();
                forwardMsg.add(com.mikuac.shiro.common.utils.ShiroUtils.generateSingleMsg(
                        event.getUserId(),
                        event.getSender().getNickname(),
                        event.getMessage()
                ));
                for (McModContent mod : randomMods) {
                    String msg = MsgUtils.builder()
                            .img(mod.getCoverImageUrl())
                            .text(mod.getFormattedName()).text("\n")
                            .text("编号: " + mod.getId()).text("\n")
                            .text("链接: " + mod.getDetailUrl())
                            .build();
                    forwardMsg.add(com.mikuac.shiro.common.utils.ShiroUtils.generateSingleMsg(
                            loginInfoEx.getUserId(),
                            loginInfoEx.getNickname(),
                            msg
                    ));
                }
                if (BaniraUtils.isGroupIdValid(groupId)) {
                    bot.sendGroupForwardMsg(groupId, forwardMsg);
                } else {
                    bot.sendPrivateForwardMsg(event.getUserId(), forwardMsg);
                }
                return bot.setMsgEmojiLikeOk(msgId);
            }
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
            String result = "【用户卡片】\n" +
                    "用户名: " + userCard.getUsername() + "\n" +
                    "用户ID: " + userId + "\n" +
                    "等级: " + (userCard.getRank() != null ? userCard.getRank() : "未知") + "\n" +
                    "在线状态: " + (userCard.getOnline() != null ?
                    (userCard.getOnline() == 1 ? "在线" : userCard.getOnline() == 0 ? "离线" : "隐身") : "未知");
            if (StringUtils.isNotNullOrEmpty(userCard.getSign())) {
                result += "\n签名: " + userCard.getSign();
            }
            if (BaniraUtils.isGroupIdValid(groupId)) {
                bot.sendGroupMsg(groupId, result, false);
            } else {
                bot.sendPrivateMsg(event.getUserId(), result, false);
            }
            return bot.setMsgEmojiLikeOk(msgId);
        } else {
            return bot.setMsgEmojiLikeBrokenHeart(msgId);
        }
    }

    /**
     * 处理搜索结果
     */
    private boolean handleSearchResults(BaniraBot bot, AnyMessageEvent event, List<McModSearchResult> results,
                                        String type, Long groupId, int msgId) {
        if (CollectionUtils.isNullOrEmpty(results)) {
            String msg = "未找到相关" + type + "信息，可能是被百科娘吃掉了";
            if (BaniraUtils.isGroupIdValid(groupId)) {
                bot.sendGroupMsg(groupId, msg, false);
            } else {
                bot.sendPrivateMsg(event.getUserId(), msg, false);
            }
            return bot.setMsgEmojiLikeBrokenHeart(msgId);
        }

        // 如果结果只有一条，直接发送
        if (results.size() == 1) {
            McModSearchResult result = results.getFirst();
            String msg = buildSearchResultMessage(result);
            if (BaniraUtils.isGroupIdValid(groupId)) {
                bot.sendGroupMsg(groupId, msg, false);
            } else {
                bot.sendPrivateMsg(event.getUserId(), msg, false);
            }
            return bot.setMsgEmojiLikeOk(msgId);
        }

        // 多条结果，使用合并转发
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

        // 限制最多显示20条
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

        if (BaniraUtils.isGroupIdValid(groupId)) {
            bot.sendGroupForwardMsg(groupId, forwardMsg);
        } else {
            bot.sendPrivateForwardMsg(event.getUserId(), forwardMsg);
        }
        return bot.setMsgEmojiLikeOk(msgId);
    }

    /**
     * 构建搜索结果消息
     */
    private String buildSearchResultMessage(McModSearchResult result) {
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

    /**
     * 处理搜索结果
     */
    private boolean handleSearchContents(BaniraBot bot, AnyMessageEvent event, List<McModContent> results,
                                         String type, Long groupId, int msgId) {
        if (CollectionUtils.isNullOrEmpty(results)) {
            String msg = "未找到相关" + type + "信息，可能是被百科娘吃掉了";
            if (BaniraUtils.isGroupIdValid(groupId)) {
                bot.sendGroupMsg(groupId, msg, false);
            } else {
                bot.sendPrivateMsg(event.getUserId(), msg, false);
            }
            return bot.setMsgEmojiLikeBrokenHeart(msgId);
        }

        // 如果结果只有一条，直接发送
        if (results.size() == 1) {
            McModContent result = results.getFirst();
            String msg = MsgUtils.builder()
                    .text(result.getFormattedName()).text("\n")
                    .text("编号: " + result.getId()).text("\n")
                    .text("链接: " + result.getDetailUrl())
                    .build();
            if (BaniraUtils.isGroupIdValid(groupId)) {
                bot.sendGroupMsg(groupId, msg, false);
            } else {
                bot.sendPrivateMsg(event.getUserId(), msg, false);
            }
            return bot.setMsgEmojiLikeOk(msgId);
        }

        // 多条结果，使用合并转发
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

        // 限制最多显示20条
        int maxResults = Math.min(results.size(), 20);
        for (int i = 0; i < maxResults; i++) {
            McModContent result = results.get(i);
            String msg = MsgUtils.builder()
                    .text(result.getFormattedName()).text("\n")
                    .text("编号: " + result.getId()).text("\n")
                    .text("链接: " + result.getDetailUrl())
                    .build();
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

        if (BaniraUtils.isGroupIdValid(groupId)) {
            bot.sendGroupForwardMsg(groupId, forwardMsg);
        } else {
            bot.sendPrivateForwardMsg(event.getUserId(), forwardMsg);
        }
        return bot.setMsgEmojiLikeOk(msgId);
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
            // 从消息中提取信息
            // 格式：类型: MOD\n容器: 123456\n编号: 789012\n...
            String[] lines = message.split("\n");
            String commentTypeStr = null;
            String containerId = null;
            String commentId = null;

            // 遍历前5行
            for (int i = 0; i < lines.length && i < 5; i++) {
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

            // 解析评论类型
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
            BaniraUtils.getOthersConfig(groupId).mcModCommentConfig().enable(true);
            return bot.setMsgEmojiLikeHeart(msgId);
        }
        // 禁用
        else if (baseIns.disable().contains(operate)) {
            BaniraUtils.getOthersConfig(groupId).mcModCommentConfig().enable(false);
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
            OtherConfig otherConfig = BaniraUtils.getOthersConfig(groupId);
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
            groupConfig.get().otherConfig().put(groupId, otherConfig);
            BaniraUtils.saveGroupConfig();

            // 首次添加记录则获取全量评论列表
            String cacheKey = McModCommentService.getCacheKey(commentType, containerId);
            if (!McModCommentService.COMMENT_CACHE.containsKey(cacheKey)) {
                // 拉取全量评论
                Map<String, McModCommentRow> commentMap = new HashMap<>();

                McModCommentResult comments = McModUtils.getComments(commentType, containerId, 1);
                if (comments != null) {
                    for (McModCommentRow comment : comments.getRow()) {
                        comment.setCommentType(commentType);
                        comment.setContainerId(containerId);
                        String key = McModCommentService.getCommentKey(commentType, containerId, comment.getId());
                        commentMap.put(key, comment);
                    }
                    McModCommentResult curent = comments;
                    while (curent != null && curent.getPage() != null && curent.getPage().getNext() != null) {
                        // 休眠2秒
                        Thread.sleep(2000);
                        curent = McModUtils.getComments(commentType, containerId, curent.getPage().getNext());
                        if (curent != null) {
                            for (McModCommentRow comment : curent.getRow()) {
                                comment.setCommentType(commentType);
                                comment.setContainerId(containerId);
                                String key = McModCommentService.getCommentKey(commentType, containerId, comment.getId());
                                commentMap.put(key, comment); // 新内容覆盖旧内容
                            }
                        }
                    }
                }
                // 拉取全量回复
                List<McModCommentRow> commentList = new ArrayList<>(commentMap.values());
                for (McModCommentRow commentRow : commentList) {
                    McModCommentResult replies = McModUtils.getCommentReplies(commentRow.getId(), 1);
                    if (replies != null) {
                        for (McModCommentRow reply : replies.getRow()) {
                            reply.setCommentType(commentType);
                            reply.setContainerId(containerId);
                            reply.setParentId(commentRow.getId());
                            String key = McModCommentService.getCommentKey(commentType, containerId, reply.getId());
                            commentMap.put(key, reply);
                        }
                        McModCommentResult curent = replies;
                        while (curent != null && curent.getPage() != null && curent.getPage().getNext() != null) {
                            // 休眠2秒
                            Thread.sleep(2000);
                            curent = McModUtils.getCommentReplies(commentRow.getId(), curent.getPage().getNext());
                            if (curent != null) {
                                for (McModCommentRow reply : curent.getRow()) {
                                    reply.setCommentType(commentType);
                                    reply.setContainerId(containerId);
                                    reply.setParentId(commentRow.getId());
                                    String key = McModCommentService.getCommentKey(commentType, containerId, reply.getId());
                                    commentMap.put(key, reply);
                                }
                            }
                        }
                    }
                }
                Set<McModCommentRow> cachedComment = new HashSet<>(commentMap.values());
                McModCommentService.COMMENT_CACHE.put(cacheKey, cachedComment);
                McModCommentService.saveCacheToFile(cacheKey, cachedComment);
            }

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
            OtherConfig otherConfig = BaniraUtils.getOthersConfig(groupId);
            McModCommentConfig config = otherConfig.mcModCommentConfig();
            if (config == null || !config.isWatching(commentType, containerId, groupId)) {
                return bot.setMsgEmojiLikeBrokenHeart(msgId);
            }

            // 删除该群对该容器的监控
            config.removeModWatch(commentType, containerId, groupId);
            groupConfig.get().otherConfig().put(groupId, otherConfig);
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
            OtherConfig otherConfig = BaniraUtils.getOthersConfig(groupId);
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
                String typeName = getCommentTypeName(info.commentType());
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
            return bot.isActionDataMsgIdNotEmpty(msgDataId);
        } catch (Exception e) {
            LOGGER.error("Error listing containers", e);
            return bot.setMsgEmojiLikeBrokenHeart(msgId);
        }
    }

    /**
     * 获取评论类型名称
     */
    private String getCommentTypeName(EnumContentType commentType) {
        return switch (commentType) {
            case MOD -> "模组";
            case MODPACK -> "整合包";
            case AUTHOR -> "作者";
            case USER_CENTER -> "用户中心";
        };
    }

}
