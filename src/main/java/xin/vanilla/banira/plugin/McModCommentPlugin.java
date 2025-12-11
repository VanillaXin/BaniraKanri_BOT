package xin.vanilla.banira.plugin;

import com.mikuac.shiro.annotation.GroupMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.dto.action.common.ActionData;
import com.mikuac.shiro.dto.action.common.MsgId;
import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.GroupConfig;
import xin.vanilla.banira.config.entity.basic.BaseInstructionsConfig;
import xin.vanilla.banira.config.entity.basic.OtherConfig;
import xin.vanilla.banira.config.entity.extended.McModCommentConfig;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.plugin.mcmodcomment.McModCommentScheduler;
import xin.vanilla.banira.plugin.mcmodcomment.McModCommentService;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.StringUtils;
import xin.vanilla.banira.util.mcmod.McModCommentRow;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.Supplier;

/**
 * MCMod评论监控插件
 */
@Slf4j
@Shiro
@Component
public class McModCommentPlugin extends BasePlugin {

    @Resource
    private McModCommentService commentService;

    @Resource
    private McModCommentScheduler mcModCommentScheduler;

    @Resource
    private Supplier<GroupConfig> groupConfig;

    @Nonnull
    @Override
    public List<String> getHelpInfo(@Nullable Long groupId, @Nonnull String... types) {
        List<String> result = new ArrayList<>();
        String type = types.length > 0 ? types[0] : "";
        List<String> command = insConfig.get().mcModComment();
        if (command.stream().anyMatch(type::equalsIgnoreCase) || types.length == 0) {
            BaseInstructionsConfig baseIns = BaniraUtils.getBaseIns();
            result.add("MCMod评论监控插件：\n" +
                    "检测MC百科评论变化并进行提示。" + "\n\n" +
                    "添加：\n" +
                    BaniraUtils.getInsPrefixWithSpace() +
                    command + " " +
                    baseIns.add() + " " +
                    "<mod编号>" + "\n\n" +
                    "删除：\n" +
                    BaniraUtils.getInsPrefixWithSpace() +
                    command + " " +
                    baseIns.del() + " " +
                    "<mod编号>" + "\n\n" +
                    "查询：\n" +
                    BaniraUtils.getInsPrefixWithSpace() +
                    command + " " +
                    baseIns.list()
            );
        }
        return result;
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
        if (insConfig.get().mcModComment().stream().noneMatch(firstWord::equalsIgnoreCase)) {
            return false;
        }

        Integer msgId = event.getMessageId();
        if (split.length < 2) {
            return bot.setMsgEmojiLikeBrokenHeart(msgId);
        }

        if (!bot.isAdmin(event.getGroupId(), event.getSender().getUserId())) {
            return bot.setMsgEmojiLikeNo(msgId);
        }

        BaseInstructionsConfig baseIns = BaniraUtils.getBaseIns();
        String operate = split[1];
        Long groupId = event.getGroupId();

        // 添加
        if (baseIns.add().contains(operate)) {
            if (split.length < 3) {
                return bot.setMsgEmojiLikeBrokenHeart(msgId);
            }
            String modId = split[2];
            return handleAdd(bot, groupId, msgId, modId);
        }
        // 删除
        else if (baseIns.del().contains(operate)) {
            if (split.length < 3) {
                return bot.setMsgEmojiLikeBrokenHeart(msgId);
            }
            String modId = split[2];
            return handleDelete(bot, groupId, msgId, modId);
        }
        // 查询
        else if (baseIns.list().contains(operate)) {
            return handleList(bot, groupId, msgId);
        }
        // 未知操作
        else {
            return bot.setMsgEmojiLikeBrokenHeart(msgId);
        }
    }

    /**
     * 处理添加检测目标
     */
    private boolean handleAdd(BaniraBot bot, Long groupId, int msgId, String modId) {
        if (StringUtils.isNullOrEmptyEx(modId) || !modId.matches("\\d+")) {
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
                config.modWatchMap(new java.util.LinkedHashMap<>());
            }

            // 检查是否已经存在该mod的监控
            if (config.isWatching(modId, groupId)) {
                return bot.setMsgEmojiLikeBrokenHeart(msgId);
            }

            // 添加监控配置，记录群号和botId
            Long botId = bot.getSelfId();
            config.addModWatch(modId, groupId, botId);
            groupConfig.get().otherConfig().put(groupId, otherConfig);
            BaniraUtils.saveGroupConfig();

            // 尝试从文件加载缓存，如果文件不存在则初始化缓存（首次添加时）
            // 注意：初始化缓存后，首次定时任务执行时不会发送通知（因为缓存已存在）
            commentService.loadCacheFromFile(modId);
            if (!commentService.isCacheInitialized(modId)) {
                List<McModCommentRow> comments = commentService.fetchComments(modId);
                if (comments != null && !comments.isEmpty()) {
                    commentService.initCache(modId, comments);
                    LOGGER.info("Initialized cache for mod {} with {} comments", modId, comments.size());
                }
            }

            // 重新调度任务
            mcModCommentScheduler.rescheduleTask();

            return bot.setMsgEmojiLikeHeart(msgId);
        } catch (Exception e) {
            LOGGER.error("Error adding mod to watch list", e);
            return bot.setMsgEmojiLikeBrokenHeart(msgId);
        }
    }

    /**
     * 处理删除检测目标
     */
    private boolean handleDelete(BaniraBot bot, Long groupId, int msgId, String modId) {
        if (StringUtils.isNullOrEmptyEx(modId) || !modId.matches("\\d+")) {
            return bot.setMsgEmojiLikeBrokenHeart(msgId);
        }

        try {
            OtherConfig otherConfig = BaniraUtils.getOthersConfig(groupId);
            McModCommentConfig config = otherConfig.mcModCommentConfig();
            if (config == null || !config.isWatching(modId, groupId)) {
                return bot.setMsgEmojiLikeBrokenHeart(msgId);
            }

            // 删除该群对该mod的监控
            config.removeModWatch(modId, groupId);
            groupConfig.get().otherConfig().put(groupId, otherConfig);
            BaniraUtils.saveGroupConfig();

            // 重新调度任务
            mcModCommentScheduler.rescheduleTask();

            return bot.setMsgEmojiLikeHeart(msgId);
        } catch (Exception e) {
            LOGGER.error("Error removing mod from watch list", e);
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
            List<String> modIds = config.modWatchMap().entrySet().stream()
                    .filter(entry -> entry.getValue().stream()
                            .anyMatch(info -> info.groupId().equals(groupId)))
                    .map(Map.Entry::getKey)
                    .toList();

            if (modIds.isEmpty()) {
                return bot.setMsgEmojiLikeBrokenHeart(msgId);
            }

            StringBuilder sb = new StringBuilder();
            sb.append("当前群的检测目标列表：\n");
            for (int i = 0; i < modIds.size(); i++) {
                String modId = modIds.get(i);
                sb.append(i + 1).append(". ").append(modId);
                sb.append(" (https://www.mcmod.cn/class/").append(modId).append(".html)\n");
            }

            ActionData<MsgId> msgDataId = bot.sendGroupMsg(groupId, sb.toString(), false);
            return bot.isActionDataMsgIdNotEmpty(msgDataId);
        } catch (Exception e) {
            LOGGER.error("Error listing mods", e);
            return bot.setMsgEmojiLikeBrokenHeart(msgId);
        }
    }

}

