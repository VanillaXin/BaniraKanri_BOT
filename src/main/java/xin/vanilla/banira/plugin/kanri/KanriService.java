package xin.vanilla.banira.plugin.kanri;

import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.enums.EnumMessageType;
import xin.vanilla.banira.plugin.chat.agent.AgentContext;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.help.HelpTopic;
import xin.vanilla.banira.util.StringUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 群管指令 AI 执行服务（不含踢人）
 */
@Slf4j
@Service
public class KanriService {

    @Resource
    private KanriHandlerRegistry handlerRegistry;

    @Nonnull
    public String listAiActions() {
        Map<String, KanriHandler> handlers = handlerRegistry.aiHandlers();
        if (handlers.isEmpty()) {
            return "当前没有可用的群管 AI 能力。";
        }
        return handlers.values().stream()
                .distinct()
                .sorted((a, b) -> a.getHelpSubTopic().name().compareTo(b.getHelpSubTopic().name()))
                .map(this::formatHandler)
                .collect(Collectors.joining("\n"));
    }

    @Nonnull
    public String execute(@Nonnull AgentContext agentContext, @Nonnull String action, @Nonnull String[] args) {
        return executeReport(agentContext, action, args).toAgentMessage();
    }

    @Nonnull
    public KanriExecuteReport executeReport(@Nonnull AgentContext agentContext
            , @Nonnull String action
            , @Nonnull String[] args
    ) {
        if (agentContext.msgType() != EnumMessageType.GROUP || agentContext.scopeGroupId() == 0) {
            return KanriExecuteReport.withMessage(KanriHandler.FAIL, action, "群管指令只能在群聊中使用。");
        }
        BaniraCodeContext codeContext = agentContext.messageContext();
        if (codeContext == null) {
            return KanriExecuteReport.withMessage(KanriHandler.FAIL, action, "缺少消息上下文，无法执行群管指令。");
        }

        KanriHandler handler = handlerRegistry.resolveForAi(action);
        if (handler == null) {
            return KanriExecuteReport.withMessage(KanriHandler.FAIL, action,
                    "未找到群管动作：" + action + "（AI 仅支持禁言/解禁/名片/头衔/精华/群名等安全操作）");
        }

        GroupMessageEvent event = KanriMessageEvents.from(codeContext);
        KanriContext kanriContext = new KanriContext(
                event,
                agentContext.bot(),
                agentContext.scopeGroupId(),
                agentContext.senderId(),
                codeContext.msgId(),
                KanriContext.getGuildMsgId(event),
                String.join(" ", args)
        ).aiInvoked(true);

        if (!handler.botHasPermission(kanriContext)) {
            return KanriExecuteReport.withMessage(
                    KanriHandler.BOT_NO_OP,
                    handler.getHelpSubTopic().name(),
                    "机器人没有执行该操作的权限（需要机器人为 QQ 群主或群管）。"
            );
        }
        if (!handler.hasPermission(kanriContext)
                && !KanriSelfOperationPolicy.allowsMute(kanriContext, args, handler)
                && !KanriSelfOperationPolicy.allowsLoud(kanriContext, args, handler)
                && !KanriSelfOperationPolicy.allowsCard(kanriContext, args, handler)) {
            return KanriExecuteReport.withMessage(
                    KanriHandler.NO_OP,
                    handler.getHelpSubTopic().name(),
                    "你没有执行该操作的权限。"
            );
        }

        int result;
        try {
            result = handler.execute(kanriContext, args);
        } catch (Exception e) {
            LOGGER.warn("Kanri AI command failed: action={}", action, e);
            return KanriExecuteReport.withMessage(
                    KanriHandler.FAIL,
                    handler.getHelpSubTopic().name(),
                    "群管指令执行失败：" + e.getMessage()
            );
        }
        return buildReport(result, handler, kanriContext, args, agentContext.bot(), agentContext.scopeGroupId());
    }

    @Nonnull
    public String executeMute(@Nonnull AgentContext agentContext
            , @Nonnull Collection<Long> targets
            , int minutes
    ) {
        return executeMuteReport(agentContext, targets, minutes).toAgentMessage();
    }

    @Nonnull
    public KanriExecuteReport executeMuteReport(@Nonnull AgentContext agentContext
            , @Nonnull Collection<Long> targets
            , int minutes
    ) {
        String action = muteActionKey();
        if (action == null) {
            return KanriExecuteReport.withMessage(KanriHandler.FAIL, "禁言", "未找到可用的禁言动作。");
        }
        return executeReport(agentContext, action, buildMuteArgs(targets, minutes));
    }

    @Nonnull
    public String[] parseArgs(@Nullable String argLine) {
        if (StringUtils.isNullOrEmptyEx(argLine)) {
            return new String[0];
        }
        return Arrays.stream(argLine.trim().split("\\s+"))
                .filter(part -> !part.isBlank())
                .toArray(String[]::new);
    }

    @Nullable
    private String muteActionKey() {
        return handlerRegistry.aiHandlers().entrySet().stream()
                .filter(entry -> entry.getValue() instanceof MuteCommand)
                .map(Map.Entry::getKey)
                .findFirst()
                .orElse(null);
    }

    @Nonnull
    private static String[] buildMuteArgs(@Nonnull Collection<Long> targets, int minutes) {
        List<String> parts = new ArrayList<>();
        for (Long target : targets) {
            if (target != null && target > 0) {
                parts.add(String.valueOf(target));
            }
        }
        parts.add(String.valueOf(Math.max(1, minutes)));
        return parts.toArray(String[]::new);
    }

    @Nonnull
    private KanriExecuteReport buildReport(int result
            , @Nonnull KanriHandler handler
            , @Nonnull KanriContext kanriContext
            , @Nonnull String[] args
            , @Nonnull BaniraBot bot
            , long groupId
    ) {
        String actionName = handler.getHelpSubTopic().name();
        if (result == KanriHandler.SUCCESS && containsWholeGroupTarget(args)) {
            String message = actionName.contains("解除") || actionName.contains("解禁")
                    ? "全员禁言已关闭。"
                    : "全员禁言已开启。";
            return KanriExecuteReport.withMessage(result, actionName, message);
        }
        Set<Long> intended = new LinkedHashSet<>(handler.getUserIdsWithReply(kanriContext, args));
        intended.remove(233L);
        Set<Long> failedIds = new LinkedHashSet<>(kanriContext.noPermissionTargets());
        Set<Long> succeededIds = new LinkedHashSet<>(intended);
        succeededIds.removeAll(failedIds);

        if (result == KanriHandler.SUCCESS && succeededIds.isEmpty() && !intended.isEmpty()) {
            succeededIds.clear();
            failedIds.clear();
            failedIds.addAll(intended);
        }

        int durationMinutes = extractDurationMinutes(handler, args);
        return new KanriExecuteReport(
                result,
                actionName,
                KanriMemberDisplayNames.formatAll(bot, groupId, succeededIds),
                KanriMemberDisplayNames.formatAll(bot, groupId, failedIds),
                durationMinutes
        );
    }

    private static int extractDurationMinutes(@Nonnull KanriHandler handler, @Nonnull String[] args) {
        if (!(handler instanceof MuteCommand) || args.length == 0) {
            return 0;
        }
        try {
            return Math.max(0, (int) Double.parseDouble(args[args.length - 1]));
        } catch (NumberFormatException ignored) {
            return 0;
        }
    }

    private static boolean containsWholeGroupTarget(@Nonnull String[] args) {
        for (String arg : args) {
            String normalized = StringUtils.nullToEmpty(arg).replaceAll("\\s+", "").toLowerCase(Locale.ROOT);
            if ("all".equals(normalized)
                    || "@all".equals(normalized)
                    || "全体".equals(normalized)
                    || "全员".equals(normalized)
                    || "@全体".equals(normalized)
                    || "@全员".equals(normalized)
                    || "@全体成员".equals(normalized)) {
                return true;
            }
        }
        return false;
    }

    @Nonnull
    private String formatHandler(@Nonnull KanriHandler handler) {
        HelpTopic topic = handler.getHelpSubTopic();
        String actions = handler.getAction().stream()
                .map(action -> action.toLowerCase(Locale.ROOT))
                .collect(Collectors.joining("|"));
        return "- " + actions + "：" + topic.name() + " — " + topic.description();
    }
}
