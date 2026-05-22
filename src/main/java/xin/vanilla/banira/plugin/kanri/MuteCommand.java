package xin.vanilla.banira.plugin.kanri;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.InstructionsConfig;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.enums.EnumPermission;
import xin.vanilla.banira.plugin.help.HelpTopic;
import xin.vanilla.banira.plugin.help.HelpTopics;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.function.Supplier;

/**
 * 禁言群员
 */
@Component
public class MuteCommand implements KanriHandler {

    @Resource
    private Supplier<InstructionsConfig> insConfig;

    @Nonnull
    @Override
    public HelpTopic getHelpSubTopic() {
        String prefix = BaniraUtils.getKanriInsPrefixWithSpace();
        String actionHint = HelpTopics.formatAliasChoices(getAction());
        String detail = "用法1：\n" + prefix + actionHint + " <QQ号|艾特> ... <禁言秒数>\n\n"
                + "用法2：(回复要禁言的成员)\n" + prefix + actionHint + " <禁言秒数>\n\n"
                + "用法3：(全员禁言)\n" + prefix + actionHint + " @全体成员";
        return HelpTopics.of("禁言", "禁言群成员或全员。", 10, getAction()).detail(detail);
    }

    @Override
    public boolean botHasPermission(@Nonnull KanriContext context) {
        return context.bot().isGroupOwnerOrAdmin(context.group());
    }

    @Override
    public boolean hasPermission(@Nonnull KanriContext context) {
        return BaniraUtils.hasKanriOperatorAccess(context.bot(), context.group(), context.sender());
    }

    @Nonnull
    @Override
    public List<String> getAction() {
        return Objects.requireNonNullElseGet(insConfig.get().kanri().mute(), List::of);
    }

    @Override
    public int execute(@Nonnull KanriContext context, @Nonnull String[] args) {
        if (args.length < 1) return FAIL;

        // 解析目标
        Set<Long> targets = getUserIdsWithReply(context, args);
        if (containsWholeGroupTarget(args)) {
            targets.add(233L);
        }

        // 解析时长
        int duration = (int) (StringUtils.toDouble(CollectionUtils.getLast(args)) * 60d);

        // 全体禁言
        if (targets.contains(233L)) {
            if (context.bot().hasPermission(context.group(), context.sender(), EnumPermission.MALL)) {
                context.bot().setGroupWholeBan(context.group(), true);
            } else {
                return NO_OP;
            }
        }
        // 群员禁言
        else {
            if (duration <= 0) {
                return FAIL;
            }

            int muted = 0;
            for (Long targetId : targets) {
                if (tryMuteTarget(context, targetId, duration)) {
                    muted++;
                } else {
                    context.noPermissionTargets().add(targetId);
                }
            }
            executeFail(context);
            if (muted == 0) {
                return targets.isEmpty() ? FAIL : NO_OP;
            }
        }

        return SUCCESS;
    }

    private static boolean tryMuteTarget(@Nonnull KanriContext context, @Nonnull Long targetId, int durationSeconds) {
        if (targetId.equals(context.sender())
                && KanriSelfOperationPolicy.canAllowSelfMuteOrLoud(context.bot(), context.group(), context.sender())) {
            context.bot().setGroupBan(context.group(), targetId, durationSeconds);
            return true;
        }
        if (targetId.equals(context.sender())
                && BaniraUtils.hasKanriOperatorAccess(context.bot(), context.group(), context.sender())
                && context.bot().isUpperInGroup(context.group(), targetId)) {
            context.bot().setGroupBan(context.group(), targetId, durationSeconds);
            return true;
        }
        if (!BaniraUtils.hasKanriOperatorAccess(context.bot(), context.group(), context.sender())) {
            return false;
        }
        if (context.bot().isUpper(context.group(), context.sender(), targetId)
                && context.bot().isUpperInGroup(context.group(), targetId)) {
            context.bot().setGroupBan(context.group(), targetId, durationSeconds);
            return true;
        }
        return false;
    }

    private static boolean containsWholeGroupTarget(@Nonnull String[] args) {
        for (String arg : args) {
            String normalized = StringUtils.nullToEmpty(arg).replaceAll("\\s+", "").toLowerCase();
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
}
