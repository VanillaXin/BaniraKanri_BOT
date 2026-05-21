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

import java.util.ArrayList;
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
        return context.bot().hasAnyPermissions(context.group(), context.sender(), EnumPermission.MUTE, EnumPermission.MALL);
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
            if (duration <= 0) return FAIL;
            if (!context.bot().hasPermission(context.group(), context.sender(), EnumPermission.MUTE)) {
                return NO_OP;
            }

            for (Long targetId : targets) {
                if (context.bot().isUpper(context.group(), context.sender(), targetId)
                        && context.bot().isUpperInGroup(context.group(), targetId)
                ) {
                    context.bot().setGroupBan(context.group(), targetId, duration);
                } else {
                    context.noPermissionTargets().add(targetId);
                }
            }
            executeFail(context);
        }

        return targets.isEmpty() ? FAIL : SUCCESS;
    }

}
