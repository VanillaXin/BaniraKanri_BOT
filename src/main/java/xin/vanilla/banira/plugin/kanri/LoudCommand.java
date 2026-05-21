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

import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.function.Supplier;

/**
 * 解除禁言
 */
@Component
public class LoudCommand implements KanriHandler {

    @Resource
    private Supplier<InstructionsConfig> insConfig;

    @Nonnull
    @Override
    public HelpTopic getHelpSubTopic() {
        String prefix = BaniraUtils.getKanriInsPrefixWithSpace();
        String actionHint = HelpTopics.formatAliasChoices(getAction());
        String detail = "用法1：\n" + prefix + actionHint + " <QQ号|艾特> ...\n\n"
                + "用法2：(回复要解除禁言的成员)\n" + prefix + actionHint + "\n\n"
                + "用法3：(解除全员禁言)\n" + prefix + actionHint + " @全体成员";
        return HelpTopics.of("解除禁言", "解除群成员或全员禁言。", 11, getAction()).detail(detail);
    }

    @Override
    public boolean botHasPermission(@Nonnull KanriContext context) {
        return context.bot().isGroupOwnerOrAdmin(context.group());
    }

    @Override
    public boolean hasPermission(@Nonnull KanriContext context) {
        return context.bot().hasAnyPermissions(context.group(), context.sender(), EnumPermission.LOUD, EnumPermission.LALL);
    }

    @Nonnull
    @Override
    public List<String> getAction() {
        return Objects.requireNonNullElseGet(insConfig.get().kanri().loud(), List::of);
    }

    @Override
    public int execute(@Nonnull KanriContext context, @Nonnull String[] args) {
        // 解析目标
        Set<Long> targets = getUserIdsWithReply(context, args);

        // 全体解禁
        if (targets.contains(233L)) {
            if (context.bot().hasPermission(context.group(), context.sender(), EnumPermission.LALL)) {
                context.bot().setGroupWholeBan(context.group(), false);
            } else {
                return NO_OP;
            }
        }
        // 群员解禁
        else {
            if (!context.bot().hasPermission(context.group(), context.sender(), EnumPermission.LOUD)) {
                return NO_OP;
            }

            for (Long targetId : targets) {
                if (context.bot().isUpper(context.group(), context.sender(), targetId)
                        && context.bot().isUpperInGroup(context.group(), targetId)
                ) {
                    context.bot().setGroupBan(context.group(), targetId, 0);
                } else {
                    context.noPermissionTargets().add(targetId);
                }
            }
            executeFail(context);
        }

        return targets.isEmpty() ? FAIL : SUCCESS;
    }

}
