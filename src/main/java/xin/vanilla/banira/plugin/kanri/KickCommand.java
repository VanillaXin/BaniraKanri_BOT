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
 * 踢出群员
 */
@Component
public class KickCommand implements KanriHandler {

    @Resource
    private Supplier<InstructionsConfig> insConfig;

    @Nonnull
    @Override
    public HelpTopic getHelpSubTopic() {
        String prefix = BaniraUtils.getKanriInsPrefixWithSpace();
        String actionHint = HelpTopics.formatAliasChoices(getAction());
        String detail = "用法1：\n" + prefix + actionHint + " <QQ号|艾特> ...\n\n"
                + "用法2：(回复要踢的成员)\n" + prefix + actionHint;
        return HelpTopics.of("踢出成员", "踢出群成员。", 12, getAction()).detail(detail);
    }

    @Override
    public boolean botHasPermission(@Nonnull KanriContext context) {
        return context.bot().isGroupOwnerOrAdmin(context.group());
    }

    @Override
    public boolean hasPermission(@Nonnull KanriContext context) {
        return context.bot().hasPermission(context.group(), context.sender(), EnumPermission.KICK);
    }

    @Nonnull
    @Override
    public List<String> getAction() {
        return Objects.requireNonNullElseGet(insConfig.get().kanri().kick(), List::of);
    }

    @Override
    public int execute(@Nonnull KanriContext context, @Nonnull String[] args) {
        // 解析目标
        Set<Long> targets = getUserIdsWithReply(context, args);

        for (Long targetId : targets) {
            if (context.bot().isUpper(context.group(), context.sender(), targetId)
                    && context.bot().isUpperInGroup(context.group(), targetId)
            ) {
                context.bot().setGroupKick(context.group(), targetId, StringUtils.stringToBoolean(CollectionUtils.getLast(args)));
            } else {
                context.noPermissionTargets().add(targetId);
            }
        }
        executeFail(context);

        return targets.isEmpty() ? FAIL : SUCCESS;
    }

}
