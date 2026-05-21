package xin.vanilla.banira.plugin.kanri;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.GroupConfig;
import xin.vanilla.banira.config.entity.InstructionsConfig;
import xin.vanilla.banira.config.entity.basic.PermissionConfig;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.enums.EnumPermission;
import xin.vanilla.banira.config.entity.basic.BaseInstructionsConfig;
import xin.vanilla.banira.plugin.help.HelpTopic;
import xin.vanilla.banira.plugin.help.HelpTopics;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.function.Supplier;

/**
 * 设置女仆
 */
@Component
public class MaidCommand implements KanriHandler {

    @Resource
    private Supplier<InstructionsConfig> insConfig;
    @Resource
    private Supplier<GroupConfig> groupConfig;

    @Nonnull
    @Override
    public HelpTopic getHelpSubTopic() {
        String prefix = BaniraUtils.getKanriInsPrefixWithSpace();
        BaseInstructionsConfig base = insConfig.get().base();
        String actionHint = HelpTopics.formatAliasChoices(getAction());
        String addHint = HelpTopics.formatAliasChoices(base.add());
        String delHint = HelpTopics.formatAliasChoices(base.del());
        return HelpTopics.of("增删女仆", "增加或移除女仆。", 22, getAction())
                .child(HelpTopics.opAdd(base, "用法：\n" + prefix + actionHint + " " + addHint + " <QQ号|艾特> ..."))
                .child(HelpTopics.opDel(base, "用法：\n" + prefix + actionHint + " " + delHint + " <QQ号|艾特> ..."));
    }

    @Override
    public boolean botHasPermission(@Nonnull KanriContext context) {
        return true;
    }

    @Override
    public boolean hasPermission(@Nonnull KanriContext context) {
        return context.bot().hasAnyPermissions(context.group(), context.sender(), EnumPermission.AMAI, EnumPermission.RMAI);
    }

    @Nonnull
    @Override
    public List<String> getAction() {
        return Objects.requireNonNullElseGet(insConfig.get().kanri().maid(), List::of);
    }

    @Override
    public int execute(@Nonnull KanriContext context, @Nonnull String[] args) {
        // 解析操作
        Boolean operate = null;
        if (args.length > 0) {
            if (insConfig.get().base().add().contains(args[0])) {
                operate = true;
            } else if (insConfig.get().base().del().contains(args[0])) {
                operate = false;
            }
        }

        if (!BaniraUtils.isGroupIdValid(context.group())) return FAIL;

        // 判断权限
        if (operate == null
                && !context.bot().hasAllPermissions(context.group(), context.sender()
                , EnumPermission.AMAI
                , EnumPermission.RMAI)
        ) return NO_OP;
        else if (Boolean.TRUE.equals(operate)
                && !context.bot().hasPermission(context.group(), context.sender()
                , EnumPermission.AMAI)
        ) return NO_OP;
        else if (Boolean.FALSE.equals(operate)
                && !context.bot().hasPermission(context.group(), context.sender()
                , EnumPermission.RMAI)
        ) return NO_OP;

        // 解析目标
        Set<Long> targets = getUserIdsWithReply(context, args);

        for (Long targetId : targets) {
            boolean add = Objects.requireNonNullElseGet(operate, () -> !BaniraUtils.isMaid(context.group(), targetId));

            if (add) {
                groupConfig.get().maid().computeIfAbsent(context.group(), k -> new ArrayList<>())
                        .add(new PermissionConfig(targetId, EnumPermission.getMaid()));
            } else {
                if (context.bot().isUpper(context.group(), context.sender(), targetId)) {
                    groupConfig.get().maid().computeIfAbsent(context.group(), k -> new ArrayList<>())
                            .removeIf(maid -> targetId.equals(maid.id()));
                } else {
                    context.noPermissionTargets().add(targetId);
                }
            }
        }
        BaniraUtils.saveGroupConfig();

        return targets.isEmpty() ? FAIL : SUCCESS;
    }

}
