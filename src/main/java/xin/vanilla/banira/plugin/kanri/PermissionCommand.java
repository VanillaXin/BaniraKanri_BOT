package xin.vanilla.banira.plugin.kanri;

import com.mikuac.shiro.common.utils.MsgUtils;
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

import java.util.*;
import java.util.function.Supplier;

/**
 * 设置权限
 */
@Component
public class PermissionCommand implements KanriHandler {

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
        String permissionList = EnumPermission.getAll().stream()
                .sorted()
                .map(op -> op.name() + "：" + op.getDesc())
                .reduce((a, b) -> a + "\n" + b)
                .orElse("");
        return HelpTopics.of("设置权限", "设置管家或女仆的权限。", 24, getAction())
                .child(HelpTopics.opAdd(base,
                        "用法：\n" + prefix + actionHint + " " + addHint + " <QQ号|艾特> ... <权限别称> ...\n\n"
                                + "权限别称列表：\n" + permissionList))
                .child(HelpTopics.opDel(base,
                        "用法：\n" + prefix + actionHint + " " + delHint + " <QQ号|艾特> ... <权限别称> ...\n\n"
                                + "权限别称列表：\n" + permissionList));
    }

    @Override
    public boolean botHasPermission(@Nonnull KanriContext context) {
        return true;
    }

    @Override
    public boolean hasPermission(@Nonnull KanriContext context) {
        return context.bot().hasAnyPermissions(context.group(), context.sender(), EnumPermission.APER, EnumPermission.RPER);
    }

    @Nonnull
    @Override
    public List<String> getAction() {
        return Objects.requireNonNullElseGet(insConfig.get().kanri().op(), List::of);
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

        // 判断权限
        if (operate == null
                && !context.bot().hasAllPermissions(context.group(), context.sender()
                , EnumPermission.APER
                , EnumPermission.RPER)
        ) return NO_OP;
        else if (Boolean.TRUE.equals(operate)
                && !context.bot().hasPermission(context.group(), context.sender()
                , EnumPermission.APER)
        ) return NO_OP;
        else if (Boolean.FALSE.equals(operate)
                && !context.bot().hasPermission(context.group(), context.sender()
                , EnumPermission.RPER)
        ) return NO_OP;

        // 解析目标
        Set<Long> targets = getUserIdsWithReply(context, args);

        // 解析内容
        List<EnumPermission> ops = EnumPermission.valueFrom(args);
        // 移除自身没有的权限
        ops.retainAll(context.bot().getPermission(context.group(), context.sender()));
        if (ops.isEmpty()) return FAIL;

        Set<Long> fail = new HashSet<>();
        for (Long targetId : targets) {
            List<EnumPermission> permissions;
            boolean butler = BaniraUtils.isButler(targetId);
            boolean maid = BaniraUtils.isMaid(context.group(), targetId);
            if (butler) {
                permissions = BaniraUtils.getButler()
                        .stream().filter(p -> targetId.equals(p.id()))
                        .findFirst().orElse(new PermissionConfig(targetId, new ArrayList<>()))
                        .permissions();
            } else if (maid) {
                permissions = groupConfig.get().maid().getOrDefault(context.group(), new ArrayList<>())
                        .stream().filter(p -> targetId.equals(p.id()))
                        .findFirst().orElse(new PermissionConfig(targetId, new ArrayList<>()))
                        .permissions();
            } else {
                fail.add(targetId);
                continue;
            }
            for (EnumPermission op : ops) {
                boolean add = Objects.requireNonNullElseGet(operate, () -> !permissions.contains(op));

                if (add) {
                    permissions.add(op);
                } else {
                    if (context.bot().isUpper(context.group(), context.sender(), targetId)) {
                        permissions.remove(op);
                    } else {
                        context.noPermissionTargets().add(targetId);
                    }
                }
            }
            if (butler) {
                BaniraUtils.getButler().removeIf(p -> targetId.equals(p.id()));
                BaniraUtils.getButler().add(new PermissionConfig(targetId, permissions));
            } else {
                groupConfig.get().maid().getOrDefault(context.group(), new ArrayList<>()).removeIf(p -> targetId.equals(p.id()));
                groupConfig.get().maid().getOrDefault(context.group(), new ArrayList<>()).add(new PermissionConfig(targetId, permissions));
            }
        }
        BaniraUtils.saveConfig();
        executeFail(context);
        if (!fail.isEmpty()) {
            MsgUtils builder = MsgUtils.builder();
            if (context.msgId() > 0) {
                builder.reply(context.msgId());
            }
            context.bot().sendGroupMsg(context.group()
                    , builder.text(String.format("无法操作非主管、女仆角色%s", fail)).build()
                    , false
            );
        }

        return targets.isEmpty() ? FAIL : SUCCESS;
    }

}
