package xin.vanilla.banira.plugin.kanri;

import com.mikuac.shiro.common.utils.MsgUtils;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.GlobalConfig;
import xin.vanilla.banira.config.entity.GroupConfig;
import xin.vanilla.banira.config.entity.basic.PermissionConfig;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.enums.EnumPermission;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import java.util.function.Supplier;

/**
 * 设置权限
 */
@Component
public class PermissionCommand implements KanriHandler {

    @Resource
    private Supplier<GlobalConfig> globalConfig;
    @Resource
    private Supplier<GroupConfig> groupConfig;

    @Nonnull
    @Override
    public String getHelpInfo(String type) {
        if (this.getAction().stream().anyMatch(s -> StringUtils.isNullOrEmptyEx(type) || s.equalsIgnoreCase(type))) {
            return "设置管家或女仆的权限：\n\n" +
                    "增加权限：\n" +
                    BaniraUtils.getKanriInsPrefixWithSpace() +
                    this.getAction() + " " +
                    globalConfig.get().instConfig().base().add() + " " +
                    "<QQ号|艾特> ..." +
                    "<权限别称> ..." + "\n\n" +
                    "删除权限：\n" +
                    BaniraUtils.getKanriInsPrefixWithSpace() +
                    this.getAction() + " " +
                    globalConfig.get().instConfig().base().del() + " " +
                    "<QQ号|艾特> ..." +
                    "<权限别称> ..." + "\n\n" +
                    "权限别称列表：\n" +
                    EnumPermission.getAll().stream()
                            .sorted()
                            .map(op -> op.name() + "：" + op.getDesc())
                            .reduce((a, b) -> a + "\n" + b)
                            .orElse("")
                    ;
        }
        return "";
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
    public Set<String> getAction() {
        return Objects.requireNonNullElseGet(globalConfig.get().instConfig().kanri().op(), Set::of);
    }

    @Override
    public int execute(@Nonnull KanriContext context, @Nonnull String[] args) {
        // 解析操作
        Boolean operate = null;
        if (args.length > 0) {
            if (globalConfig.get().instConfig().base().add().contains(args[0])) {
                operate = true;
            } else if (globalConfig.get().instConfig().base().del().contains(args[0])) {
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
        Set<Long> targets = getQQsWithReplay(context, args);

        // 解析内容
        Set<EnumPermission> ops = EnumPermission.valueFrom(args);
        // 移除自身没有的权限
        ops.retainAll(context.bot().getPermission(context.group(), context.sender()));
        if (ops.isEmpty()) return FAIL;

        Set<Long> fail = new HashSet<>();
        for (Long targetId : targets) {
            Set<EnumPermission> permissions;
            boolean butler = BaniraUtils.isButler(targetId);
            boolean maid = BaniraUtils.isMaid(context.group(), targetId);
            if (butler) {
                permissions = globalConfig.get().butler()
                        .stream().filter(p -> targetId.equals(p.id()))
                        .findFirst().orElse(new PermissionConfig(targetId, new HashSet<>()))
                        .permissions();
            } else if (maid) {
                permissions = groupConfig.get().maid().getOrDefault(context.group(), new HashSet<>())
                        .stream().filter(p -> targetId.equals(p.id()))
                        .findFirst().orElse(new PermissionConfig(targetId, new HashSet<>()))
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
                        nop.add(targetId);
                    }
                }
            }
            if (butler) {
                globalConfig.get().butler().removeIf(p -> targetId.equals(p.id()));
                globalConfig.get().butler().add(new PermissionConfig(targetId, permissions));
            } else {
                groupConfig.get().maid().getOrDefault(context.group(), new HashSet<>()).removeIf(p -> targetId.equals(p.id()));
                groupConfig.get().maid().getOrDefault(context.group(), new HashSet<>()).add(new PermissionConfig(targetId, permissions));
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
