package xin.vanilla.banira.plugin.kanri;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.GlobalConfig;
import xin.vanilla.banira.config.entity.GroupConfig;
import xin.vanilla.banira.config.entity.basic.PermissionConfig;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.enums.EnumPermission;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import java.util.function.Supplier;

/**
 * 设置女仆
 */
@Component
public class MaidCommand implements KanriHandler {

    @Resource
    private Supplier<GlobalConfig> globalConfig;
    @Resource
    private Supplier<GroupConfig> groupConfig;

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
    public Set<String> getAction() {
        return Objects.requireNonNullElseGet(globalConfig.get().instConfig().kanri().maid(), Set::of);
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
        Set<Long> targets = getQQsWithReplay(context, args);

        for (Long targetId : targets) {
            boolean add = Objects.requireNonNullElseGet(operate, () -> !BaniraUtils.isMaid(context.group(), targetId));

            if (add) {
                groupConfig.get().maid().computeIfAbsent(context.group(), k -> new HashSet<>())
                        .add(new PermissionConfig(targetId, EnumPermission.getMaid()));
            } else {
                if (context.bot().isUpper(context.group(), context.sender(), targetId)) {
                    groupConfig.get().maid().computeIfAbsent(context.group(), k -> new HashSet<>())
                            .removeIf(maid -> targetId.equals(maid.id()));
                } else {
                    nop.add(targetId);
                }
            }
        }
        BaniraUtils.saveGroupConfig();

        return targets.isEmpty() ? FAIL : SUCCESS;
    }

}
