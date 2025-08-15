package xin.vanilla.banira.plugin.kanri;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.GlobalConfig;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.enums.EnumPermission;

import java.util.Objects;
import java.util.Set;
import java.util.function.Supplier;

/**
 * 设置群管理
 */
@Component
public class AdminCommand implements KanriHandler {

    @Resource
    private Supplier<GlobalConfig> globalConfig;

    @Override
    public boolean botHasPermission(@Nonnull KanriContext context) {
        return context.bot().isGroupOwner(context.group());
    }

    @Override
    public boolean hasPermission(@Nonnull KanriContext context) {
        return context.bot().hasAnyPermissions(context.group(), context.sender(), EnumPermission.AADM, EnumPermission.RADM);
    }

    @Nonnull
    @Override
    public Set<String> getAction() {
        return Objects.requireNonNullElseGet(globalConfig.get().instConfig().kanri().admin(), Set::of);
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
                , EnumPermission.AADM
                , EnumPermission.RADM)
        ) return NO_OP;
        else if (Boolean.TRUE.equals(operate)
                && !context.bot().hasPermission(context.group(), context.sender()
                , EnumPermission.AADM)
        ) return NO_OP;
        else if (Boolean.FALSE.equals(operate)
                && !context.bot().hasPermission(context.group(), context.sender()
                , EnumPermission.RADM)
        ) return NO_OP;

        // 解析目标
        Set<Long> targets = getQQsWithReplay(context, args);

        for (Long targetId : targets) {
            boolean add = Objects.requireNonNullElseGet(operate, () -> !context.bot().isGroupAdmin(context.group(), targetId));

            if (add) {
                context.bot().setGroupAdmin(context.group(), targetId, true);
            } else {
                if (context.bot().isUpper(context.group(), context.sender(), targetId)) {
                    context.bot().setGroupAdmin(context.group(), targetId, false);
                } else {
                    fail.add(targetId);
                }
            }
        }

        return targets.isEmpty() ? FAIL : SUCCESS;
    }

}
