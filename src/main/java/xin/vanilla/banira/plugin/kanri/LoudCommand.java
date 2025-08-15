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
 * 解除禁言
 */
@Component
public class LoudCommand implements KanriHandler {

    @Resource
    private Supplier<GlobalConfig> globalConfig;

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
    public Set<String> getAction() {
        return Objects.requireNonNullElseGet(globalConfig.get().instConfig().kanri().loud(), Set::of);
    }

    @Override
    public int execute(@Nonnull KanriContext context, @Nonnull String[] args) {
        // 解析目标
        Set<Long> targets = getQQsWithReplay(context, args);

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
                    nop.add(targetId);
                }
            }
            executeFail(context);
        }

        return targets.isEmpty() ? FAIL : SUCCESS;
    }

}
