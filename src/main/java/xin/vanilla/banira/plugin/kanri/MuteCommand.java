package xin.vanilla.banira.plugin.kanri;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.GlobalConfig;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.enums.EnumPermission;
import xin.vanilla.banira.util.StringUtils;

import java.util.Objects;
import java.util.Set;
import java.util.function.Supplier;

/**
 * 禁言群员
 */
@Component
public class MuteCommand implements KanriHandler {

    @Resource
    private Supplier<GlobalConfig> globalConfig;

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
    public Set<String> getAction() {
        return Objects.requireNonNullElseGet(globalConfig.get().instConfig().kanri().mute(), Set::of);
    }

    @Override
    public int execute(@Nonnull KanriContext context, @Nonnull String[] args) {
        if (args.length < 1) return FAIL;

        // 解析目标
        Set<Long> targets = getQQsWithReplay(context, args);

        // 解析时长
        int duration = (int) (StringUtils.toDouble(args[args.length - 1]) * 60);

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
                    nop.add(targetId);
                }
            }
            executeFail(context);
        }

        return targets.isEmpty() ? FAIL : SUCCESS;
    }

}
