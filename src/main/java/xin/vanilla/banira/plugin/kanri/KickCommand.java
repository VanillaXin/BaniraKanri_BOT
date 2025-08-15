package xin.vanilla.banira.plugin.kanri;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.GlobalConfig;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.enums.EnumPermission;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.Objects;
import java.util.Set;
import java.util.function.Supplier;

/**
 * 踢出群员
 */
@Component
public class KickCommand implements KanriHandler {

    @Resource
    private Supplier<GlobalConfig> globalConfig;

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
    public Set<String> getAction() {
        return Objects.requireNonNullElseGet(globalConfig.get().instConfig().kanri().kick(), Set::of);
    }

    @Override
    public int execute(@Nonnull KanriContext context, @Nonnull String[] args) {
        // 解析目标
        Set<Long> targets = getQQsWithReplay(context, args);

        for (Long targetId : targets) {
            if (context.bot().isUpper(context.group(), context.sender(), targetId)
                    && context.bot().isUpperInGroup(context.group(), targetId)
            ) {
                context.bot().setGroupKick(context.group(), targetId, StringUtils.stringToBoolean(CollectionUtils.getLast(args)));
            } else {
                fail.add(targetId);
            }
        }
        executeFail(context);

        return targets.isEmpty() ? FAIL : SUCCESS;
    }

}
