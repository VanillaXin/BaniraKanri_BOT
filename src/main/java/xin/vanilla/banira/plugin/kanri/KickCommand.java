package xin.vanilla.banira.plugin.kanri;

import com.mikuac.shiro.common.utils.ShiroUtils;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.GlobalConfig;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.enums.EnumPermission;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.StringUtils;

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
    public boolean hasPermission(@Nonnull KanriContext context) {
        return context.bot().hasPermission(context.group(), context.sender(), EnumPermission.KICK);
    }

    @Nonnull
    @Override
    public Set<String> getAction() {
        return globalConfig.get().instConfig().kanri().kick();
    }

    @Override
    public int execute(@Nonnull KanriContext context, @Nonnull String[] args) {
        // 解析目标
        Set<Long> targets = BaniraUtils.mutableSetOf();
        if (args.length == 0) {
            if (BaniraUtils.hasReplay(context.event().getArrayMsg())) {
                targets.add(BaniraUtils.getReplayQQ(context.bot(), context.group(), context.event().getArrayMsg()));
            } else return FAIL;
        }
        targets.addAll(ShiroUtils.getAtList(context.event().getArrayMsg()));
        targets.addAll(getQQs(args));

        for (Long targetId : targets) {
            if (context.bot().isUpper(context.group(), context.sender(), targetId)) {
                context.bot().setGroupKick(context.group(), targetId, StringUtils.stringToBoolean(CollectionUtils.getLast(args)));
            } else {
                fail.add(targetId);
            }
        }
        executeFail(context);

        if (context.msgId() > 0) {
            context.bot().setMsgEmojiLike(context.msgId(), String.valueOf(124), true);
        }

        return targets.isEmpty() ? FAIL : SUCCESS;
    }

}
