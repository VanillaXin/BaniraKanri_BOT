package xin.vanilla.banira.plugin.kanri;

import com.mikuac.shiro.common.utils.FaceUtils;
import com.mikuac.shiro.common.utils.ShiroUtils;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.GlobalConfig;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.enums.EnumPermission;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.Arrays;
import java.util.Set;
import java.util.function.Supplier;

@Component
public class MuteCommand implements KanriHandler {

    @Resource
    private Supplier<GlobalConfig> globalConfig;

    @Override
    public boolean hasPermission(@Nonnull KanriContext context) {
        return context.bot().hasAnyPermissions(context.group(), context.sender(), EnumPermission.MUTE, EnumPermission.MALL);
    }

    @Nonnull
    @Override
    public Set<String> getAction() {
        return globalConfig.get().instConfig().kanri().mute();
    }

    @Override
    public int execute(@Nonnull KanriContext context, @Nonnull String[] args) {
        if (args.length < 1) return FAIL;

        // 解析目标
        Set<Object> targets = BaniraUtils.mutableSetOf();
        if (args.length == 1) {
            if (BaniraUtils.hasReplay(context.event().getArrayMsg())) {
                targets.add(BaniraUtils.getReplayQQ(context.bot(), context.group(), context.event().getArrayMsg()));
            } else if (BaniraUtils.hasAtAll(context.event().getArrayMsg())) {
                targets.add(233L);
            } else return FAIL;
        }
        targets.addAll(ShiroUtils.getAtList(context.event().getArrayMsg()));
        targets.addAll(getQQs(Arrays.copyOf(args, args.length - 1)));

        // 解析时长
        int duration = (int) (StringUtils.toDouble(args[args.length - 1]) * 60);

        // 全体禁言
        if (targets.contains(233L)) {
            if (context.bot().hasPermission(context.group(), context.sender(), EnumPermission.MALL)) {
                context.bot().setGroupWholeBan(context.group(), true);
            } else {
                return NO_PERMISSION;
            }
        }
        // 群员禁言
        else {
            if (duration <= 0) return FAIL;
            if (context.bot().hasPermission(context.group(), context.sender(), EnumPermission.MUTE)) {
                return NO_PERMISSION;
            }

            for (Object target : targets) {
                if (target instanceof Number) {
                    long targetId = ((Number) target).longValue();
                    if (context.bot().isUpper(context.group(), context.sender(), targetId)) {
                        context.bot().setGroupBan(context.group(), targetId, duration);
                    } else {
                        fail.add(targetId);
                    }
                } else if (target instanceof String) {
                    context.bot().setGroupAnonymousBan(context.group(), target.toString(), duration);
                }
            }
            executeFail(context);
        }

        if (context.msgId() > 0) {
            context.bot().setMsgEmojiLike(context.msgId(), String.valueOf(FaceUtils.get(124)), true);
        }

        return targets.isEmpty() ? FAIL : SUCCESS;
    }

}
