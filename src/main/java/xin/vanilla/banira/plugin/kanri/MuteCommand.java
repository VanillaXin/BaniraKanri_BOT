package xin.vanilla.banira.plugin.kanri;

import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.common.utils.ShiroUtils;
import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import com.mikuac.shiro.dto.event.message.GuildMessageEvent;
import com.mikuac.shiro.dto.event.message.PrivateMessageEvent;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.GlobalConfig;
import xin.vanilla.banira.domain.kanri.KanriContext;
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
        return BaniraUtils.hasPermission(context.bot(), context.group(), context.sender(), EnumPermission.MUTE);
    }

    @Nonnull
    @Override
    public Set<String> getAction() {
        return globalConfig.get().instConfig().kanri().mute();
    }

    @Override
    public boolean execute(@Nonnull KanriContext context, @Nonnull String[] args) {
        if (args.length < 1) return false;

        // 解析目标
        Set<Object> targets = BaniraUtils.mutableSetOf();
        if (args.length == 1) {
            if (BaniraUtils.hasReplay(context.event().getArrayMsg())) {
                targets.add(BaniraUtils.getReplayQQ(context.bot(), context.group(), context.event().getArrayMsg()));
            } else if (BaniraUtils.hasAtAll(context.event().getArrayMsg())) {
                targets.add(233L);
            } else return false;
        }
        targets.addAll(ShiroUtils.getAtList(context.event().getArrayMsg()));
        targets.addAll(getQQs(Arrays.copyOf(args, args.length - 1)));

        // 解析时长
        double duration = StringUtils.toDouble(args[args.length - 1]);

        // 全体禁言
        if (targets.contains(233L)) {
            context.bot().setGroupWholeBan(context.group(), true);
        }
        // 群员禁言
        else {
            if (duration <= 0) return false;
            Set<Long> fail = BaniraUtils.mutableSetOf();
            for (Object target : targets) {
                if (target instanceof Number) {
                    long targetId = ((Number) target).longValue();
                    if (BaniraUtils.isUpper(context.bot(), context.group(), context.sender(), targetId)) {
                        context.bot().setGroupBan(context.group(), targetId, (int) (duration * 60));
                    } else {
                        fail.add(targetId);
                    }
                } else if (target instanceof String) {
                    context.bot().setGroupAnonymousBan(context.group(), target.toString(), (int) (duration * 60));
                }
            }
            if (!fail.isEmpty()) {
                MsgUtils builder = MsgUtils.builder();
                switch (context.event()) {
                    case GroupMessageEvent event -> builder.reply(event.getMessageId());
                    case PrivateMessageEvent event -> builder.reply(event.getMessageId());
                    case GuildMessageEvent event -> builder.reply(event.getMessageId());
                    default -> {
                    }
                }
                context.bot().sendGroupMsg(context.group()
                        , builder.text(String.format("你没有权限对%s执行该操作", fail)).build()
                        , false
                );
            }
        }
        return !targets.isEmpty();
    }

}
