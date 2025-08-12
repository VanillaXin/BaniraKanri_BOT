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

import java.util.Set;
import java.util.function.Supplier;

@Component
public class LoudCommand implements KanriHandler {

    @Resource
    private Supplier<GlobalConfig> globalConfig;

    @Override
    public boolean hasPermission(@Nonnull KanriContext context) {
        return context.bot().hasPermission(context.group(), context.sender(), EnumPermission.LOUD);
    }

    @Nonnull
    @Override
    public Set<String> getAction() {
        return globalConfig.get().instConfig().kanri().loud();
    }

    @Override
    public int execute(@Nonnull KanriContext context, @Nonnull String[] args) {
        // 解析目标
        Set<Object> targets = BaniraUtils.mutableSetOf();
        if (args.length == 0) {
            if (BaniraUtils.hasReplay(context.event().getArrayMsg())) {
                targets.add(BaniraUtils.getReplayQQ(context.bot(), context.group(), context.event().getArrayMsg()));
            } else if (BaniraUtils.hasAtAll(context.event().getArrayMsg())) {
                targets.add(233L);
            } else return FAIL;
        }
        targets.addAll(ShiroUtils.getAtList(context.event().getArrayMsg()));
        targets.addAll(getQQs(args));

        // 全体解禁
        if (targets.contains(233L)) {
            if (context.bot().hasPermission(context.group(), context.sender(), EnumPermission.MALL)) {
                context.bot().setGroupWholeBan(context.group(), false);
            } else {
                return NO_PERMISSION;
            }
        }
        // 群员解禁
        else {
            if (context.bot().hasPermission(context.group(), context.sender(), EnumPermission.MUTE)) {
                return NO_PERMISSION;
            }

            Set<Long> fail = BaniraUtils.mutableSetOf();
            for (Object target : targets) {
                if (target instanceof Number) {
                    long targetId = ((Number) target).longValue();
                    if (context.bot().isUpper(context.group(), context.sender(), targetId)) {
                        context.bot().setGroupBan(context.group(), targetId, 0);
                    } else {
                        fail.add(targetId);
                    }
                } else if (target instanceof String) {
                    context.bot().setGroupAnonymousBan(context.group(), target.toString(), 1);
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
        return targets.isEmpty() ? FAIL : SUCCESS;
    }

}
