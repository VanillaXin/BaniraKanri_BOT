package xin.vanilla.banira.plugin.kanri;

import com.mikuac.shiro.common.utils.MsgUtils;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.GlobalConfig;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.enums.EnumPermission;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.Set;
import java.util.function.Supplier;

/**
 * AT全体成员
 */
@Component
public class AtAllCommand implements KanriHandler {

    @Resource
    private Supplier<GlobalConfig> globalConfig;

    @Override
    public boolean hasPermission(@Nonnull KanriContext context) {
        return context.bot().hasPermission(context.group(), context.sender(), EnumPermission.ATAL);
    }

    @Nonnull
    @Override
    public Set<String> getAction() {
        return globalConfig.get().instConfig().base().atAll();
    }

    @Override
    public int execute(@Nonnull KanriContext context, @Nonnull String[] args) {
        int replayId = -1;
        if (BaniraUtils.hasReplay(context.event().getArrayMsg())) {
            replayId = BaniraUtils.getReplayId(context.event().getArrayMsg()).intValue();
        }
        if (replayId < 0) {
            replayId = context.msgId();
        }
        if (replayId < 0) {
            return FAIL;
        }

        context.bot().sendGroupMsg(context.group()
                , MsgUtils.builder()
                        .reply(replayId)
                        .atAll()
                        .build()
                , false
        );
        return SUCCESS;
    }

}
