package xin.vanilla.banira.plugin.kanri;

import com.mikuac.shiro.common.utils.MsgUtils;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.GlobalConfig;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.enums.EnumPermission;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.Objects;
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
    public boolean botHasPermission(@Nonnull KanriContext context) {
        return context.bot().isGroupOwnerOrAdmin(context.group());
    }

    @Override
    public boolean hasPermission(@Nonnull KanriContext context) {
        return context.bot().hasPermission(context.group(), context.sender(), EnumPermission.ATAL);
    }

    @Nonnull
    @Override
    public Set<String> getAction() {
        return Objects.requireNonNullElseGet(globalConfig.get().instConfig().base().atAll(), Set::of);
    }

    @Override
    public int execute(@Nonnull KanriContext context, @Nonnull String[] args) {
        int replayId = -1;
        if (BaniraUtils.hasReply(context.event().getArrayMsg())) {
            replayId = BaniraUtils.getReplyId(context.event().getArrayMsg()).intValue();
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
