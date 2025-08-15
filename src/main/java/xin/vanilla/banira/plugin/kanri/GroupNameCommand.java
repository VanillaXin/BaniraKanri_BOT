package xin.vanilla.banira.plugin.kanri;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.coder.BaniraCodeHandler;
import xin.vanilla.banira.config.entity.GlobalConfig;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.enums.EnumPermission;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.Objects;
import java.util.Set;
import java.util.function.Supplier;

/**
 * 设置群名称
 */
@Component
public class GroupNameCommand implements KanriHandler {

    @Resource
    private Supplier<GlobalConfig> globalConfig;
    @Resource
    private BaniraCodeHandler codeHandler;

    @Override
    public boolean botHasPermission(@Nonnull KanriContext context) {
        return context.bot().isGroupOwnerOrAdmin(context.group());
    }

    @Override
    public boolean hasPermission(@Nonnull KanriContext context) {
        return context.bot().hasPermission(context.group(), context.sender(), EnumPermission.GNAM);
    }

    @Nonnull
    @Override
    public Set<String> getAction() {
        return Objects.requireNonNullElseGet(globalConfig.get().instConfig().kanri().groupName(), Set::of);
    }

    @Override
    public int execute(@Nonnull KanriContext context, @Nonnull String[] args) {
        // 解析内容
        String tag;
        if (args.length == 0) {
            if (BaniraUtils.hasReplay(context.event().getArrayMsg())) {
                tag = BaniraUtils.getReplayContent(context.bot(), context.event().getArrayMsg());
            } else {
                tag = "";
            }
        } else {
            tag = context.content();
        }
        if (tag == null) return FAIL;

        BaniraCodeContext codeContext = new BaniraCodeContext(context.bot());

        BaniraCodeContext code = codeHandler.decode(
                codeContext.setSender(context.sender())
                        .setGroup(context.group())
                        .setMsg(tag)
        );
        context.bot().setGroupName(context.group(), code.getMsg());

        return SUCCESS;
    }

}
