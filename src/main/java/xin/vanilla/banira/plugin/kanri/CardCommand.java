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
 * 设置群名片
 */
@Component
public class CardCommand implements KanriHandler {

    @Resource
    private Supplier<GlobalConfig> globalConfig;
    @Resource
    private BaniraCodeHandler codeHandler;

    @Override
    public boolean botHasPermission(@Nonnull KanriContext context) {
        // 可以改自己的所以直接返回true
        return true;
    }

    @Override
    public boolean hasPermission(@Nonnull KanriContext context) {
        return context.bot().hasPermission(context.group(), context.sender(), EnumPermission.CARD);
    }

    @Nonnull
    @Override
    public Set<String> getAction() {
        return Objects.requireNonNullElseGet(globalConfig.get().instConfig().kanri().card(), Set::of);
    }

    @Override
    public int execute(@Nonnull KanriContext context, @Nonnull String[] args) {
        // 解析目标
        Set<Long> targets = getQQsWithReplay(context, args);

        // 解析内容
        String card;
        if (args.length == 0) {
            if (BaniraUtils.hasReplay(context.event().getArrayMsg())) {
                card = BaniraUtils.getReplayContent(context.bot(), context.event().getArrayMsg());
            } else {
                card = "";
            }
        } else {
            card = args[args.length - 1];
        }
        if (card == null) return FAIL;

        BaniraCodeContext codeContext = new BaniraCodeContext(context.bot());

        for (Long targetId : targets) {
            if (context.bot().isUpper(context.group(), context.sender(), targetId)) {
                BaniraCodeContext code = codeHandler.decode(
                        codeContext.setSender(context.sender())
                                .setGroup(context.group())
                                .setTarget(targetId)
                                .setMsg(card)
                );
                context.bot().setGroupCard(context.group(), targetId, code.getMsg());
            } else {
                nop.add(targetId);
            }
        }
        executeFail(context);

        return targets.isEmpty() ? FAIL : SUCCESS;
    }

}
