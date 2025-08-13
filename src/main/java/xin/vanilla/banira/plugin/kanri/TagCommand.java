package xin.vanilla.banira.plugin.kanri;

import com.mikuac.shiro.common.utils.FaceUtils;
import com.mikuac.shiro.common.utils.ShiroUtils;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.coder.BaniraCodeHandler;
import xin.vanilla.banira.config.entity.GlobalConfig;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.enums.EnumPermission;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.Arrays;
import java.util.Set;
import java.util.function.Supplier;

@Component
public class TagCommand implements KanriHandler {

    @Resource
    private Supplier<GlobalConfig> globalConfig;
    @Resource
    private BaniraCodeHandler codeHandler;

    @Override
    public boolean hasPermission(@Nonnull KanriContext context) {
        return context.bot().hasPermission(context.group(), context.sender(), EnumPermission.TAG);
    }

    @Nonnull
    @Override
    public Set<String> getAction() {
        return globalConfig.get().instConfig().kanri().tag();
    }

    @Override
    public int execute(@Nonnull KanriContext context, @Nonnull String[] args) {
        if (args.length < 1) return FAIL;

        // 解析目标
        Set<Long> targets = BaniraUtils.mutableSetOf();
        if (args.length == 1) {
            if (BaniraUtils.hasReplay(context.event().getArrayMsg())) {
                targets.add(BaniraUtils.getReplayQQ(context.bot(), context.group(), context.event().getArrayMsg()));
            } else return FAIL;
        }
        targets.addAll(ShiroUtils.getAtList(context.event().getArrayMsg()));
        targets.addAll(getQQs(Arrays.copyOf(args, args.length - 1)));

        String tag = args[args.length - 1];
        BaniraCodeContext codeContext = new BaniraCodeContext(context.bot());

        for (Long targetId : targets) {
            if (context.bot().isUpper(context.group(), context.sender(), targetId)) {
                BaniraCodeContext code = codeHandler.decode(
                        codeContext.setSender(context.sender())
                                .setGroup(context.group())
                                .setTarget(targetId)
                                .setMsg(tag)
                );
                context.bot().setGroupSpecialTitle(context.group(), targetId, code.getMsg(), -1);
            } else {
                fail.add(targetId);
            }
        }
        executeFail(context);

        if (context.msgId() > 0) {
            context.bot().setMsgEmojiLike(context.msgId(), String.valueOf(FaceUtils.get(124)), true);
        }

        return targets.isEmpty() ? FAIL : SUCCESS;
    }

}
