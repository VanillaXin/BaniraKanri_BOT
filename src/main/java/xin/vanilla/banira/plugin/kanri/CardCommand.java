package xin.vanilla.banira.plugin.kanri;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.coder.common.BaniraCodeHandler;
import xin.vanilla.banira.config.entity.InstructionsConfig;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.enums.EnumPermission;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.function.Supplier;

/**
 * 设置群名片
 */
@Component
public class CardCommand implements KanriHandler {

    @Resource
    private Supplier<InstructionsConfig> insConfig;
    @Resource
    private BaniraCodeHandler codeHandler;

    @Nonnull
    @Override
    public List<String> getHelpInfo(String... types) {
        List<String> result = new ArrayList<>();
        String type = CollectionUtils.getFirst(types);
        if (this.getAction().stream().anyMatch(s -> StringUtils.isNullOrEmptyEx(type) || s.equalsIgnoreCase(type))) {
            result.add("群管 - 设置群名片：\n\n" +
                    "用法1：\n" +
                    BaniraUtils.getKanriInsPrefixWithSpace() +
                    this.getAction() + " " +
                    "<QQ号|艾特> ... " + "<名片>" + "\n\n" +
                    "用法2：(回复要设置的内容)\n" +
                    BaniraUtils.getKanriInsPrefixWithSpace() +
                    this.getAction()
            );
        }
        return result;
    }

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
    public List<String> getAction() {
        return Objects.requireNonNullElseGet(insConfig.get().kanri().card(), List::of);
    }

    @Override
    public int execute(@Nonnull KanriContext context, @Nonnull String[] args) {
        // 解析目标
        Set<Long> targets = getUserIdsWithReply(context, args);

        // 解析内容
        String card;
        if (args.length == 0) {
            if (BaniraUtils.hasReply(context.event().getArrayMsg())) {
                card = BaniraUtils.getReplyContentString(context.bot(), context.event().getArrayMsg());
            } else {
                card = "";
            }
        } else {
            card = CollectionUtils.getLast(args);
        }
        if (card == null) return FAIL;

        BaniraCodeContext codeContext = new BaniraCodeContext(context.bot(), context.event().getArrayMsg());

        for (Long targetId : targets) {
            if (context.bot().isUpper(context.group(), context.sender(), targetId)) {
                BaniraCodeContext code = codeHandler.decode(
                        codeContext.sender(context.sender())
                                .group(context.group())
                                .target(targetId)
                                .msg(card)
                );
                context.bot().setGroupCard(context.group(), targetId, code.msg());
            } else {
                nop.add(targetId);
            }
        }
        executeFail(context);

        return targets.isEmpty() ? FAIL : SUCCESS;
    }

}
