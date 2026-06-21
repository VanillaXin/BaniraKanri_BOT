package xin.vanilla.banira.plugin.kanri;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.coder.common.BaniraCodeHandler;
import xin.vanilla.banira.config.entity.InstructionsConfig;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.enums.EnumPermission;
import xin.vanilla.banira.plugin.help.HelpTopic;
import xin.vanilla.banira.plugin.help.HelpTopics;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;

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
    public HelpTopic getHelpSubTopic() {
        String prefix = BaniraUtils.getKanriInsPrefixWithSpace();
        String actionHint = HelpTopics.formatAliasChoices(getAction());
        String detail = "用法1：\n" + prefix + actionHint + " <QQ号|艾特> ... <名片>\n\n"
                + "用法2：(回复要设置的内容)\n" + prefix + actionHint;
        return HelpTopics.of("设置群名片", "设置群成员名片。", 14, getAction()).detail(detail);
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

        BaniraCodeContext codeContext = new BaniraCodeContext(context.bot()
                , context.event().getArrayMsg()
                , context.group()
                , context.sender()
                , context.sender()
        );

        for (Long targetId : targets) {
            BaniraCodeContext code = codeHandler.decode(
                    codeContext.sender(context.sender())
                            .group(context.group())
                            .target(targetId)
                            .msg(card)
            );
            context.bot().setGroupCard(context.group(), targetId, code.msg());
        }
        executeFail(context);

        return targets.isEmpty() ? FAIL : SUCCESS;
    }

}
