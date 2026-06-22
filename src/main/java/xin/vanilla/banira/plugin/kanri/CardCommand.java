package xin.vanilla.banira.plugin.kanri;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.coder.common.BaniraCodeHandler;
import xin.vanilla.banira.config.entity.InstructionsConfig;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.KanriContext;
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
        // 名片权限取决于目标：机器人改自己不需要群管，改其他人需要机器人是群主/群管。
        return true;
    }

    @Override
    public boolean hasPermission(@Nonnull KanriContext context) {
        // 普通成员也允许请求修改自己的群名片；是否能改别人交给 execute 按目标判断。
        return true;
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

        int success = 0;
        for (Long targetId : targets) {
            if (!canSenderRequestTarget(context, targetId) || !canBotModifyTarget(context, targetId)) {
                context.noPermissionTargets().add(targetId);
                continue;
            }
            BaniraCodeContext code = codeHandler.decode(
                    codeContext.sender(context.sender())
                            .group(context.group())
                            .target(targetId)
                            .msg(card)
            );
            context.bot().setGroupCard(context.group(), targetId, code.msg());
            success++;
        }
        executeFail(context);

        if (targets.isEmpty()) {
            return FAIL;
        }
        return success > 0 ? SUCCESS : NO_OP;
    }

    private boolean canSenderRequestTarget(@Nonnull KanriContext context, long targetId) {
        if (targetId == context.sender()) {
            return true;
        }
        if (context.bot().isGroupOwnerOrAdmin(context.group(), context.sender())) {
            return true;
        }
        try {
            return BaniraUtils.hasKanriOperatorAccess(context.bot(), context.group(), context.sender());
        } catch (IllegalStateException ignored) {
            return false;
        }
    }

    private boolean canBotModifyTarget(@Nonnull KanriContext context, long targetId) {
        return targetId == context.bot().getSelfId()
                || context.bot().isGroupOwnerOrAdmin(context.group());
    }

}
