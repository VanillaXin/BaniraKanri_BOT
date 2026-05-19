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
 * 设置群头衔
 */
@Component
public class TagCommand implements KanriHandler {

    @Resource
    private Supplier<InstructionsConfig> insConfig;
    @Resource
    private BaniraCodeHandler codeHandler;

    @Nonnull
    @Override
    public HelpTopic getHelpSubTopic() {
        String prefix = BaniraUtils.getKanriInsPrefixWithSpace();
        String detail = "用法1：\n" + prefix + getAction() + " <QQ号|艾特> ... <头衔>\n\n"
                + "用法2：(回复要设置的内容)\n" + prefix + getAction();
        return HelpTopics.of("设置群头衔", "设置群成员头衔。", 15, getAction()).detail(detail);
    }

    @Override
    public boolean botHasPermission(@Nonnull KanriContext context) {
        return context.bot().isGroupOwner(context.group());
    }

    @Override
    public boolean hasPermission(@Nonnull KanriContext context) {
        return context.bot().hasPermission(context.group(), context.sender(), EnumPermission.TAG);
    }

    @Nonnull
    @Override
    public List<String> getAction() {
        return Objects.requireNonNullElseGet(insConfig.get().kanri().tag(), List::of);
    }

    @Override
    public int execute(@Nonnull KanriContext context, @Nonnull String[] args) {
        // 解析目标
        Set<Long> targets = getUserIdsWithReply(context, args);

        // 解析内容
        String tag;
        if (args.length == 0) {
            if (BaniraUtils.hasReply(context.event().getArrayMsg())) {
                tag = BaniraUtils.getReplyContentString(context.bot(), context.event().getArrayMsg());
            } else {
                tag = "";
            }
        } else {
            tag = CollectionUtils.getLast(args);
        }
        if (tag == null) return FAIL;

        BaniraCodeContext codeContext = new BaniraCodeContext(context.bot()
                , context.event().getArrayMsg()
                , context.group()
                , context.sender()
                , context.sender()
        );

        for (Long targetId : targets) {
            if (context.bot().isUpper(context.group(), context.sender(), targetId)) {
                BaniraCodeContext code = codeHandler.decode(
                        codeContext.target(targetId)
                                .msg(tag)
                );
                context.bot().setGroupSpecialTitle(context.group(), targetId, code.msg(), -1);
            } else {
                context.noPermissionTargets().add(targetId);
            }
        }
        executeFail(context);

        return targets.isEmpty() ? FAIL : SUCCESS;
    }

}
