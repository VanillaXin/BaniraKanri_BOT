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

import java.util.List;
import java.util.Objects;
import java.util.function.Supplier;

/**
 * 设置群名称
 */
@Component
public class GroupNameCommand implements KanriHandler {

    @Resource
    private Supplier<InstructionsConfig> insConfig;
    @Resource
    private BaniraCodeHandler codeHandler;

    @Nonnull
    @Override
    public HelpTopic getHelpSubTopic() {
        String prefix = BaniraUtils.getKanriInsPrefixWithSpace();
        String detail = "用法1：\n" + prefix + getAction() + " <群名称>\n\n"
                + "用法2：(回复要设置的内容)\n" + prefix + getAction();
        return HelpTopics.of("设置群名称", "修改群名称。", 16, getAction()).detail(detail);
    }

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
    public List<String> getAction() {
        return Objects.requireNonNullElseGet(insConfig.get().kanri().groupName(), List::of);
    }

    @Override
    public int execute(@Nonnull KanriContext context, @Nonnull String[] args) {
        // 解析内容
        String name;
        if (args.length == 0) {
            if (BaniraUtils.hasReply(context.event().getArrayMsg())) {
                name = BaniraUtils.getReplyContentString(context.bot(), context.event().getArrayMsg());
            } else {
                name = "";
            }
        } else {
            name = context.content();
        }
        if (name == null) return FAIL;

        BaniraCodeContext codeContext = new BaniraCodeContext(context.bot()
                , context.event().getArrayMsg()
                , context.group()
                , context.sender()
                , context.sender()
        );

        BaniraCodeContext code = codeHandler.decode(
                codeContext.sender(context.sender())
                        .group(context.group())
                        .msg(name)
        );
        context.bot().setGroupName(context.group(), code.msg());

        return SUCCESS;
    }

}
