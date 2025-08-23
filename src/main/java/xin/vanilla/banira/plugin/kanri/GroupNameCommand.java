package xin.vanilla.banira.plugin.kanri;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.coder.common.BaniraCodeHandler;
import xin.vanilla.banira.config.entity.GlobalConfig;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.enums.EnumPermission;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
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

    @Nonnull
    @Override
    public List<String> getHelpInfo(String... types) {
        List<String> result = new ArrayList<>();
        String type = CollectionUtils.getFirst(types);
        if (this.getAction().stream().anyMatch(s -> StringUtils.isNullOrEmptyEx(type) || s.equalsIgnoreCase(type))) {
            result.add("群管 - 设置群名称：\n\n" +
                    "用法1：\n" +
                    BaniraUtils.getKanriInsPrefixWithSpace() +
                    this.getAction() + " " +
                    "<群名称>" + "\n\n" +
                    "用法2：(回复要设置的内容)\n" +
                    BaniraUtils.getKanriInsPrefixWithSpace() +
                    this.getAction()
            );
        }
        return result;
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
        return Objects.requireNonNullElseGet(globalConfig.get().instConfig().kanri().groupName(), List::of);
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

        BaniraCodeContext codeContext = new BaniraCodeContext(context.bot(), context.event().getArrayMsg());

        BaniraCodeContext code = codeHandler.decode(
                codeContext.setSender(context.sender())
                        .setGroup(context.group())
                        .setMsg(name)
        );
        context.bot().setGroupName(context.group(), code.getMsg());

        return SUCCESS;
    }

}
