package xin.vanilla.banira.plugin.kanri;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.InstructionsConfig;
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
 * 设置群管理
 */
@Component
public class AdminCommand implements KanriHandler {

    @Resource
    private Supplier<InstructionsConfig> insConfig;

    @Nonnull
    @Override
    public List<String> getHelpInfo(String... types) {
        List<String> result = new ArrayList<>();
        String type = CollectionUtils.getFirst(types);
        if (this.getAction().stream().anyMatch(s -> StringUtils.isNullOrEmptyEx(type) || s.equalsIgnoreCase(type))) {
            result.add("群管 - 增删群管理员：\n\n" +
                    "增加：\n" +
                    BaniraUtils.getKanriInsPrefixWithSpace()
                    + this.getAction() + " "
                    + insConfig.get().base().add() + " "
                    + "<QQ号|艾特> ..." + "\n\n" +
                    "删除：\n" +
                    BaniraUtils.getKanriInsPrefixWithSpace()
                    + this.getAction() + " "
                    + insConfig.get().base().del() + " "
                    + "<QQ号|艾特> ..."
            );
        }
        return result;
    }

    @Override
    public boolean botHasPermission(@Nonnull KanriContext context) {
        return context.bot().isGroupOwner(context.group());
    }

    @Override
    public boolean hasPermission(@Nonnull KanriContext context) {
        return context.bot().hasAnyPermissions(context.group(), context.sender(), EnumPermission.AADM, EnumPermission.RADM);
    }

    @Nonnull
    @Override
    public List<String> getAction() {
        return Objects.requireNonNullElseGet(insConfig.get().kanri().admin(), ArrayList::new);
    }

    @Override
    public int execute(@Nonnull KanriContext context, @Nonnull String[] args) {
        // 解析操作
        Boolean operate = null;
        if (args.length > 0) {
            if (insConfig.get().base().add().contains(args[0])) {
                operate = true;
            } else if (insConfig.get().base().del().contains(args[0])) {
                operate = false;
            }
        }

        // 判断权限
        if (operate == null
                && !context.bot().hasAllPermissions(context.group(), context.sender()
                , EnumPermission.AADM
                , EnumPermission.RADM)
        ) return NO_OP;
        else if (Boolean.TRUE.equals(operate)
                && !context.bot().hasPermission(context.group(), context.sender()
                , EnumPermission.AADM)
        ) return NO_OP;
        else if (Boolean.FALSE.equals(operate)
                && !context.bot().hasPermission(context.group(), context.sender()
                , EnumPermission.RADM)
        ) return NO_OP;

        // 解析目标
        Set<Long> targets = getQQsWithReply(context, args);

        for (Long targetId : targets) {
            boolean add = Objects.requireNonNullElseGet(operate, () -> !context.bot().isGroupAdmin(context.group(), targetId));

            if (add) {
                context.bot().setGroupAdmin(context.group(), targetId, true);
            } else {
                if (context.bot().isUpper(context.group(), context.sender(), targetId)) {
                    context.bot().setGroupAdmin(context.group(), targetId, false);
                } else {
                    nop.add(targetId);
                }
            }
        }

        return targets.isEmpty() ? FAIL : SUCCESS;
    }

}
