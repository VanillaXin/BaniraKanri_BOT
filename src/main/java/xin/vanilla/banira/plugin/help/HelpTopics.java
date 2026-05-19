package xin.vanilla.banira.plugin.help;

import jakarta.annotation.Nonnull;
import xin.vanilla.banira.config.entity.basic.BaseInstructionsConfig;
import xin.vanilla.banira.util.StringUtils;

import java.util.Collection;
import java.util.List;

/**
 * HelpTopic 构建工具
 */
public final class HelpTopics {

    private HelpTopics() {
    }

    @Nonnull
    public static HelpTopic of(@Nonnull String name
            , @Nonnull String description
            , int order
            , @Nonnull Collection<String> aliases
    ) {
        return new HelpTopic()
                .name(name)
                .description(description)
                .order(order)
                .aliases(aliases);
    }

    @Nonnull
    public static HelpTopic sub(@Nonnull String name
            , @Nonnull String description
            , int order
            , @Nonnull Collection<String> aliases
            , @Nonnull String detail
    ) {
        return new HelpTopic()
                .name(name)
                .description(description)
                .order(order)
                .aliases(aliases)
                .detail(detail);
    }

    @Nonnull
    public static HelpTopic opAdd(@Nonnull BaseInstructionsConfig base, @Nonnull String detail) {
        return sub("添加", "添加配置或规则", 1, base.add(), detail);
    }

    @Nonnull
    public static HelpTopic opDel(@Nonnull BaseInstructionsConfig base, @Nonnull String detail) {
        return sub("删除", "删除配置或规则", 2, base.del(), detail);
    }

    @Nonnull
    public static HelpTopic opList(@Nonnull BaseInstructionsConfig base, @Nonnull String detail) {
        return sub("查询", "查询已有配置或规则", 3, base.list(), detail);
    }

    @Nonnull
    public static HelpTopic opEnable(@Nonnull BaseInstructionsConfig base, @Nonnull String detail) {
        return sub("启用", "启用功能或规则", 4, base.enable(), detail);
    }

    @Nonnull
    public static HelpTopic opDisable(@Nonnull BaseInstructionsConfig base, @Nonnull String detail) {
        return sub("禁用", "禁用功能或规则", 5, base.disable(), detail);
    }

    @Nonnull
    public static HelpTopic opRefresh(@Nonnull BaseInstructionsConfig base, @Nonnull String detail) {
        return sub("刷新", "刷新缓存或重载配置", 6, base.refresh(), detail);
    }

    @Nonnull
    public static String joinAliases(@Nonnull Collection<String> aliases) {
        return String.join(", ", aliases);
    }

    /**
     * 格式化示例指令中的可选别名组：多个别名时用尖括号包裹，如 {@code <delcomment, 删除评论>}
     */
    @Nonnull
    public static String formatAliasChoices(@Nonnull Collection<String> aliases) {
        List<String> list = aliases.stream()
                .filter(StringUtils::isNotNullOrEmpty)
                .toList();
        if (list.isEmpty()) {
            return "";
        }
        if (list.size() == 1) {
            return list.getFirst();
        }
        return "<" + String.join(", ", list) + ">";
    }

}
