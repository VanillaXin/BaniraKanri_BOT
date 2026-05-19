package xin.vanilla.banira.plugin.help;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import xin.vanilla.banira.util.StringUtils;

import java.util.*;

/**
 * 帮助主题，支持功能与子功能两级结构
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
public class HelpTopic {

    private String name = "";
    private List<String> aliases = new ArrayList<>();
    private String description = "";
    private int order = 100;
    /**
     * 所属插件类名，用于 capability 过滤
     */
    private String ownerClass = "";
    /**
     * 无子功能或选中时的详细用法
     */
    private String detail = "";
    private List<HelpTopic> children = new ArrayList<>();

    @Nonnull
    public HelpTopic aliases(@Nonnull Collection<String> values) {
        values.stream()
                .filter(StringUtils::isNotNullOrEmpty)
                .forEach(aliases::add);
        return this;
    }

    @Nonnull
    public HelpTopic alias(@Nonnull String... values) {
        return aliases(List.of(values));
    }

    @Nonnull
    public HelpTopic child(@Nonnull HelpTopic topic) {
        children.add(topic);
        return this;
    }

    @Nonnull
    public Set<String> normalizedAliases() {
        Set<String> result = new LinkedHashSet<>();
        if (StringUtils.isNotNullOrEmpty(name)) {
            result.add(normalize(name));
        }
        aliases.stream()
                .filter(StringUtils::isNotNullOrEmpty)
                .map(HelpTopic::normalize)
                .forEach(result::add);
        return result;
    }

    @Nonnull
    public List<String> displayAliases() {
        Set<String> display = new LinkedHashSet<>();
        if (StringUtils.isNotNullOrEmpty(name)) {
            display.add(name);
        }
        aliases.stream()
                .filter(StringUtils::isNotNullOrEmpty)
                .forEach(display::add);
        return display.stream().sorted().toList();
    }

    public boolean matches(@Nullable String alias) {
        if (StringUtils.isNullOrEmptyEx(alias)) {
            return false;
        }
        return normalizedAliases().contains(normalize(alias));
    }

    @Nonnull
    public Optional<HelpTopic> findChild(@Nullable String alias) {
        if (StringUtils.isNullOrEmptyEx(alias)) {
            return Optional.empty();
        }
        return children.stream()
                .filter(child -> child.matches(alias))
                .min(Comparator.comparingInt((HelpTopic t) -> t.order())
                        .thenComparing((HelpTopic t) -> t.name()));
    }

    @Nonnull
    public List<HelpTopic> sortedChildren() {
        return children.stream()
                .sorted(Comparator.comparingInt((HelpTopic t) -> t.order())
                        .thenComparing((HelpTopic t) -> t.name()))
                .toList();
    }

    @Nonnull
    public static String normalize(@Nullable String value) {
        if (value == null) {
            return "";
        }
        return value.trim().toLowerCase(Locale.ROOT);
    }

}
