package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import xin.vanilla.banira.config.entity.extended.ChatGuardSettings;
import xin.vanilla.banira.plugin.chat.agent.PromptTemplateLoader;
import xin.vanilla.banira.util.StringUtils;

import java.util.*;

/**
 * Merged, read-only view of chat guard rules.
 */
public final class GuardRuleSet {

    private final Map<String, List<String>> sections;

    private GuardRuleSet(@Nonnull Map<String, List<String>> sections) {
        this.sections = sections;
    }

    @Nonnull
    public static GuardRuleSet from(@Nullable ChatGuardSettings settings) {
        ChatGuardSettings effective = settings != null ? settings : new ChatGuardSettings();
        Map<String, List<String>> merged = new LinkedHashMap<>();
        List<String> resources = effective.resourceRulePaths() != null
                ? effective.resourceRulePaths()
                : new ChatGuardSettings().resourceRulePaths();
        for (String resource : resources) {
            if (StringUtils.isNullOrEmptyEx(resource)) {
                continue;
            }
            parseResource(resource).forEach((key, values) ->
                    merged.computeIfAbsent(key, ignored -> new ArrayList<>()).addAll(values));
        }
        if (effective.overrideRules() != null) {
            effective.overrideRules().forEach((key, values) -> {
                if (StringUtils.isNotNullOrEmpty(key)) {
                    merged.put(normalizeSection(key), clean(values));
                }
            });
        }
        if (effective.appendRules() != null) {
            effective.appendRules().forEach((key, values) -> {
                if (StringUtils.isNotNullOrEmpty(key)) {
                    merged.computeIfAbsent(normalizeSection(key), ignored -> new ArrayList<>()).addAll(clean(values));
                }
            });
        }
        if (effective.selfIntroTemplates() != null && !effective.selfIntroTemplates().isEmpty()) {
            merged.put("replytemplates", clean(effective.selfIntroTemplates()));
        }
        if (effective.unnamedSelfIntroTemplates() != null && !effective.unnamedSelfIntroTemplates().isEmpty()) {
            merged.put("unnamedreplytemplates", clean(effective.unnamedSelfIntroTemplates()));
        }
        if (effective.identityReplies() != null && !effective.identityReplies().isEmpty()) {
            merged.put("denyreplies", clean(effective.identityReplies()));
        }
        if (effective.identityConfusedReplies() != null && !effective.identityConfusedReplies().isEmpty()) {
            merged.put("confusedreplies", clean(effective.identityConfusedReplies()));
        }
        if (StringUtils.isNotNullOrEmpty(effective.promptLeakRefusal())) {
            merged.put("refusaltext", List.of(effective.promptLeakRefusal().trim()));
        }
        Map<String, List<String>> immutable = new LinkedHashMap<>();
        merged.forEach((key, values) -> immutable.put(key, List.copyOf(values)));
        return new GuardRuleSet(Map.copyOf(immutable));
    }

    @Nonnull
    public List<String> lines(@Nonnull String section, @Nonnull List<String> fallback) {
        List<String> values = sections.get(normalizeSection(section));
        return values == null || values.isEmpty() ? fallback : values;
    }

    @Nonnull
    public String firstLine(@Nonnull String section, @Nonnull String fallback) {
        List<String> values = lines(section, List.of());
        return values.isEmpty() ? fallback : values.getFirst();
    }

    @Nonnull
    private static Map<String, List<String>> parseResource(@Nonnull String resourcePath) {
        String content = PromptTemplateLoader.render(resourcePath, Map.of());
        if (StringUtils.isNullOrEmptyEx(content)) {
            return Map.of();
        }
        Map<String, List<String>> parsed = new LinkedHashMap<>();
        String current = "default";
        for (String rawLine : content.split("\\R")) {
            String line = rawLine.trim();
            if (line.isEmpty() || line.startsWith("#")) {
                continue;
            }
            if (line.startsWith("[") && line.endsWith("]")) {
                current = normalizeSection(line.substring(1, line.length() - 1));
                continue;
            }
            parsed.computeIfAbsent(current, ignored -> new ArrayList<>()).add(line);
        }
        return parsed;
    }

    @Nonnull
    private static List<String> clean(@Nullable List<String> values) {
        if (values == null || values.isEmpty()) {
            return List.of();
        }
        List<String> cleaned = new ArrayList<>();
        for (String value : values) {
            if (StringUtils.isNotNullOrEmpty(value)) {
                cleaned.add(value.trim());
            }
        }
        return cleaned;
    }

    @Nonnull
    private static String normalizeSection(@Nonnull String section) {
        return section.trim().toLowerCase(Locale.ROOT);
    }
}
