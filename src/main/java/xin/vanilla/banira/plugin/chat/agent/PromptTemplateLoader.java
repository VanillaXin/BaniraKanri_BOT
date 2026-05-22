package xin.vanilla.banira.plugin.chat.agent;

import jakarta.annotation.Nonnull;
import xin.vanilla.banira.util.StringUtils;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Loads prompt templates from resources so behavior rules are not embedded in Java code.
 */
public final class PromptTemplateLoader {

    private PromptTemplateLoader() {
    }

    @Nonnull
    public static List<String> loadSections(@Nonnull String resourcePath) {
        String content = load(resourcePath);
        if (StringUtils.isNullOrEmptyEx(content)) {
            return List.of();
        }
        List<String> sections = new ArrayList<>();
        for (String section : content.split("(?m)^---\\s*$")) {
            String trimmed = section.trim();
            if (StringUtils.isNotNullOrEmpty(trimmed)) {
                sections.add(trimmed);
            }
        }
        return sections;
    }

    @Nonnull
    public static String render(@Nonnull String resourcePath, @Nonnull Map<String, String> variables) {
        String content = load(resourcePath);
        if (StringUtils.isNullOrEmptyEx(content)) {
            return "";
        }
        String result = content;
        for (Map.Entry<String, String> entry : variables.entrySet()) {
            String value = entry.getValue() != null ? entry.getValue() : "";
            result = result.replace("{{" + entry.getKey() + "}}", value);
        }
        return result.trim();
    }

    @Nonnull
    private static String load(@Nonnull String resourcePath) {
        ClassLoader loader = PromptTemplateLoader.class.getClassLoader();
        try (InputStream input = loader.getResourceAsStream(resourcePath)) {
            if (input == null) {
                return "";
            }
            return new String(input.readAllBytes(), StandardCharsets.UTF_8);
        } catch (IOException ignored) {
            return "";
        }
    }
}
