package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import xin.vanilla.banira.util.StringUtils;

import java.text.Normalizer;
import java.util.Locale;
import java.util.regex.Pattern;

public final class ChatInputSanitizer {

    private static final Pattern CONTROL_AND_FORMAT = Pattern.compile("[\\p{Cntrl}\\p{Cf}&&[^\\r\\n\\t]]");
    private static final Pattern RESERVED_BLOCK = Pattern.compile("(?i)\\[\\s*/?\\s*(ENGAGE|PREFLIGHT|REF|AT|REPLY|TOOL|SYSTEM|USER|ASSISTANT)(?:\\s*[:|][^]]*)?]");
    private static final Pattern XMLISH_ROLE = Pattern.compile("(?i)</?\\s*(system|developer|assistant|user|tool|identity|guardrails|priority|instructions)\\b[^>]*>");

    private ChatInputSanitizer() {
    }

    @Nonnull
    public static String sanitizeUserText(@Nullable String text) {
        if (StringUtils.isNullOrEmpty(text)) {
            return "";
        }
        String cleaned = normalize(text);
        cleaned = RESERVED_BLOCK.matcher(cleaned).replaceAll(match -> fullwidth(match.group()));
        cleaned = XMLISH_ROLE.matcher(cleaned).replaceAll(match -> fullwidth(match.group()));
        return cleaned;
    }

    @Nonnull
    public static String sanitizeInlineName(@Nullable String value) {
        if (StringUtils.isNullOrEmpty(value)) {
            return "";
        }
        String cleaned = normalize(value)
                .replaceAll("[\"'`\\\\\\[\\]{}<>|]", " ")
                .replaceAll("\\s+", " ")
                .trim();
        return cleaned;
    }

    @Nonnull
    private static String normalize(@Nonnull String text) {
        String normalized = Normalizer.normalize(text, Normalizer.Form.NFKC);
        return CONTROL_AND_FORMAT.matcher(normalized)
                .replaceAll("")
                .replaceAll("[\\r\\n\\t]+", " ")
                .replaceAll("\\s{2,}", " ")
                .trim();
    }

    @Nonnull
    private static String fullwidth(@Nonnull String text) {
        StringBuilder builder = new StringBuilder(text.length());
        for (int i = 0; i < text.length(); i++) {
            char c = text.charAt(i);
            if (c == '[') {
                builder.append('［');
            } else if (c == ']') {
                builder.append('］');
            } else if (c == '<') {
                builder.append('＜');
            } else if (c == '>') {
                builder.append('＞');
            } else {
                builder.append(c);
            }
        }
        return builder.toString().toLowerCase(Locale.ROOT).contains("ignore previous")
                ? builder.toString().replaceAll("(?i)ignore previous", "ignore_previous")
                : builder.toString();
    }
}
