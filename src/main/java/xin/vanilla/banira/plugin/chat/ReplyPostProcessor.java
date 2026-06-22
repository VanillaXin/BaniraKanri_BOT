package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;
import xin.vanilla.banira.config.entity.extended.ChatReplySettings;
import xin.vanilla.banira.util.StringUtils;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 回复后处理：长度截断等
 */
public class ReplyPostProcessor {

    private static final Pattern EMOJI = Pattern.compile("[\\x{1F000}-\\x{1FAFF}\\x{2600}-\\x{27BF}]");
    private static final Pattern FINAL_PERIOD = Pattern.compile("[。．.]+(?=\\s*$)");
    private static final Pattern CUTE_SUFFIX = Pattern.compile("[呀啦哦呢哇呐喔噢]+(?=\\s*$)");
    private static final Pattern TRAILING_DECOR = Pattern.compile("[~～♪★☆✨]+(?=\\s*$)");
    private static final Pattern TRAILING_PRAISE = Pattern.compile(
            "(?:呢|啊)?(?:真的(?:很)?(?:太)?(?:好|棒|厉害|优秀|可爱|温柔|贴心|完美)|(?:太|超)(?:好|棒|厉害|可爱)|好厉害|好棒|太好了)(?=\\s*$)"
    );
    private static final Pattern DASH_RUN = Pattern.compile("\\s*[—–]{1,}\\s*");
    private static final Pattern LEADING_DASH_DECOR = Pattern.compile("(?m)^\\s*[—–]{1,}\\s*");
    private static final Pattern TRAILING_DASH_DECOR = Pattern.compile("(?m)\\s*[—–]{1,}\\s*$");
    private static final Pattern DANGLING_CLOSING_QUOTE_LINE = Pattern.compile("(?m)([^\\n])\\n\\s*([”\"』」’']+)\\s*(?=\\n|$)");
    private static final Pattern INTERNAL_META_LINE = Pattern.compile(
            "(?im)^\\s*\\[(?:ENGAGE|PREFLIGHT)\\b[^\\n\\]]*]\\s*$"
    );

    private ReplyPostProcessor() {
    }

    @Nonnull
    public static String process(@Nonnull String reply, @Nonnull ChatReplySettings cfg) {
        if (StringUtils.isNullOrEmptyEx(reply)) {
            return "";
        }
        String text = normalizeStyle(reply.trim(), cfg);
        int maxChars = hardReplyFallbackLimit(cfg);
        if (maxChars <= 0 || text.length() <= maxChars) {
            return text;
        }
        return normalizeStyle(truncate(text, maxChars), cfg);
    }

    @Nonnull
    public static String processPart(@Nonnull String part, @Nonnull ChatReplySettings cfg) {
        if (StringUtils.isNullOrEmptyEx(part)) {
            return "";
        }
        return normalizeStyle(part.trim(), cfg);
    }

    @Nonnull
    private static String normalizeStyle(@Nonnull String text, @Nonnull ChatReplySettings cfg) {
        text = stripInternalMetaArtifacts(text);
        text = stripMarkdownArtifacts(text);
        text = stripAddresseeStageDirections(text);
        text = normalizeDashStyle(text);
        text = mergeDanglingClosingQuotes(text);
        text = stripCuteDecorations(text);
        text = limitEmoji(text, cfg.maxEmojiPerReply());
        if (cfg.stripFinalSentencePeriod()) {
            text = stripSentencePeriods(text);
        }
        return text.trim();
    }

    @Nonnull
    static String stripMarkdownArtifacts(@Nonnull String text) {
        if (StringUtils.isNullOrEmptyEx(text)) {
            return "";
        }
        String normalized = text.replace("\r\n", "\n").replace('\r', '\n');
        normalized = normalized.replaceAll("(?m)^\\s*```[a-zA-Z0-9_+\\-.#]*\\s*$", "");
        normalized = normalized.replaceAll("(?m)^\\s*```\\s*$", "");
        normalized = normalized.replaceAll("(?m)^\\s{0,3}#{1,6}\\s+", "");
        normalized = normalized.replaceAll("(?m)^\\s{0,3}>\\s?", "");
        normalized = normalized.replaceAll("(?m)^\\s*[-*+]\\s+", "");
        normalized = normalized.replaceAll("(?m)^\\s*\\d+[.)、]\\s+", "");
        normalized = normalized.replaceAll("(?m)^\\s*\\*{2,}\\s*$", "");
        normalized = normalized.replaceAll("(?m)^\\s*_{2,}\\s*$", "");
        normalized = normalized.replaceAll("\\*{2,}([^*\\n]+?)\\*{2,}", "$1");
        normalized = normalized.replaceAll("_{2,}([^_\\n]+?)_{2,}", "$1");
        normalized = normalized.replaceAll("`([^`\\n]{1,120})`", "$1");
        normalized = normalized.replaceAll("\\n{3,}", "\n\n");
        return normalized.trim();
    }

    @Nonnull
    static String stripInternalMetaArtifacts(@Nonnull String text) {
        if (StringUtils.isNullOrEmptyEx(text)) {
            return "";
        }
        return INTERNAL_META_LINE.matcher(text.replace("\r\n", "\n").replace('\r', '\n')).replaceAll("").trim();
    }

    @Nonnull
    static String stripAddresseeStageDirections(@Nonnull String text) {
        if (StringUtils.isNullOrEmptyEx(text)) {
            return "";
        }
        return ADDRESSEE_STAGE_DIRECTION.matcher(text).replaceAll("").trim();
    }

    @Nonnull
    static String normalizeDashStyle(@Nonnull String text) {
        if (StringUtils.isNullOrEmptyEx(text)) {
            return "";
        }
        String normalized = LEADING_DASH_DECOR.matcher(text).replaceAll("");
        normalized = TRAILING_DASH_DECOR.matcher(normalized).replaceAll("");
        normalized = DASH_RUN.matcher(normalized).replaceAll("，");
        normalized = normalized.replaceAll("，{2,}", "，");
        normalized = normalized.replaceAll("(?m)，\\s*$", "");
        return normalized.trim();
    }

    @Nonnull
    static String mergeDanglingClosingQuotes(@Nonnull String text) {
        if (StringUtils.isNullOrEmptyEx(text)) {
            return "";
        }
        String normalized = text;
        String previous;
        do {
            previous = normalized;
            normalized = DANGLING_CLOSING_QUOTE_LINE.matcher(normalized).replaceAll("$1$2");
        } while (!previous.equals(normalized));
        return normalized.trim();
    }

    @Nonnull
    static String stripCuteDecorations(@Nonnull String text) {
        if (StringUtils.isNullOrEmptyEx(text)) {
            return "";
        }
        String[] lines = text.replace("\r\n", "\n").split("\\n", -1);
        for (int i = 0; i < lines.length; i++) {
            String line = lines[i].trim();
            line = TRAILING_DECOR.matcher(line).replaceAll("");
            line = CUTE_SUFFIX.matcher(line).replaceAll("");
            line = TRAILING_PRAISE.matcher(line).replaceAll("");
            lines[i] = line;
        }
        return String.join("\n", lines).trim();
    }

    private static final Pattern ADDRESSEE_STAGE_DIRECTION = Pattern.compile(
            "(?m)^\\s*[（(]\\s*(?:对|回复|给|跟)\\s*[^）)\\n]{1,32}?\\s*(?:说)?\\s*[）)]\\s*"
    );

    @Nonnull
    private static String stripSentencePeriods(@Nonnull String text) {
        StringBuilder normalized = new StringBuilder(text.length());
        for (int i = 0; i < text.length(); i++) {
            char c = text.charAt(i);
            if (c == '。' || c == '．') {
                int next = i + 1;
                while (next < text.length() && Character.isWhitespace(text.charAt(next)) && text.charAt(next) != '\n') {
                    next++;
                }
                if (next < text.length() && "”\"』」’'".indexOf(text.charAt(next)) >= 0) {
                    continue;
                }
                normalized.append('\n');
                continue;
            }
            normalized.append(c);
        }
        text = normalized.toString();
        String[] lines = text.split("\\n", -1);
        for (int i = 0; i < lines.length; i++) {
            lines[i] = FINAL_PERIOD.matcher(lines[i].trim()).replaceAll("");
        }
        return String.join("\n", lines);
    }

    @Nonnull
    private static String limitEmoji(@Nonnull String text, int maxEmoji) {
        if (maxEmoji < 0) {
            maxEmoji = 0;
        }
        Matcher matcher = EMOJI.matcher(text);
        StringBuilder builder = new StringBuilder();
        int last = 0;
        int kept = 0;
        while (matcher.find()) {
            builder.append(text, last, matcher.start());
            if (kept < maxEmoji) {
                builder.append(matcher.group());
                kept++;
            }
            last = matcher.end();
        }
        builder.append(text.substring(last));
        return builder.toString();
    }

    private static int hardReplyFallbackLimit(@Nonnull ChatReplySettings cfg) {
        int softLimit = cfg.maxReplyChars();
        if (softLimit <= 0) {
            return 0;
        }
        int dynamicBudget = MessageSplitter.dynamicSpeechCharBudget(cfg.maxCharsPerPart(), cfg.maxSplitParts());
        int forwardLimit = Math.max(100, cfg.maxForwardLength());
        return Math.max(softLimit, Math.max(dynamicBudget, forwardLimit));
    }

    @Nonnull
    private static String truncate(@Nonnull String text, int maxChars) {
        int cut = maxChars;
        for (int i = Math.min(maxChars, text.length()) - 1; i >= Math.max(0, maxChars - 20); i--) {
            char c = text.charAt(i);
            if ("。！？!?…\n".indexOf(c) >= 0) {
                cut = i + 1;
                break;
            }
        }
        if (cut <= 0) {
            cut = maxChars;
        }
        return text.substring(0, Math.min(cut, text.length())).trim();
    }

}
