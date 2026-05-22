package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 解析 AI 回复中的台词与参考资料块
 */
public final class ReplyContentParser {

    private static final Pattern REF_BLOCK = Pattern.compile("\\[REF\\](.*?)\\[/REF\\]", Pattern.DOTALL | Pattern.CASE_INSENSITIVE);
    private static final Pattern AT_DIRECTIVE = Pattern.compile("(?i)(?:\\[\\s*AT\\s*[:=]\\s*(?:qq\\s*=\\s*)?(\\d{5,12})\\s*]|(?<![\\p{Alnum}_])AT\\s*[:=]\\s*(?:qq\\s*=\\s*)?(\\d{5,12})|\\[CQ:at,qq=(\\d{5,12})])");
    private static final Pattern REPLY_DIRECTIVE = Pattern.compile("(?i)(?:\\[\\s*REPLY\\s*[:=]\\s*(?:id\\s*=\\s*)?(\\d+)\\s*]|(?<![\\p{Alnum}_])REPLY\\s*[:=]\\s*(?:id\\s*=\\s*)?(\\d+))");

    private ReplyContentParser() {
    }

    @Nonnull
    public static StructuredReply parse(@Nonnull String raw, @Nonnull List<String> autoReferences) {
        List<String> references = new ArrayList<>();
        Matcher matcher = REF_BLOCK.matcher(raw);
        StringBuilder speechBuilder = new StringBuilder();
        int lastEnd = 0;
        while (matcher.find()) {
            speechBuilder.append(raw, lastEnd, matcher.start());
            String ref = matcher.group(1);
            if (StringUtils.isNotNullOrEmpty(ref)) {
                references.add(ref.trim());
            }
            lastEnd = matcher.end();
        }
        speechBuilder.append(raw.substring(lastEnd));
        for (String auto : autoReferences) {
            if (StringUtils.isNotNullOrEmpty(auto)) {
                references.add(auto.trim());
            }
        }
        List<String> dedupedReferences = dedupeReferences(references);
        ParsedDirectives directives = parseDirectives(speechBuilder.toString());
        return new StructuredReply(directives.speech(), dedupedReferences, directives.replyToMessageId(), directives.atTargets(), false);
    }

    @Nonnull
    private static ParsedDirectives parseDirectives(@Nonnull String rawSpeech) {
        List<Long> atTargets = new ArrayList<>();
        Matcher atMatcher = AT_DIRECTIVE.matcher(rawSpeech);
        while (atMatcher.find()) {
            long qq = firstLongGroup(atMatcher);
            if (qq > 0 && !atTargets.contains(qq)) {
                atTargets.add(qq);
            }
        }

        Integer replyToMessageId = null;
        Matcher replyMatcher = REPLY_DIRECTIVE.matcher(rawSpeech);
        if (replyMatcher.find()) {
            int msgId = (int) firstLongGroup(replyMatcher);
            if (msgId > 0) {
                replyToMessageId = msgId;
            }
        }

        String speech = AT_DIRECTIVE.matcher(rawSpeech).replaceAll("");
        speech = REPLY_DIRECTIVE.matcher(speech).replaceAll("");
        return new ParsedDirectives(speech.trim(), atTargets, replyToMessageId);
    }

    private static long firstLongGroup(@Nonnull Matcher matcher) {
        for (int i = 1; i <= matcher.groupCount(); i++) {
            String value = matcher.group(i);
            if (StringUtils.isNotNullOrEmpty(value)) {
                return xin.vanilla.banira.util.StringUtils.toLong(value, 0);
            }
        }
        return 0;
    }

    @Nonnull
    private static List<String> dedupeReferences(@Nonnull List<String> references) {
        LinkedHashSet<String> set = new LinkedHashSet<>();
        for (String reference : references) {
            if (StringUtils.isNullOrEmptyEx(reference)) {
                continue;
            }
            String normalized = reference.trim();
            if (StructuredReplyPipeline.isUnsuitableForwardReferenceText(normalized)) {
                continue;
            }
            boolean duplicated = set.stream().anyMatch(existing -> isSimilar(existing, normalized));
            if (!duplicated) {
                set.add(normalized);
            }
        }
        return new ArrayList<>(set);
    }

    private static boolean isSimilar(@Nonnull String a, @Nonnull String b) {
        if (a.equals(b)) {
            return true;
        }
        String shortText = a.length() <= b.length() ? a : b;
        String longText = a.length() > b.length() ? a : b;
        return longText.contains(shortText) && shortText.length() >= Math.min(120, longText.length() / 2);
    }

    private record ParsedDirectives(@Nonnull String speech, @Nonnull List<Long> atTargets, Integer replyToMessageId) {
    }

}
