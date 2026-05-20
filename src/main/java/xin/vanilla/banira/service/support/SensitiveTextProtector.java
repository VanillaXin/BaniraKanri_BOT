package xin.vanilla.banira.service.support;

import xin.vanilla.banira.util.PlantCipher;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 在敏感词扫描前保护 URL、CQ 码、已有 Plant 密文，避免误替换破坏结构。
 */
public final class SensitiveTextProtector {

    private static final String PLACEHOLDER_PREFIX = "\uE000BK";
    private static final String PLACEHOLDER_SUFFIX = "\uE001";

    private static final Pattern CQ_PATTERN = Pattern.compile("\\[CQ:[^\\]]+]", Pattern.CASE_INSENSITIVE);
    private static final Pattern URL_PATTERN = Pattern.compile("(?i)(?:https?|file)://[^\\s\\]<>\"']+");

    private SensitiveTextProtector() {
    }

    public record ProtectResult(String protectedText, List<String> segments) {
    }

    public static ProtectResult protect(String text) {
        if (StringUtils.isNullOrEmpty(text)) {
            return new ProtectResult(text, List.of());
        }
        List<Segment> segments = new ArrayList<>();
        collectMatches(text, CQ_PATTERN, segments);
        collectMatches(text, URL_PATTERN, segments);
        collectPlantTokens(text, segments);
        if (segments.isEmpty()) {
            return new ProtectResult(text, List.of());
        }
        segments.sort(Comparator.comparingInt(Segment::start));
        List<Segment> merged = mergeNonOverlapping(segments);
        List<String> originals = new ArrayList<>(merged.size());
        StringBuilder sb = new StringBuilder(text.length());
        int cursor = 0;
        int index = 0;
        for (Segment segment : merged) {
            if (segment.start > cursor) {
                sb.append(text, cursor, segment.start());
            }
            originals.add(segment.value);
            sb.append(placeholder(index++));
            cursor = segment.end();
        }
        if (cursor < text.length()) {
            sb.append(text, cursor, text.length());
        }
        return new ProtectResult(sb.toString(), originals);
    }

    public static String restore(String protectedText, List<String> segments) {
        if (StringUtils.isNullOrEmpty(protectedText) || segments == null || segments.isEmpty()) {
            return protectedText;
        }
        String result = protectedText;
        for (int i = 0; i < segments.size(); i++) {
            result = result.replace(placeholder(i), segments.get(i));
        }
        return result;
    }

    private static void collectMatches(String text, Pattern pattern, List<Segment> segments) {
        Matcher matcher = pattern.matcher(text);
        while (matcher.find()) {
            segments.add(new Segment(matcher.start(), matcher.end(), matcher.group()));
        }
    }

    private static void collectPlantTokens(String text, List<Segment> segments) {
        if (PlantCipher.isPlantToken(text)) {
            segments.add(new Segment(0, text.length(), text));
            return;
        }
        for (var locator : PlantCipher.LOCATOR) {
            String prefix = locator.getKey();
            String suffix = locator.getValue();
            int from = 0;
            while (from < text.length()) {
                int start = text.indexOf(prefix, from);
                if (start < 0) {
                    break;
                }
                int end = text.indexOf(suffix, start + prefix.length());
                if (end < 0) {
                    break;
                }
                end += suffix.length();
                String candidate = text.substring(start, end);
                if (PlantCipher.isPlantToken(candidate)) {
                    segments.add(new Segment(start, end, candidate));
                    from = end;
                } else {
                    from = start + 1;
                }
            }
        }
    }

    private static List<Segment> mergeNonOverlapping(List<Segment> segments) {
        List<Segment> merged = new ArrayList<>();
        int lastEnd = -1;
        for (Segment segment : segments) {
            if (segment.start >= lastEnd) {
                merged.add(segment);
                lastEnd = segment.end;
            }
        }
        return merged;
    }

    private static String placeholder(int index) {
        return PLACEHOLDER_PREFIX + index + PLACEHOLDER_SUFFIX;
    }

    private record Segment(int start, int end, String value) {
    }

}
