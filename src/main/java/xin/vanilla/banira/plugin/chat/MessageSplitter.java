package xin.vanilla.banira.plugin.chat;

import xin.vanilla.banira.config.entity.extended.ChatReplySettings;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * 长消息拆分器
 */
public class MessageSplitter {
    private final ChatReplySettings cfg;

    private static final int DYNAMIC_SPLIT_MULTIPLIER = 2;
    private static final int MAX_DYNAMIC_SPLIT_PARTS = 20;
    private static final String PUNCTUATION = "。！？?!…｡\n";
    private static final Set<Character> CLOSING_QUOTES = Set.of('”', '"', '）', ')', '】', '』', '」', '’', '\'');
    private static final Set<Character> SOFT_BREAKS = Set.of('，', ',', '；', ';', '：', ':');

    public MessageSplitter(ChatReplySettings cfg) {
        this.cfg = cfg;
    }

    public List<String> split(String text) {
        if (cfg == null) {
            throw new IllegalStateException("ChatReplySettings is null");
        }
        return split(text, cfg.maxCharsPerPart(), cfg.maxSplitParts());
    }

    public static List<String> split(String text, int maxCharsPerPart, int maxParts) {
        if (text == null) {
            return Collections.emptyList();
        }
        text = text.replace("\r\n", "\n").trim();
        if (text.isEmpty()) {
            return Collections.emptyList();
        }
        if (maxCharsPerPart <= 0 || maxParts <= 0) {
            return Collections.emptyList();
        }
        maxParts = dynamicMaxPartsForLength(text.length(), maxCharsPerPart, maxParts);
        if (text.contains("\n")) {
            List<String> parts = new ArrayList<>();
            for (String line : normalizeLines(text)) {
                String trimmed = line.trim();
                if (trimmed.isEmpty()) {
                    continue;
                }
                parts.addAll(splitSingleLine(trimmed, maxCharsPerPart, maxParts - parts.size()));
                if (parts.size() >= maxParts) {
                    break;
                }
            }
            return parts.stream().filter(s -> !s.isEmpty()).collect(Collectors.toList());
        }
        return splitSingleLine(text, maxCharsPerPart, maxParts);
    }

    public static int dynamicMaxPartsForLength(int textLength, int maxCharsPerPart, int baseMaxParts) {
        if (textLength <= 0 || maxCharsPerPart <= 0 || baseMaxParts <= 0) {
            return 0;
        }
        int requiredParts = (int) Math.ceil((double) textLength / (double) maxCharsPerPart);
        int dynamicCeiling = Math.min(MAX_DYNAMIC_SPLIT_PARTS, baseMaxParts * DYNAMIC_SPLIT_MULTIPLIER);
        return Math.max(baseMaxParts, Math.min(requiredParts, dynamicCeiling));
    }

    public static int dynamicSpeechCharBudget(int maxCharsPerPart, int baseMaxParts) {
        if (maxCharsPerPart <= 0 || baseMaxParts <= 0) {
            return 0;
        }
        int dynamicCeiling = Math.min(MAX_DYNAMIC_SPLIT_PARTS, baseMaxParts * DYNAMIC_SPLIT_MULTIPLIER);
        return maxCharsPerPart * dynamicCeiling;
    }

    private static List<String> normalizeLines(String text) {
        List<String> lines = new ArrayList<>();
        for (String line : text.split("\\n+")) {
            String trimmed = line.trim();
            if (trimmed.isEmpty()) {
                continue;
            }
            if (!lines.isEmpty() && isDanglingClosingFragment(trimmed)) {
                int last = lines.size() - 1;
                lines.set(last, lines.get(last) + trimmed);
                continue;
            }
            lines.add(trimmed);
        }
        return lines;
    }

    private static boolean isDanglingClosingFragment(String text) {
        if (text.length() > 4) {
            return false;
        }
        for (int i = 0; i < text.length(); i++) {
            char ch = text.charAt(i);
            if (!CLOSING_QUOTES.contains(ch) && "，,。．；！？!?…｡".indexOf(ch) < 0) {
                return false;
            }
        }
        return true;
    }

    private static List<String> splitSingleLine(String text, int maxCharsPerPart, int maxParts) {
        if (maxParts <= 0) {
            return Collections.emptyList();
        }
        if (text.length() <= maxCharsPerPart) {
            return Collections.singletonList(text);
        }

        List<Integer> cuts = new ArrayList<>();
        for (int i = 0; i < text.length(); i++) {
            char c = text.charAt(i);
            if (PUNCTUATION.indexOf(c) >= 0) {
                int cut = i + 1;
                while (cut < text.length() && CLOSING_QUOTES.contains(text.charAt(cut))) {
                    cut++;
                }
                if (cuts.isEmpty() || !cuts.get(cuts.size() - 1).equals(cut)) {
                    cuts.add(cut);
                }
                i = cut - 1;
            }
        }

        List<String> parts = new ArrayList<>();
        int start = 0;

        if (!cuts.isEmpty()) {
            int lastGoodCut = -1;
            for (int idx = 0; idx < cuts.size() && parts.size() < maxParts; idx++) {
                int cut = cuts.get(idx);
                if (cut - start <= maxCharsPerPart) {
                    lastGoodCut = cut;
                    continue;
                }
                if (lastGoodCut > start) {
                    addPart(parts, text.substring(start, lastGoodCut));
                    start = lastGoodCut;
                    lastGoodCut = -1;
                    if (cut - start <= maxCharsPerPart) {
                        lastGoodCut = cut;
                        continue;
                    }
                }
                int pos = start;
                int regionEnd = cut;
                while (pos < regionEnd && parts.size() < maxParts) {
                    int breakPos = findBestBreak(text, pos, Math.min(regionEnd, pos + maxCharsPerPart));
                    addPart(parts, text.substring(pos, breakPos));
                    pos = breakPos;
                }
                start = cut;
            }
            if (parts.size() < maxParts && lastGoodCut > start) {
                addPart(parts, text.substring(start, lastGoodCut));
                start = lastGoodCut;
            }
        }

        if (parts.size() < maxParts && start < text.length()) {
            int pos = start;
            while (pos < text.length() && parts.size() < maxParts) {
                int breakPos = findBestBreak(text, pos, Math.min(text.length(), pos + maxCharsPerPart));
                addPart(parts, text.substring(pos, breakPos));
                pos = breakPos;
            }
        }

        return parts.stream().filter(s -> !s.isEmpty()).collect(Collectors.toList());
    }

    private static int findBestBreak(String s, int pos, int limit) {
        int breakPos = findBreakBackwards(s, pos, limit);
        return breakPos > pos ? breakPos : limit;
    }

    private static int findBreakBackwards(String s, int pos, int limit) {
        for (int j = limit; j > pos; j--) {
            char ch = s.charAt(j - 1);
            if (Character.isWhitespace(ch) || SOFT_BREAKS.contains(ch)) {
                return j;
            }
        }
        return -1;
    }

    private static void addPart(List<String> parts, String part) {
        String trimmed = part.trim();
        if (!trimmed.isEmpty()) {
            parts.add(trimmed);
        }
    }
}
