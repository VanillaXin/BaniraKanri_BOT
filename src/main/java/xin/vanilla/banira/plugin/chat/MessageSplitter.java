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

    private static final String PUNCTUATION = "。！？!?…｡\n";
    private static final Set<Character> CLOSING_QUOTES = Set.of('”', '"', '）', ')', '』', '」', '’', '\'');
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
            if (!CLOSING_QUOTES.contains(ch) && "，,。．！？!?…".indexOf(ch) < 0) {
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
                if (cuts.isEmpty() || cuts.getLast() != cut) {
                    cuts.add(cut);
                }
                i = cut - 1;
            }
        }

        List<String> parts = new ArrayList<>();
        int start = 0;

        final class Helper {
            int findBreakBackwards(String s, int pos, int limit) {
                for (int j = limit; j > pos; j--) {
                    char ch = s.charAt(j - 1);
                    if (Character.isWhitespace(ch) || SOFT_BREAKS.contains(ch)) {
                        return j;
                    }
                }
                return -1;
            }
        }
        Helper helper = new Helper();

        if (!cuts.isEmpty()) {
            for (int idx = 0; idx < cuts.size() && parts.size() < maxParts; idx++) {
                int cut = cuts.get(idx);
                if (cut - start <= maxCharsPerPart) {
                    parts.add(text.substring(start, cut).trim());
                    start = cut;
                } else {
                    int pos = start;
                    int regionEnd = cut;
                    while (pos < regionEnd && parts.size() < maxParts) {
                        int nextPos = Math.min(regionEnd, pos + maxCharsPerPart);
                        int breakPos = helper.findBreakBackwards(text, pos, nextPos);
                        if (breakPos == -1 || breakPos <= pos) {
                            breakPos = nextPos;
                        }
                        parts.add(text.substring(pos, breakPos).trim());
                        pos = breakPos;
                    }
                    start = cut;
                }
            }

            if (parts.size() < maxParts && start < text.length()) {
                String tail = text.substring(start);
                int pos = 0;
                while (pos < tail.length() && parts.size() < maxParts) {
                    int nextPos = Math.min(tail.length(), pos + maxCharsPerPart);
                    int breakPos = helper.findBreakBackwards(tail, pos, nextPos);
                    if (breakPos == -1 || breakPos <= pos) {
                        breakPos = nextPos;
                    }
                    parts.add(tail.substring(pos, breakPos).trim());
                    pos = breakPos;
                }
            }
        } else {
            int pos = 0;
            while (pos < text.length() && parts.size() < maxParts) {
                int nextPos = Math.min(text.length(), pos + maxCharsPerPart);
                int breakPos = helper.findBreakBackwards(text, pos, nextPos);
                if (breakPos == -1 || breakPos <= pos) {
                    breakPos = nextPos;
                }
                parts.add(text.substring(pos, breakPos).trim());
                pos = breakPos;
            }
        }

        if (parts.size() > maxParts) {
            parts = parts.subList(0, maxParts);
        }

        return parts.stream().filter(s -> !s.isEmpty()).collect(Collectors.toList());
    }
}
