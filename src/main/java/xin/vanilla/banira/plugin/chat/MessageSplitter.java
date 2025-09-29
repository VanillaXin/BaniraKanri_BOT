package xin.vanilla.banira.plugin.chat;

import xin.vanilla.banira.config.entity.extended.ChatConfig;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * 长消息拆分器
 */
public class MessageSplitter {
    private final ChatConfig cfg;

    // 句尾标点
    private static final String PUNCTUATION = "。！？!?…｡\n";
    // 闭合标点
    private static final Set<Character> CLOSING_QUOTES = Set.of('”', '"', '）', ')', '』', '」', '’', '\'');
    // 软断点
    private static final Set<Character> SOFT_BREAKS = Set.of('，', ',', '；', ';', '：', ':');

    public MessageSplitter(ChatConfig cfg) {
        this.cfg = cfg;
    }

    /**
     * 使用 ChatConfig 的拆分文本为多个片段
     */
    public List<String> split(String text) {
        if (cfg == null) throw new IllegalStateException("ChatConfig is null");
        return split(text, cfg.maxCharsPerPart(), cfg.maxSplitParts());
    }

    /**
     * 根据长度拆分文本为多个片段
     *
     * @param text            原始文本
     * @param maxCharsPerPart 每片最大长度
     * @param maxParts        最多拆分为多少片
     */
    public static List<String> split(String text, int maxCharsPerPart, int maxParts) {
        if (text == null) return Collections.emptyList();
        // 规范换行
        text = text.replace("\r\n", "\n").trim();
        if (text.isEmpty()) return Collections.emptyList();
        if (maxCharsPerPart <= 0 || maxParts <= 0) return Collections.emptyList();
        if (text.length() <= maxCharsPerPart) return Collections.singletonList(text);

        List<Integer> cuts = new ArrayList<>();
        for (int i = 0; i < text.length(); i++) {
            char c = text.charAt(i);
            if (PUNCTUATION.indexOf(c) >= 0) {
                int cut = i + 1;
                // 把紧跟在句尾标点后的闭合引号/括号也包含进句尾
                while (cut < text.length() && CLOSING_QUOTES.contains(text.charAt(cut))) cut++;
                // 避免重复切点
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
                    // start .. cut 过长，需要在这个区间内分段
                    int pos = start;
                    int regionEnd = cut;
                    while (pos < regionEnd && parts.size() < maxParts) {
                        int nextPos = Math.min(regionEnd, pos + maxCharsPerPart);
                        int breakPos = helper.findBreakBackwards(text, pos, nextPos);
                        if (breakPos == -1 || breakPos <= pos) breakPos = nextPos; // 无软断点则硬切
                        parts.add(text.substring(pos, breakPos).trim());
                        pos = breakPos;
                    }
                    start = cut;
                }
            }

            // 处理最后剩余部分（最后一个切点之后的尾巴）
            if (parts.size() < maxParts && start < text.length()) {
                String tail = text.substring(start);
                int pos = 0;
                while (pos < tail.length() && parts.size() < maxParts) {
                    int nextPos = Math.min(tail.length(), pos + maxCharsPerPart);
                    int breakPos = helper.findBreakBackwards(tail, pos, nextPos);
                    if (breakPos == -1 || breakPos <= pos) breakPos = nextPos;
                    parts.add(tail.substring(pos, breakPos).trim());
                    pos = breakPos;
                }
            }
        } else {
            // 没有句尾标点：直接按长度分，但优先在软断点处断开以保留单词/短语完整性
            int pos = 0;
            while (pos < text.length() && parts.size() < maxParts) {
                int nextPos = Math.min(text.length(), pos + maxCharsPerPart);
                int breakPos = helper.findBreakBackwards(text, pos, nextPos);
                if (breakPos == -1 || breakPos <= pos) breakPos = nextPos;
                parts.add(text.substring(pos, breakPos).trim());
                pos = breakPos;
            }
        }

        // 最多保留 maxParts
        if (parts.size() > maxParts) {
            parts = parts.subList(0, maxParts);
        }

        return parts.stream().filter(s -> !s.isEmpty()).collect(Collectors.toList());
    }
}
