package xin.vanilla.banira.util;

import java.util.*;

/**
 * 基于 Aho-Corasick 的多模式匹配
 *
 * @param <T> 匹配结果类型
 */
public class AhoCorasick<T> {
    private final Node<T> root = new Node<>();
    private boolean built = false;

    /**
     * 将 keyword 加入自动机，并把 value 与 keyword 长度一起保存在终止节点。
     *
     * @param keyword 模式串（用于构造 trie）
     * @param value   与该模式相关联的值（搜索时返回）
     */
    public void addKeyword(String keyword, T value) {
        if (keyword == null || keyword.isEmpty()) return;
        Node<T> current = root;
        for (char c : keyword.toCharArray()) {
            current = current.children.computeIfAbsent(c, k -> new Node<>());
        }
        // 保存 value -> length 映射（便于 later 返回位置）
        current.values.put(value, keyword.length());
    }

    public void buildFailureLinks() {
        Queue<Node<T>> queue = new LinkedList<>();

        // 第一层节点的 failure 指向 root
        for (Node<T> child : root.children.values()) {
            child.failure = root;
            queue.add(child);
        }

        // 构建剩余节点的 failure 链接
        while (!queue.isEmpty()) {
            Node<T> current = queue.poll();

            for (Map.Entry<Character, Node<T>> entry : current.children.entrySet()) {
                char c = entry.getKey();
                Node<T> child = entry.getValue();

                Node<T> failure = current.failure;
                while (failure != null && !failure.children.containsKey(c)) {
                    failure = failure.failure;
                }

                if (failure != null && failure.children.containsKey(c)) {
                    child.failure = failure.children.get(c);
                    // 合并输出（value->length）
                    child.values.putAll(child.failure.values);
                } else {
                    child.failure = root;
                }

                queue.add(child);
            }
        }

        built = true;
    }

    /**
     * 搜索并返回所有匹配
     */
    public Collection<T> search(String text) {
        if (!built) {
            throw new IllegalStateException("Automaton not built");
        }
        Set<T> results = new HashSet<>();
        if (text == null || text.isEmpty()) return results;

        Node<T> current = root;
        for (char c : text.toCharArray()) {
            while (current != root && !current.children.containsKey(c)) {
                current = current.failure;
            }
            if (current.children.containsKey(c)) {
                current = current.children.get(c);
                results.addAll(current.values.keySet());
            } else {
                current = root;
            }
        }
        return results;
    }

    /**
     * 带位置信息的搜索，返回所有匹配（可能重叠）
     */
    public List<Match<T>> searchWithPositions(String text) {
        if (!built) {
            throw new IllegalStateException("Automaton not built");
        }
        List<Match<T>> matches = new ArrayList<>();
        if (text == null || text.isEmpty()) return matches;

        Node<T> current = root;
        char[] arr = text.toCharArray();
        for (int i = 0; i < arr.length; i++) {
            char c = arr[i];
            while (current != root && !current.children.containsKey(c)) {
                current = current.failure;
            }
            current = current.children.getOrDefault(c, root);

            if (!current.values.isEmpty()) {
                // 对每个 value，知道其 length，计算 start = i - len + 1
                for (Map.Entry<T, Integer> ent : current.values.entrySet()) {
                    T value = ent.getKey();
                    int len = ent.getValue();
                    int start = i - len + 1;
                    if (start >= 0) {
                        matches.add(new Match<>(start, start + len, value));
                    }
                }
            }
        }
        return matches;
    }


    private static class Node<T> {
        final Map<Character, Node<T>> children = new HashMap<>();
        // value -> length
        final Map<T, Integer> values = new HashMap<>();
        Node<T> failure;
    }

    /**
     * 匹配结果（包含位置）
     *
     * @param start inclusive
     * @param end   exclusive
     */
    public record Match<T>(int start, int end, T value) {
        @Override
        public String toString() {
            return "Match{" + "start=" + start + ", end=" + end + ", value=" + value + '}';
        }
    }
}
