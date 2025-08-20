package xin.vanilla.banira.util;

import java.util.*;

/**
 * 基于Aho-Corasick算法的关键词匹配
 *
 * @param <T> 匹配结果类型
 */
public class AhoCorasick<T> {
    private final Node<T> root = new Node<>();
    private boolean built = false;

    public void addKeyword(String keyword, T value) {
        Node<T> current = root;
        for (char c : keyword.toCharArray()) {
            current = current.children.computeIfAbsent(c, k -> new Node<>());
        }
        current.values.add(value);
    }

    public void buildFailureLinks() {
        Queue<Node<T>> queue = new LinkedList<>();

        // 第一层节点的failure指向root
        for (Node<T> child : root.children.values()) {
            child.failure = root;
            queue.add(child);
        }

        // 构建剩余节点的failure链接
        while (!queue.isEmpty()) {
            Node<T> current = queue.poll();

            for (Map.Entry<Character, Node<T>> entry : current.children.entrySet()) {
                char c = entry.getKey();
                Node<T> child = entry.getValue();

                Node<T> failure = current.failure;
                while (failure != root && !failure.children.containsKey(c)) {
                    failure = failure.failure;
                }

                if (failure.children.containsKey(c)) {
                    child.failure = failure.children.get(c);
                    // 合并输出
                    child.values.addAll(child.failure.values);
                } else {
                    child.failure = root;
                }

                queue.add(child);
            }
        }

        built = true;
    }

    public Collection<T> search(String text) {
        if (!built) {
            throw new IllegalStateException("Automaton not built");
        }

        Set<T> results = new HashSet<>();
        Node<T> current = root;

        for (char c : text.toCharArray()) {
            while (current != root && !current.children.containsKey(c)) {
                current = current.failure;
            }

            if (current.children.containsKey(c)) {
                current = current.children.get(c);
                results.addAll(current.values);
            } else {
                current = root;
            }
        }

        return results;
    }

    private static class Node<T> {
        Map<Character, Node<T>> children = new HashMap<>();
        Set<T> values = new HashSet<>();
        Node<T> failure;
    }
}
