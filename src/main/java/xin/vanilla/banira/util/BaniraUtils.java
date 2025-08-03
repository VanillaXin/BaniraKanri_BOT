package xin.vanilla.banira.util;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

public class BaniraUtils {

    // region mutableSetOf

    public static <E> Set<E> mutableSetOf() {
        return new HashSet<>();
    }

    public static <E> Set<E> mutableSetOf(E e1) {
        return new HashSet<>(Collections.singletonList(e1));
    }

    public static <E> Set<E> mutableSetOf(E e1, E e2) {
        return new HashSet<>(Arrays.asList(e1, e2));
    }

    public static <E> Set<E> mutableSetOf(E e1, E e2, E e3) {
        return new HashSet<>(Arrays.asList(e1, e2, e3));
    }

    public static <E> Set<E> mutableSetOf(E e1, E e2, E e3, E e4) {
        return new HashSet<>(Arrays.asList(e1, e2, e3, e4));
    }

    public static <E> Set<E> mutableSetOf(E e1, E e2, E e3, E e4, E e5) {
        return new HashSet<>(Arrays.asList(e1, e2, e3, e4, e5));
    }

    public static <E> Set<E> mutableSetOf(E e1, E e2, E e3, E e4, E e5, E e6) {
        return new HashSet<>(Arrays.asList(e1, e2, e3, e4, e5, e6));
    }

    public static <E> Set<E> mutableSetOf(E e1, E e2, E e3, E e4, E e5, E e6, E e7) {
        return new HashSet<>(Arrays.asList(e1, e2, e3, e4, e5, e6, e7));
    }

    public static <E> Set<E> mutableSetOf(E e1, E e2, E e3, E e4, E e5, E e6, E e7, E e8) {
        return new HashSet<>(Arrays.asList(e1, e2, e3, e4, e5, e6, e7, e8));
    }

    public static <E> Set<E> mutableSetOf(E e1, E e2, E e3, E e4, E e5, E e6, E e7, E e8, E e9) {
        return new HashSet<>(Arrays.asList(e1, e2, e3, e4, e5, e6, e7, e8, e9));
    }

    public static <E> Set<E> mutableSetOf(E e1, E e2, E e3, E e4, E e5, E e6, E e7, E e8, E e9, E e10) {
        return new HashSet<>(Arrays.asList(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10));
    }

    @SafeVarargs
    public static <E> Set<E> mutableSetOf(E... elements) {
        return new HashSet<>(Arrays.asList(elements));
    }

    // endregion mutableSetOf

}
