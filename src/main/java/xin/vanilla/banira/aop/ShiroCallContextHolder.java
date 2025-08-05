package xin.vanilla.banira.aop;

public class ShiroCallContextHolder {
    private static final ThreadLocal<Boolean> blocked = ThreadLocal.withInitial(() -> false);

    public static boolean isBlocked() {
        return blocked.get();
    }

    public static void markBlocked() {
        blocked.set(true);
    }

    public static void clear() {
        blocked.remove();
    }
}
