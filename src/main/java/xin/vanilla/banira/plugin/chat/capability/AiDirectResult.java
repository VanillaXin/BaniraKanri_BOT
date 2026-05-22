package xin.vanilla.banira.plugin.chat.capability;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

public final class AiDirectResult {

    private static final String PREFIX = "[[BANIRA_DIRECT_HANDLED]]";

    private AiDirectResult() {
    }

    @Nonnull
    public static String sent(@Nonnull String message) {
        return PREFIX + message;
    }

    public static boolean isDirect(@Nullable String result) {
        return result != null && result.startsWith(PREFIX);
    }

    @Nonnull
    public static String message(@Nullable String result) {
        if (!isDirect(result)) {
            return result == null ? "" : result;
        }
        return result.substring(PREFIX.length()).trim();
    }
}
