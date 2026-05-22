package xin.vanilla.banira.plugin.chat.capability;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import xin.vanilla.banira.util.StringUtils;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;

/**
 * AI 能力参数字符串解析
 */
public final class AiCapabilityArgs {

    private AiCapabilityArgs() {
    }

    @Nonnull
    public static Map<String, String> parse(@Nullable String args) {
        Map<String, String> result = new LinkedHashMap<>();
        if (StringUtils.isNullOrEmptyEx(args)) {
            return result;
        }
        Arrays.stream(args.split(","))
                .map(String::trim)
                .filter(part -> part.contains("="))
                .forEach(part -> {
                    int idx = part.indexOf('=');
                    result.put(part.substring(0, idx).trim(), part.substring(idx + 1).trim());
                });
        return result;
    }

    @Nonnull
    public static String require(@Nonnull Map<String, String> args, @Nonnull String key) {
        String value = args.get(key);
        if (StringUtils.isNullOrEmptyEx(value)) {
            throw new IllegalArgumentException("缺少参数：" + key);
        }
        return value.trim();
    }

    public static int parseInt(@Nonnull Map<String, String> args, @Nonnull String key, int defaultValue) {
        String value = args.get(key);
        if (StringUtils.isNullOrEmptyEx(value)) {
            return defaultValue;
        }
        try {
            return Integer.parseInt(value.trim());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("参数 " + key + " 不是有效整数");
        }
    }

    public static long parseLong(@Nonnull Map<String, String> args, @Nonnull String key, long defaultValue) {
        String value = args.get(key);
        if (StringUtils.isNullOrEmptyEx(value)) {
            return defaultValue;
        }
        try {
            return Long.parseLong(value.trim());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("参数 " + key + " 不是有效数字");
        }
    }

    public static boolean parseBoolean(@Nonnull Map<String, String> args, @Nonnull String key, boolean defaultValue) {
        String value = args.get(key);
        if (StringUtils.isNullOrEmptyEx(value)) {
            return defaultValue;
        }
        return switch (value.trim().toLowerCase(Locale.ROOT)) {
            case "true", "1", "yes", "y", "是", "确认" -> true;
            case "false", "0", "no", "n", "否" -> false;
            default -> defaultValue;
        };
    }

}
