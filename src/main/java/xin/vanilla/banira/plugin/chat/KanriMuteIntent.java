package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import xin.vanilla.banira.util.StringUtils;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parses mute durations only. Whether a message is asking for a mute must be decided by the Agent.
 */
public final class KanriMuteIntent {

    private static final Pattern DURATION_MINUTES = Pattern.compile("(\\d+)\\s*(分钟|分|min)");
    private static final Pattern DURATION_HOURS = Pattern.compile("(\\d+)\\s*(个)?\\s*(小时|h|钟头)");
    private static final Pattern HALF_HOUR = Pattern.compile("半\\s*(个)?\\s*(小时|h|钟头)");
    private static final Pattern DURATION_DAYS = Pattern.compile("(\\d+)\\s*(天|日|d|day)");
    private static final Pattern DURATION_MONTHS = Pattern.compile("(\\d+)\\s*(个)?\\s*(月|month)");

    private KanriMuteIntent() {
    }

    public static boolean hasMuteDurationOnlyReply(@Nullable String current) {
        if (StringUtils.isNullOrEmptyEx(current)) {
            return false;
        }
        String plain = stripCq(current).trim();
        if (plain.length() > 32) {
            return false;
        }
        if (containsAnyExact(plain, "一小时", "一个小时", "半小时", "半个小时", "俩小时", "两小时", "俩钟头", "两个钟头",
                "一月", "一个月", "半个月", "俩月", "两个月", "二个月")) {
            return true;
        }
        return DURATION_MINUTES.matcher(plain).matches()
                || DURATION_HOURS.matcher(plain).matches()
                || HALF_HOUR.matcher(plain).matches()
                || DURATION_DAYS.matcher(plain).matches()
                || DURATION_MONTHS.matcher(plain).matches();
    }

    public static int extractMinutes(@Nullable String text, int defaultMinutes) {
        if (StringUtils.isNullOrEmptyEx(text)) {
            return defaultMinutes;
        }
        String plain = stripCq(text);
        if (HALF_HOUR.matcher(plain).find()) {
            return 30;
        }
        Matcher hours = DURATION_HOURS.matcher(plain);
        if (hours.find()) {
            return Math.max(1, Integer.parseInt(hours.group(1))) * 60;
        }
        if (containsAny(plain, "一小时", "一个小时", "钟头")) {
            return 60;
        }
        if (containsAny(plain, "俩小时", "两小时", "俩钟头", "两个钟头")) {
            return 120;
        }
        Matcher minutes = DURATION_MINUTES.matcher(plain);
        if (minutes.find()) {
            return Math.max(1, Integer.parseInt(minutes.group(1)));
        }
        Matcher days = DURATION_DAYS.matcher(plain);
        if (days.find()) {
            return Math.max(1, Integer.parseInt(days.group(1))) * 24 * 60;
        }
        Matcher months = DURATION_MONTHS.matcher(plain);
        if (months.find()) {
            return Math.max(1, Integer.parseInt(months.group(1))) * 30 * 24 * 60;
        }
        if (containsAny(plain, "一月", "一个月")) {
            return 30 * 24 * 60;
        }
        if (containsAny(plain, "半个月")) {
            return 15 * 24 * 60;
        }
        if (containsAny(plain, "俩月", "两个月", "二个月")) {
            return 60 * 24 * 60;
        }
        return KanriProposalParser.extractMuteMinutes(text, defaultMinutes);
    }

    @Nonnull
    private static String stripCq(@Nullable String value) {
        if (value == null) {
            return "";
        }
        return value.replaceAll("\\[CQ:[^]]+]", " ");
    }

    private static boolean containsAny(@Nonnull String text, String... needles) {
        for (String needle : needles) {
            if (StringUtils.isNotNullOrEmpty(needle) && text.contains(needle)) {
                return true;
            }
        }
        return false;
    }

    private static boolean containsAnyExact(@Nonnull String text, String... values) {
        for (String value : values) {
            if (text.equals(value)) {
                return true;
            }
        }
        return false;
    }
}
