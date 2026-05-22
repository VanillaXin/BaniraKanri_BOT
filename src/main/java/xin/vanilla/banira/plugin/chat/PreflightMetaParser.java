package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;
import xin.vanilla.banira.util.StringUtils;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 解析 [PREFLIGHT invoke=yes|no interest=NN]。
 */
public final class PreflightMetaParser {

    private static final Pattern PREFLIGHT_TAG = Pattern.compile(
            "\\[PREFLIGHT\\s+invoke=(yes|no)\\s+interest=(\\d{1,3})\\s*]",
            Pattern.CASE_INSENSITIVE
    );

    private PreflightMetaParser() {
    }

    @Nonnull
    public static PreflightDecision parse(@Nonnull String raw) {
        Matcher matcher = PREFLIGHT_TAG.matcher(raw);
        PreflightDecision decision = null;
        while (matcher.find()) {
            boolean invoke = "yes".equalsIgnoreCase(matcher.group(1));
            int interest = EngagementMeta.clampInterest(Integer.parseInt(matcher.group(2)));
            decision = new PreflightDecision(invoke, interest);
        }
        if (decision != null) {
            return decision;
        }
        String trimmed = raw.trim();
        if (StringUtils.isNullOrEmptyEx(trimmed)) {
            return PreflightDecision.skip(0);
        }
        boolean invoke = trimmed.toLowerCase().contains("invoke=yes")
                || trimmed.contains("需要回复")
                || trimmed.contains("应该回复");
        return invoke ? PreflightDecision.invoke(0) : PreflightDecision.skip(0);
    }

    public record PreflightDecision(boolean invoke, int interest) {
        @Nonnull
        public static PreflightDecision invoke(int interest) {
            return new PreflightDecision(true, EngagementMeta.clampInterest(interest));
        }

        @Nonnull
        public static PreflightDecision skip(int interest) {
            return new PreflightDecision(false, EngagementMeta.clampInterest(interest));
        }
    }
}
