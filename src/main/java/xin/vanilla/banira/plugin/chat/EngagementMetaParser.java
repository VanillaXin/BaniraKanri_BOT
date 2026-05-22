package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;
import xin.vanilla.banira.util.StringUtils;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 解析并剥离 [ENGAGE reply=yes|no interest=NN] 元数据行。
 */
public final class EngagementMetaParser {

    private static final Pattern ENGAGE_TAG = Pattern.compile(
            "\\[ENGAGE\\s+reply=(yes|no)(?:\\s+|\\s*[|,;，；]\\s*)interest=(\\d{1,3})\\s*]",
            Pattern.CASE_INSENSITIVE
    );

    private EngagementMetaParser() {
    }

    @Nonnull
    public static ParsedEngagement parse(@Nonnull String raw) {
        if (StringUtils.isNullOrEmptyEx(raw)) {
            return new ParsedEngagement("", EngagementMeta.inferFromSpeech(""));
        }
        Matcher matcher = ENGAGE_TAG.matcher(raw);
        EngagementMeta meta = null;
        while (matcher.find()) {
            boolean shouldReply = "yes".equalsIgnoreCase(matcher.group(1));
            int interest = EngagementMeta.clampInterest(Integer.parseInt(matcher.group(2)));
            meta = shouldReply ? EngagementMeta.replyWithInterest(interest) : EngagementMeta.silentWithInterest(interest);
        }
        String speech = ENGAGE_TAG.matcher(raw).replaceAll("").trim();
        if (meta == null) {
            meta = EngagementMeta.inferFromSpeech(speech);
        }
        return new ParsedEngagement(speech, meta);
    }

    public record ParsedEngagement(@Nonnull String speech, @Nonnull EngagementMeta meta) {
    }
}
