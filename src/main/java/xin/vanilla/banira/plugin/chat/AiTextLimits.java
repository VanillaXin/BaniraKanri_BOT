package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import xin.vanilla.banira.util.StringUtils;

/**
 * AI 工具/能力返回文本长度限制
 */
public final class AiTextLimits {

    public static final int MAX_CAPABILITY_RESULT = 3500;
    public static final int MAX_TOOL_RESULT = 3500;

    private AiTextLimits() {
    }

    @Nonnull
    public static String truncate(@Nullable String text, int maxChars) {
        if (StringUtils.isNullOrEmptyEx(text) || maxChars <= 0) {
            return "";
        }
        if (text.length() <= maxChars) {
            return text;
        }
        return text.substring(0, maxChars) + "\n...(已截断，原文共 " + text.length() + " 字)";
    }

}
