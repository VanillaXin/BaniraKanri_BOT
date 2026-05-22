package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.util.StringUtils;

import java.util.List;

final class ChatTurnCoalesceFormatter {

    private ChatTurnCoalesceFormatter() {
    }

    @Nonnull
    static String formatSupplementary(@Nonnull ChatTurnCoalesceState coalesceState) {
        if (!coalesceState.hasSupplementary()) {
            return "";
        }
        StringBuilder builder = new StringBuilder();
        if (coalesceState.retryAttempt() > 0) {
            builder.append("你上一轮分析尚未结束时，群里又陆续出现了新消息；请把它们当作同一次接话任务，不要分开各答一遍。");
        } else {
            builder.append("分析过程中群里又补充了新内容；请一并纳入本次判断。");
        }
        List<BaniraCodeContext> supplementary = coalesceState.supplementaryMessages();
        if (!supplementary.isEmpty()) {
            builder.append("\n期间补充消息：\n");
            for (BaniraCodeContext ctx : supplementary) {
                builder.append("- ").append(describe(ctx)).append('\n');
            }
        }
        return builder.toString().trim();
    }

    @Nonnull
    private static String describe(@Nonnull BaniraCodeContext ctx) {
        String msg = StringUtils.nullToEmpty(ctx.msg()).replaceAll("\\s+", " ").trim();
        if (msg.length() > 120) {
            msg = msg.substring(0, 120) + "...";
        }
        return "qq=" + ctx.sender() + (ctx.msgId() != null ? " msgId=" + ctx.msgId() : "") + "：" + msg;
    }
}
