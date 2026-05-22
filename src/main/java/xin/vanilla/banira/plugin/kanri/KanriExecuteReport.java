package xin.vanilla.banira.plugin.kanri;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.util.List;

/**
 * 智能体调用群管后的结构化结果，供 LLM 或 pending 确认链路生成口语回复。
 */
public record KanriExecuteReport(
        int resultCode,
        @Nonnull String actionName,
        @Nonnull List<String> succeededNames,
        @Nonnull List<String> failedNames,
        int durationMinutes,
        @Nullable String agentMessageOverride
) {

    public KanriExecuteReport(int resultCode
            , @Nonnull String actionName
            , @Nonnull List<String> succeededNames
            , @Nonnull List<String> failedNames
            , int durationMinutes
    ) {
        this(resultCode, actionName, succeededNames, failedNames, durationMinutes, null);
    }

    @Nonnull
    public static KanriExecuteReport withMessage(int resultCode
            , @Nonnull String actionName
            , @Nonnull String message
    ) {
        return new KanriExecuteReport(resultCode, actionName, List.of(), List.of(), 0, message);
    }

    public boolean hasSucceeded() {
        return resultCode == KanriHandler.SUCCESS && (!succeededNames.isEmpty()
                || agentMessageOverride != null && !agentMessageOverride.isBlank());
    }

    public boolean isPartialSuccess() {
        return hasSucceeded() && !failedNames.isEmpty();
    }

    @Nonnull
    public String toAgentMessage() {
        if (agentMessageOverride != null && !agentMessageOverride.isBlank()) {
            return agentMessageOverride;
        }
        if (resultCode == KanriHandler.SUCCESS) {
            if (failedNames.isEmpty()) {
                if (durationMinutes > 0 && actionName.contains("禁言")) {
                    return actionName + "已执行：" + String.join("、", succeededNames)
                            + "（" + durationMinutes + "分钟）。";
                }
                if (!succeededNames.isEmpty()) {
                    return actionName + "已执行：" + String.join("、", succeededNames) + "。";
                }
                return actionName + " 已执行。";
            }
            StringBuilder message = new StringBuilder();
            message.append(actionName).append("部分完成。");
            if (!succeededNames.isEmpty()) {
                message.append("已成功：").append(String.join("、", succeededNames));
                if (durationMinutes > 0) {
                    message.append("（").append(durationMinutes).append("分钟）");
                }
                message.append("。");
            }
            message.append("未能操作：").append(String.join("、", failedNames))
                    .append("（权限不足，对方可能为群管或等级更高）。");
            message.append("请向用户口语化说明结果，不要列出 QQ 号。");
            return message.toString();
        }
        return switch (resultCode) {
            case KanriHandler.NO_OP -> failedNames.isEmpty()
                    ? "权限不足，无法执行 " + actionName + "。"
                    : actionName + "未能执行。未能操作："
                    + String.join("、", failedNames)
                    + "（权限不足，对方可能为群管或等级更高）。请向用户口语化说明，不要列出 QQ 号。";
            case KanriHandler.BOT_NO_OP -> "机器人权限不足，无法执行 " + actionName + "。";
            case KanriHandler.FAIL -> actionName + " 执行失败，请检查参数。";
            default -> actionName + " 未产生操作。";
        };
    }
}
