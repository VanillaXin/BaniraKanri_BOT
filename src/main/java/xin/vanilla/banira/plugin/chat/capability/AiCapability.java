package xin.vanilla.banira.plugin.chat.capability;

import jakarta.annotation.Nonnull;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import xin.vanilla.banira.plugin.chat.AiTextLimits;
import xin.vanilla.banira.plugin.chat.agent.AgentContext;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;

/**
 * AI 可调用的插件能力描述
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
public class AiCapability {

    private String name = "";
    private String description = "";
    private String parameterHint = "";
    private List<AiCapabilityParameter> parameters = new ArrayList<>();
    private AiCapabilityResultMode resultMode = AiCapabilityResultMode.TEXT;
    private AiMutationPolicy mutationPolicy = AiMutationPolicy.NONE;
    private AiConfirmationPolicy confirmationPolicy = AiConfirmationPolicy.NONE;
    private boolean allowQuotedContext;
    private String ownerClass = "";
    private AiCapabilityAccess access = AiCapabilityAccess.PUBLIC;
    private boolean mutating;
    private boolean sensitive;
    private boolean requireConfirmation;
    private BiFunction<AgentContext, Map<String, String>, String> executor;

    public boolean isAllowed(@Nonnull AgentContext ctx) {
        if (access == null) {
            access = AiCapabilityAccess.PUBLIC;
        }
        return switch (access) {
            case PUBLIC -> true;
            case ADMIN -> BaniraUtils.hasKanriOperatorAccess(ctx.bot(), ctx.scopeGroupId(), ctx.senderId());
            case OWNER -> BaniraUtils.isOwner(ctx.senderId());
        };
    }

    @Nonnull
    public String execute(@Nonnull AgentContext ctx, @Nonnull Map<String, String> args) {
        if (!isAllowed(ctx)) {
            return switch (access) {
                case OWNER -> "权限不足：这个能力仅主人可用。";
                case ADMIN -> "权限不足：这个能力需要群主/群管或主人/管家/女仆身份。";
                default -> "权限不足。";
            };
        }
        boolean confirmationRequired = requireConfirmation
                || confirmationPolicy == AiConfirmationPolicy.ALWAYS
                || confirmationPolicy == AiConfirmationPolicy.REQUIRED_FOR_SENSITIVE && sensitive;
        if (confirmationRequired && !"true".equalsIgnoreCase(args.getOrDefault("confirm", ""))) {
            return "这个能力会执行敏感操作。请先向用户简短确认，确认后再带 confirm=true 调用。";
        }
        if (executor == null) {
            return "能力未实现：" + name;
        }
        try {
            String result = executor.apply(ctx, args);
            if (result == null) {
                return "";
            }
            return AiTextLimits.truncate(result, AiTextLimits.MAX_CAPABILITY_RESULT);
        } catch (Exception e) {
            return "执行失败：" + e.getMessage();
        }
    }

    @Nonnull
    public String executeTrusted(@Nonnull AgentContext ctx, @Nonnull Map<String, String> args) {
        boolean confirmationRequired = requireConfirmation
                || confirmationPolicy == AiConfirmationPolicy.ALWAYS
                || confirmationPolicy == AiConfirmationPolicy.REQUIRED_FOR_SENSITIVE && sensitive;
        if (confirmationRequired && !"true".equalsIgnoreCase(args.getOrDefault("confirm", ""))) {
            return "这个能力会执行敏感操作。请先向用户简短确认，确认后再带 confirm=true 调用。";
        }
        if (executor == null) {
            return "能力未实现：" + name;
        }
        try {
            String result = executor.apply(ctx, args);
            if (result == null) {
                return "";
            }
            return AiTextLimits.truncate(result, AiTextLimits.MAX_CAPABILITY_RESULT);
        } catch (Exception e) {
            return "执行失败：" + e.getMessage();
        }
    }

}
