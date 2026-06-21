package xin.vanilla.banira.plugin.chat.capability;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.aop.support.AopUtils;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.basic.PluginConfig;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.plugin.RecorderPlugin;
import xin.vanilla.banira.plugin.chat.agent.AgentContext;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.plugin.kanri.KanriSelfOperationPolicy;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Supplier;
import java.util.stream.Collectors;

/**
 * AI 能力注册中心，聚合各插件贡献的能力并按 capability 过滤
 */
@Slf4j
@Component
public class AiCapabilityRegistry {

    @Resource
    private ApplicationContext applicationContext;
    @Resource
    private Supplier<PluginConfig> pluginConfig;

    private final ConcurrentHashMap<Long, List<AiCapability>> cache = new ConcurrentHashMap<>();

    @Nonnull
    public List<AiCapability> list(@Nullable Long groupId) {
        long cacheKey = groupId == null ? 0L : groupId;
        return cache.computeIfAbsent(cacheKey, this::buildList);
    }

    public void invalidate() {
        cache.clear();
    }

    @Nullable
    public AiCapability resolve(@Nullable Long groupId, @Nullable String name) {
        if (StringUtils.isNullOrEmptyEx(name)) {
            return null;
        }
        String normalized = normalize(name);
        return list(groupId).stream()
                .filter(cap -> normalize(cap.name()).equals(normalized))
                .findFirst()
                .orElse(null);
    }

    @Nonnull
    public String execute(@Nonnull AgentContext ctx, @Nonnull ChatConfig chatConfig, @Nonnull String name, @Nonnull Map<String, String> args) {
        AiCapability capability = resolve(ctx.scopeGroupId(), name);
        if (capability == null) {
            return "未找到能力：" + name;
        }
        String policyReason = validate(ctx, chatConfig, capability, args);
        if (policyReason != null) {
            return policyReason;
        }
        LOGGER.debug("AI capability execute name={} group={} sender={} argKeys={}",
                capability.name(), ctx.scopeGroupId(), ctx.senderId(), args.keySet());
        if (!capability.isAllowed(ctx) && isAllowedSelfKanri(ctx, capability, args)) {
            return capability.executeTrusted(ctx, args);
        }
        return capability.execute(ctx, args);
    }

    @Nullable
    public String validate(@Nonnull AgentContext ctx
            , @Nonnull ChatConfig chatConfig
            , @Nonnull AiCapability capability
            , @Nonnull Map<String, String> args
    ) {
        return validatePolicy(ctx, chatConfig, capability, args);
    }

    @Nonnull
    public String describeAvailable(@Nonnull AgentContext ctx, @Nonnull ChatConfig chatConfig) {
        List<AiCapability> capabilities = available(ctx, chatConfig);
        if (capabilities.isEmpty()) {
            return "当前没有可用能力。";
        }
        return capabilities.stream()
                .map(cap -> "- " + cap.name() + ": " + cap.description()
                        + formatFlags(cap)
                        + parameterSummary(cap))
                .collect(Collectors.joining("\n"));
    }

    @Nonnull
    public List<AiCapability> available(@Nonnull AgentContext ctx, @Nonnull ChatConfig chatConfig) {
        return list(ctx.scopeGroupId()).stream()
                .filter(cap -> validatePolicy(ctx, chatConfig, cap, Map.of()) == null)
                .toList();
    }

    @Nonnull
    public String describeAll(@Nullable Long groupId) {
        List<AiCapability> capabilities = list(groupId);
        if (capabilities.isEmpty()) {
            return "当前没有可用能力。";
        }
        return capabilities.stream()
                .map(cap -> "- " + cap.name() + ": " + cap.description()
                        + formatFlags(cap)
                        + parameterSummary(cap))
                .collect(Collectors.joining("\n"));
    }

    @Nonnull
    private List<AiCapability> buildList(long groupId) {
        List<AiCapability> capabilities = new ArrayList<>();
        Map<String, BasePlugin> pluginBeans = applicationContext.getBeansOfType(BasePlugin.class);
        for (BasePlugin plugin : pluginBeans.values()) {
            if (!isEnabled(plugin)) {
                continue;
            }
            if (!(plugin instanceof AiCapabilityProvider provider)) {
                continue;
            }
            List<AiCapability> contributed = new ArrayList<>();
            provider.registerAiCapabilities(contributed, groupId == 0L ? null : groupId);
            String ownerClass = resolvePluginClassName(plugin);
            contributed.forEach(capability -> {
                if (StringUtils.isNullOrEmptyEx(capability.ownerClass())) {
                    capability.ownerClass(ownerClass);
                }
                capabilities.add(capability);
            });
        }
        return capabilities.stream()
                .filter(cap -> StringUtils.isNotNullOrEmpty(cap.name()))
                .sorted(Comparator.comparing(AiCapability::name))
                .toList();
    }

    private boolean isEnabled(@Nonnull BasePlugin plugin) {
        if (plugin instanceof RecorderPlugin) {
            return false;
        }
        PluginConfig config = pluginConfig.get();
        if (config == null || config.capability() == null) {
            return false;
        }
        Integer capability = config.capability().get(resolvePluginClassName(plugin));
        return capability != null && capability > 0;
    }

    @Nullable
    private static String validatePolicy(@Nonnull AgentContext ctx
            , @Nonnull ChatConfig chatConfig
            , @Nonnull AiCapability capability
            , @Nonnull Map<String, String> args
    ) {
        if (!capability.isAllowed(ctx)) {
            if (isAllowedSelfKanri(ctx, capability, args) || isVisibleSelfKanri(ctx, capability, args)) {
                return null;
            }
            if (capability.access() == AiCapabilityAccess.ADMIN) {
                return "权限不足：当前发起者不是群管/主人/管家/女仆，不能调用 " + capability.name() + "。";
            }
            return "权限不足：当前用户不能调用 " + capability.name() + "。";
        }
        if (chatConfig.agent() == null) {
            return null;
        }
        String name = normalize(capability.name());
        List<String> blocked = chatConfig.agent().blockedCapabilities();
        if (blocked != null && blocked.stream().map(AiCapabilityRegistry::normalize).anyMatch(name::equals)) {
            return "能力已被配置禁用：" + capability.name();
        }
        List<String> allowed = chatConfig.agent().allowedCapabilities();
        if (allowed != null && !allowed.isEmpty()
                && allowed.stream().map(AiCapabilityRegistry::normalize).noneMatch(name::equals)) {
            return "能力不在 AI 允许列表中：" + capability.name();
        }
        List<String> ownerOnly = chatConfig.agent().ownerOnlyCapabilities();
        if (ownerOnly != null && ownerOnly.stream().map(AiCapabilityRegistry::normalize).anyMatch(name::equals)
                && !BaniraUtils.isOwner(ctx.senderId())) {
            return "权限不足：这个能力已配置为仅主人可用。";
        }
        List<String> adminOnly = chatConfig.agent().adminOnlyCapabilities();
        if (adminOnly != null && adminOnly.stream().map(AiCapabilityRegistry::normalize).anyMatch(name::equals)
                && !BaniraUtils.hasKanriOperatorAccess(ctx.bot(), ctx.scopeGroupId(), ctx.senderId())) {
            return "权限不足：这个能力已配置为仅群管/主人/管家/女仆可用。";
        }
        return null;
    }

    private static boolean isAllowedSelfKanri(@Nonnull AgentContext ctx
            , @Nonnull AiCapability capability
            , @Nonnull Map<String, String> args
    ) {
        if (!"execute_kanri".equals(normalize(capability.name()))) {
            return false;
        }
        String action = StringUtils.nullToEmpty(args.get("action")).trim().toLowerCase(Locale.ROOT);
        if (!"mute".equals(action) && !"loud".equals(action) && !"card".equals(action)) {
            return false;
        }
        if (ctx.bot() == null || ctx.senderId() == null || ctx.senderId() <= 0 || ctx.scopeGroupId() <= 0) {
            return false;
        }
        Set<Long> targets = BaniraUtils.getUserIds(StringUtils.nullToEmpty(args.get("args")).split("\\s+"));
        if (targets.size() != 1 || !targets.contains(ctx.senderId())) {
            return false;
        }
        if ("card".equals(action)) {
            return KanriSelfOperationPolicy.canAllowSelfCard(ctx.bot(), ctx.scopeGroupId(), ctx.senderId());
        }
        return KanriSelfOperationPolicy.canAllowSelfMuteOrLoud(ctx.bot(), ctx.scopeGroupId(), ctx.senderId());
    }

    private static boolean isVisibleSelfKanri(@Nonnull AgentContext ctx
            , @Nonnull AiCapability capability
            , @Nonnull Map<String, String> args
    ) {
        if (!args.isEmpty() || !"execute_kanri".equals(normalize(capability.name()))) {
            return false;
        }
        return ctx.bot() != null
                && ctx.senderId() != null
                && ctx.senderId() > 0
                && ctx.scopeGroupId() > 0
                && (KanriSelfOperationPolicy.canAllowSelfMuteOrLoud(ctx.bot(), ctx.scopeGroupId(), ctx.senderId())
                || KanriSelfOperationPolicy.canAllowSelfCard(ctx.bot(), ctx.scopeGroupId(), ctx.senderId()));
    }

    @Nonnull
    private static String formatFlags(@Nonnull AiCapability capability) {
        List<String> flags = new ArrayList<>();
        if (capability.access() != null && capability.access() != AiCapabilityAccess.PUBLIC) {
            flags.add(capability.access() == AiCapabilityAccess.OWNER ? "主人" : "管理");
        }
        if (capability.mutating()) {
            flags.add("会修改");
        }
        if (capability.sensitive()) {
            flags.add("敏感");
        }
        if (capability.requireConfirmation()) {
            flags.add("需确认");
        }
        return flags.isEmpty() ? "" : "（" + String.join("、", flags) + "）";
    }

    @Nonnull
    private static String parameterSummary(@Nonnull AiCapability capability) {
        if (capability.parameters() != null && !capability.parameters().isEmpty()) {
            return " 参数：" + capability.parameters().stream()
                    .filter(parameter -> parameter != null && StringUtils.isNotNullOrEmpty(parameter.name()))
                    .map(parameter -> parameter.name()
                            + (parameter.required() ? "*" : "")
                            + (StringUtils.isNotNullOrEmpty(parameter.description()) ? "=" + parameter.description() : ""))
                    .collect(Collectors.joining(", "));
        }
        return StringUtils.isNotNullOrEmpty(capability.parameterHint()) ? " 参数：" + capability.parameterHint() : "";
    }

    @Nonnull
    private static String resolvePluginClassName(@Nonnull BasePlugin plugin) {
        return AopUtils.getTargetClass(plugin).getName();
    }

    @Nonnull
    private static String normalize(@Nullable String value) {
        if (value == null) {
            return "";
        }
        return value.trim().toLowerCase(Locale.ROOT);
    }

}
