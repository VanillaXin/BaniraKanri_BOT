package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import xin.vanilla.banira.config.entity.extended.*;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * ChatConfig 默认值填充与边界校正
 */
public final class ChatConfigSupport {

    private ChatConfigSupport() {
    }

    @Nonnull
    public static ChatConfig createEnabledTemplate() {
        return new ChatConfig()
                .enabled(true)
                .model(new ChatModelSettings())
                .reply(new ChatReplySettings())
                .agent(new ChatAgentSettings())
                .memory(new ChatMemorySettings())
                .engagement(new ChatEngagementSettings())
                .useDefaultPersonaPrompt(true)
                .systemPrompt(new ArrayList<>());
    }

    @Nonnull
    public static ChatConfig normalize(@Nonnull ChatConfig cfg) {
        if (cfg.model() == null) {
            cfg.model(new ChatModelSettings());
        }
        if (cfg.reply() == null) {
            cfg.reply(new ChatReplySettings());
        }
        if (cfg.agent() == null) {
            cfg.agent(new ChatAgentSettings());
        }
        if (cfg.memory() == null) {
            cfg.memory(new ChatMemorySettings());
        }
        if (cfg.affinity() == null) {
            cfg.affinity(new ChatAffinitySettings());
        }
        if (cfg.engagement() == null) {
            cfg.engagement(new ChatEngagementSettings());
        }
        if (cfg.guard() == null) {
            cfg.guard(new ChatGuardSettings());
        }
        if (cfg.systemPrompt() == null) {
            cfg.systemPrompt(new ArrayList<>());
        }

        ChatModelSettings model = cfg.model();
        if (model.endpoints() == null) {
            model.endpoints(new ArrayList<>());
        }
        migrateLegacyEndpoint(model);
        if (StringUtils.isNullOrEmptyEx(model.modelName())) {
            model.modelName("gpt-4o-mini");
        }
        if (StringUtils.isNullOrEmptyEx(model.baseUrl())) {
            model.baseUrl("https://api.openai.com/v1/");
        }
        model.temperature(clamp(model.temperature(), 0.0, 2.0));
        model.timeout(Math.max(5, model.timeout()));
        model.maxRetries(Math.max(0, model.maxRetries()));
        for (ChatModelEndpoint endpoint : model.endpoints()) {
            if (endpoint == null) {
                continue;
            }
            if (StringUtils.isNullOrEmptyEx(endpoint.name())) {
                endpoint.name("default");
            }
            if (StringUtils.isNullOrEmptyEx(endpoint.baseUrl())) {
                endpoint.baseUrl(model.baseUrl());
            }
            if (StringUtils.isNullOrEmptyEx(endpoint.modelName())) {
                endpoint.modelName(model.modelName());
            }
        }

        ChatReplySettings reply = cfg.reply();
        reply.baseReplyProbability(clamp(reply.baseReplyProbability(), 0.0, 1.0));
        reply.mentionBoost(clamp(reply.mentionBoost(), 0.0, 1.0));
        reply.directMentionReplyProbability(clamp(reply.directMentionReplyProbability(), 0.0, 1.0));
        reply.nameMentionReplyProbability(clamp(reply.nameMentionReplyProbability(), 0.0, 1.0));
        if (reply.botNameAliases() == null) {
            reply.botNameAliases(new ArrayList<>());
        }
        reply.perTargetCooldownSeconds(Math.max(0, reply.perTargetCooldownSeconds()));
        reply.perTargetRateLimitPerMinute(Math.max(1, reply.perTargetRateLimitPerMinute()));
        reply.historyLimit(clamp(reply.historyLimit(), 1, 100));
        reply.maxSplitParts(clamp(reply.maxSplitParts(), 1, 10));
        reply.splitPartDelayMillis(clamp(reply.splitPartDelayMillis(), 0L, 5000L));
        reply.splitPartDelayJitterMillis(clamp(reply.splitPartDelayJitterMillis(), 0L, 5000L));
        reply.thinkingFeedbackDelayMillis(clamp(reply.thinkingFeedbackDelayMillis(), 300L, 15000L));
        reply.maxCharsPerPart(clamp(reply.maxCharsPerPart(), 20, 2000));
        reply.maxForwardLength(clamp(reply.maxForwardLength(), 100, 10000));
        reply.maxForwardChunkChars(clamp(reply.maxForwardChunkChars(), 300, 4000));
        reply.maxReplyChars(Math.max(0, reply.maxReplyChars()));
        reply.maxAtTargets(clamp(reply.maxAtTargets(), 0, 10));
        reply.maxEmojiPerReply(clamp(reply.maxEmojiPerReply(), 0, 10));
        reply.busyGroupWindowSeconds(clamp(reply.busyGroupWindowSeconds(), 5L, 300L));
        reply.busyGroupMessageThreshold(clamp(reply.busyGroupMessageThreshold(), 0, 50));
        reply.busyGroupDistinctSenderThreshold(clamp(reply.busyGroupDistinctSenderThreshold(), 0, 20));

        ChatAgentSettings agent = cfg.agent();
        agent.maxIterations(clamp(agent.maxIterations(), 1, 20));
        if (agent.allowedCapabilities() == null) {
            agent.allowedCapabilities(new ArrayList<>());
        }
        if (agent.blockedCapabilities() == null) {
            agent.blockedCapabilities(new ArrayList<>());
        }
        if (agent.ownerOnlyCapabilities() == null) {
            agent.ownerOnlyCapabilities(new ArrayList<>());
        }
        if (agent.adminOnlyCapabilities() == null) {
            agent.adminOnlyCapabilities(new ArrayList<>());
        }

        ChatMemorySettings memory = cfg.memory();
        memory.retrieveLimit(clamp(memory.retrieveLimit(), 0, 20));
        memory.maxExtractedPerTurn(clamp(memory.maxExtractedPerTurn(), 1, 10));
        memory.maxMemoryChars(clamp(memory.maxMemoryChars(), 20, 500));
        memory.minMemoryChars(clamp(memory.minMemoryChars(), 2, memory.maxMemoryChars()));
        memory.semanticTopK(clamp(memory.semanticTopK(), 1, 20));
        memory.semanticMinScore(clamp(memory.semanticMinScore(), 0.0, 1.0));
        memory.memoryExtractionQueueSize(clamp(memory.memoryExtractionQueueSize(), 1, 1000));

        ChatAffinitySettings affinity = cfg.affinity();
        affinity.minScore(clamp(affinity.minScore(), 0, 100));
        affinity.maxScore(clamp(affinity.maxScore(), affinity.minScore(), 100));
        affinity.initialScore(clamp(affinity.initialScore(), affinity.minScore(), affinity.maxScore()));
        affinity.lowThreshold(clamp(affinity.lowThreshold(), affinity.minScore(), affinity.maxScore()));
        affinity.veryLowThreshold(clamp(affinity.veryLowThreshold(), affinity.minScore(), affinity.lowThreshold()));
        affinity.lowReplyMultiplier(clamp(affinity.lowReplyMultiplier(), 0.0, 1.0));
        affinity.veryLowReplyMultiplier(clamp(affinity.veryLowReplyMultiplier(), 0.0, affinity.lowReplyMultiplier()));
        affinity.positiveDelta(Math.max(0, affinity.positiveDelta()));
        affinity.negativeDelta(Math.min(0, affinity.negativeDelta()));
        affinity.strongNegativeDelta(Math.min(affinity.negativeDelta(), affinity.strongNegativeDelta()));

        ChatEngagementSettings engagement = cfg.engagement();
        engagement.followInterestThreshold(clamp(engagement.followInterestThreshold(), 0, 100));
        engagement.followTtlSeconds(clamp(engagement.followTtlSeconds(), 15L, 3600L));
        engagement.passiveDecayDelta(clamp(engagement.passiveDecayDelta(), 1, 50));
        if (StringUtils.isNullOrEmptyEx(engagement.preflightScope())) {
            engagement.preflightScope("SEMIFULL");
        } else {
            engagement.preflightScope(engagement.preflightScope().trim().toUpperCase(java.util.Locale.ROOT));
        }
        engagement.preflightShortNoiseMaxChars(clamp(engagement.preflightShortNoiseMaxChars(), 1, 10));
        engagement.preflightHistoryLimit(clamp(engagement.preflightHistoryLimit(), 3, 30));
        engagement.preflightTemperature(clamp(engagement.preflightTemperature(), 0.0, 1.5));
        engagement.coalesceRetryDelayMillis(clamp(engagement.coalesceRetryDelayMillis(), 0L, 2000L));
        if (engagement.preflightEndpointName() == null) {
            engagement.preflightEndpointName("");
        }
        if (engagement.enabled() && !engagement.randomBubbleEnabled()) {
            reply.baseReplyProbability(0.0);
        }

        ChatGuardSettings guard = cfg.guard();
        if (guard.resourceRulePaths() == null) {
            guard.resourceRulePaths(new ArrayList<>());
        }
        if (guard.overrideRules() == null) {
            guard.overrideRules(new java.util.LinkedHashMap<>());
        }
        if (guard.appendRules() == null) {
            guard.appendRules(new java.util.LinkedHashMap<>());
        }
        if (guard.selfIntroTemplates() == null) {
            guard.selfIntroTemplates(new ArrayList<>());
        }
        if (guard.unnamedSelfIntroTemplates() == null) {
            guard.unnamedSelfIntroTemplates(new ArrayList<>());
        }
        if (guard.identityReplies() == null) {
            guard.identityReplies(new ArrayList<>());
        }
        if (guard.identityConfusedReplies() == null) {
            guard.identityConfusedReplies(new ArrayList<>());
        }
        if (guard.resourceRulePaths().isEmpty()) {
            guard.resourceRulePaths(new ChatGuardSettings().resourceRulePaths());
        }

        return cfg;
    }

    public static boolean isModelReady(@Nonnull ChatConfig cfg) {
        ChatModelSettings model = cfg.model();
        if (model.endpoints() != null) {
            for (ChatModelEndpoint endpoint : model.endpoints()) {
                if (endpoint != null && endpoint.enabled() && StringUtils.isNotNullOrEmpty(endpoint.apiKey())) {
                    return true;
                }
            }
        }
        return StringUtils.isNotNullOrEmpty(model.apiKey());
    }

    private static void migrateLegacyEndpoint(@Nonnull ChatModelSettings model) {
        if (model.endpoints() != null && !model.endpoints().isEmpty()) {
            return;
        }
        if (StringUtils.isNullOrEmptyEx(model.apiKey())) {
            return;
        }
        model.endpoints(new ArrayList<>(List.of(new ChatModelEndpoint()
                .name(StringUtils.isNotNullOrEmpty(model.defaultEndpoint()) ? model.defaultEndpoint() : "default")
                .apiKey(model.apiKey())
                .baseUrl(model.baseUrl())
                .modelName(model.modelName())
                .enabled(true)
        )));
    }

    @Nonnull
    private static List<ChatModelEndpoint> copyEndpoints(@Nonnull List<ChatModelEndpoint> source) {
        List<ChatModelEndpoint> copied = new ArrayList<>();
        for (ChatModelEndpoint endpoint : source) {
            if (endpoint == null) {
                continue;
            }
            copied.add(new ChatModelEndpoint()
                    .name(endpoint.name())
                    .apiKey(endpoint.apiKey())
                    .baseUrl(endpoint.baseUrl())
                    .modelName(endpoint.modelName())
                    .quotaCheckUrl(endpoint.quotaCheckUrl())
                    .enabled(endpoint.enabled())
                    .remark(endpoint.remark())
            );
        }
        return copied;
    }

    /**
     * 启用聊天：保留已有配置，必要时从全局（群 0）合并 model 参数
     */
    @Nonnull
    public static ChatConfig prepareEnable(@Nullable ChatConfig existing, @Nullable ChatConfig global) {
        ChatConfig cfg = existing;
        if (cfg == null) {
            cfg = createEnabledTemplate();
        } else {
            normalize(cfg);
        }
        cfg.enabled(true);
        mergeModelFromGlobalIfNeeded(cfg, global);
        migrateLegacyEndpoint(cfg.model());
        return normalize(cfg);
    }

    public static void mergeModelFromGlobalIfNeeded(@Nonnull ChatConfig target, @Nullable ChatConfig global) {
        if (global == null || isModelReady(target)) {
            return;
        }
        normalize(global);
        if (!isModelReady(global)) {
            return;
        }
        ChatModelSettings targetModel = target.model();
        ChatModelSettings globalModel = global.model();
        migrateLegacyEndpoint(targetModel);
        migrateLegacyEndpoint(globalModel);
        if (targetModel.endpoints() == null || targetModel.endpoints().isEmpty()) {
            if (globalModel.endpoints() != null && !globalModel.endpoints().isEmpty()) {
                targetModel.endpoints(copyEndpoints(globalModel.endpoints()));
                if (StringUtils.isNullOrEmptyEx(targetModel.defaultEndpoint())) {
                    targetModel.defaultEndpoint(globalModel.defaultEndpoint());
                }
            }
        }
        if (StringUtils.isNullOrEmptyEx(targetModel.apiKey())) {
            targetModel.apiKey(globalModel.apiKey());
        }
        if (StringUtils.isNullOrEmptyEx(targetModel.modelName())) {
            targetModel.modelName(globalModel.modelName());
        }
        if (StringUtils.isNullOrEmptyEx(targetModel.baseUrl())) {
            targetModel.baseUrl(globalModel.baseUrl());
        }
        if (targetModel.timeout() <= 0) {
            targetModel.timeout(globalModel.timeout());
        }
        if (targetModel.maxRetries() <= 0) {
            targetModel.maxRetries(globalModel.maxRetries());
        }
        if (targetModel.temperature() <= 0) {
            targetModel.temperature(globalModel.temperature());
        }
    }

    private static int clamp(int value, int min, int max) {
        return Math.max(min, Math.min(max, value));
    }

    private static long clamp(long value, long min, long max) {
        return Math.max(min, Math.min(max, value));
    }

    private static double clamp(double value, double min, double max) {
        return Math.max(min, Math.min(max, value));
    }
}
