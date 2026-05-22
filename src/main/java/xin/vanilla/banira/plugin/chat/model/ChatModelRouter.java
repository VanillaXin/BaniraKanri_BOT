package xin.vanilla.banira.plugin.chat.model;

import dev.langchain4j.data.message.ChatMessage;
import dev.langchain4j.model.chat.ChatModel;
import dev.langchain4j.model.chat.request.ChatRequest;
import dev.langchain4j.model.chat.response.ChatResponse;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.config.entity.extended.ChatModelEndpoint;
import xin.vanilla.banira.config.entity.extended.ChatModelSettings;
import xin.vanilla.banira.plugin.chat.ChatModelFactory;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

/**
 * 多 LLM 接口路由与故障转移
 */
@Slf4j
public class ChatModelRouter {

    @Getter
    private final ChatConfig config;
    private final List<ResolvedEndpoint> endpoints;
    private final AtomicInteger roundRobin = new AtomicInteger(0);

    public ChatModelRouter(@Nonnull ChatConfig config) {
        this.config = Objects.requireNonNull(config, "config");
        this.endpoints = resolveEndpoints(config.model());
        LOGGER.info("Resolved LLM endpoints: imageInputEnabled={}, {}", config.model().imageInputEnabled(), endpoints.isEmpty()
                ? "(none)"
                : endpoints.stream().map(endpoint -> describeEndpoint(endpoint.endpoint())).collect(Collectors.joining(" | ")));
    }

    public boolean isReady() {
        return !endpoints.isEmpty();
    }

    @Nonnull
    public List<ResolvedEndpoint> endpoints() {
        return endpoints;
    }

    @Nonnull
    public ChatModel primaryModel() {
        if (endpoints.isEmpty()) {
            throw new IllegalStateException("No LLM endpoint configured");
        }
        return endpoints.getFirst().model();
    }

    @Nonnull
    public ChatResponse chat(@Nonnull List<ChatMessage> messages) {
        if (endpoints.isEmpty()) {
            throw new IllegalStateException("No LLM endpoint configured");
        }
        List<ResolvedEndpoint> order = orderedEndpoints();
        Exception lastError = null;
        for (ResolvedEndpoint endpoint : order) {
            try {
                return endpoint.model().chat(messages);
            } catch (Exception e) {
                lastError = e;
                LOGGER.warn("LLM endpoint '{}' chat failed: {} ({})",
                        endpoint.endpoint().name(), e.getMessage(), describeEndpoint(endpoint.endpoint()));
            }
        }
        for (ResolvedEndpoint endpoint : order) {
            try {
                return endpoint.model().chat(ChatRequest.builder().messages(messages).build());
            } catch (Exception e) {
                lastError = e;
                LOGGER.warn("LLM endpoint '{}' chat(request) failed: {} ({})",
                        endpoint.endpoint().name(), e.getMessage(), describeEndpoint(endpoint.endpoint()));
            }
        }
        if (lastError instanceof RuntimeException runtime) {
            throw runtime;
        }
        throw new IllegalStateException("All LLM endpoints failed", lastError);
    }

    @Nonnull
    public ChatResponse chat(@Nonnull ChatRequest request) {
        if (endpoints.isEmpty()) {
            throw new IllegalStateException("No LLM endpoint configured");
        }
        Exception lastError = null;
        for (ResolvedEndpoint endpoint : orderedEndpoints()) {
            try {
                return endpoint.model().chat(request);
            } catch (Exception e) {
                lastError = e;
                LOGGER.warn("LLM endpoint '{}' agent chat failed: {} ({})",
                        endpoint.endpoint().name(), e.getMessage(), describeEndpoint(endpoint.endpoint()));
            }
        }
        if (lastError instanceof RuntimeException runtime) {
            throw runtime;
        }
        throw new IllegalStateException("All LLM endpoints failed", lastError);
    }

    @Nonnull
    public ChatResponse chatViaEndpoint(@Nonnull List<ChatMessage> messages
            , @Nullable String endpointName
            , double temperature
    ) {
        ResolvedEndpoint resolved = findEndpoint(endpointName);
        if (resolved == null) {
            return chat(messages);
        }
        try {
            return ChatModelFactory.create(resolved.endpoint(), config.model(), temperature).chat(messages);
        } catch (Exception e) {
            LOGGER.warn("Preflight endpoint '{}' failed, fallback to primary: {}", endpointName, e.getMessage());
            return chat(messages);
        }
    }

    @Nullable
    private ResolvedEndpoint findEndpoint(@Nullable String endpointName) {
        if (endpoints.isEmpty()) {
            return null;
        }
        if (StringUtils.isNotNullOrEmpty(endpointName)) {
            for (ResolvedEndpoint endpoint : endpoints) {
                if (endpointName.equalsIgnoreCase(endpoint.endpoint().name())) {
                    return endpoint;
                }
            }
            LOGGER.warn("LLM endpoint '{}' not found, fallback to primary", endpointName);
        }
        return endpoints.getFirst();
    }

    @Nonnull
    private List<ResolvedEndpoint> orderedEndpoints() {
        if (endpoints.size() <= 1 || !config.model().rotateOnFailure()) {
            return endpoints;
        }
        int start = Math.floorMod(roundRobin.getAndIncrement(), endpoints.size());
        List<ResolvedEndpoint> ordered = new ArrayList<>(endpoints.size());
        for (int i = 0; i < endpoints.size(); i++) {
            ordered.add(endpoints.get((start + i) % endpoints.size()));
        }
        return ordered;
    }

    @Nonnull
    private static List<ResolvedEndpoint> resolveEndpoints(@Nonnull ChatModelSettings settings) {
        List<ResolvedEndpoint> resolved = new ArrayList<>();
        if (settings.endpoints() == null || settings.endpoints().isEmpty()) {
            return resolved;
        }
        String defaultName = settings.defaultEndpoint();
        List<ChatModelEndpoint> enabled = settings.endpoints().stream()
                .filter(endpoint -> endpoint != null && endpoint.enabled())
                .filter(endpoint -> StringUtils.isNotNullOrEmpty(endpoint.apiKey()))
                .toList();
        if (StringUtils.isNotNullOrEmpty(defaultName)) {
            enabled.stream()
                    .filter(endpoint -> defaultName.equalsIgnoreCase(endpoint.name()))
                    .findFirst()
                    .ifPresent(endpoint -> resolved.add(toResolved(endpoint, settings)));
            enabled.stream()
                    .filter(endpoint -> !defaultName.equalsIgnoreCase(endpoint.name()))
                    .forEach(endpoint -> resolved.add(toResolved(endpoint, settings)));
        } else {
            enabled.forEach(endpoint -> resolved.add(toResolved(endpoint, settings)));
        }
        return resolved;
    }

    @Nonnull
    private static ResolvedEndpoint toResolved(@Nonnull ChatModelEndpoint endpoint, @Nonnull ChatModelSettings settings) {
        ChatModelEndpoint copy = new ChatModelEndpoint()
                .name(endpoint.name())
                .apiKey(endpoint.apiKey())
                .baseUrl(ChatModelFactory.normalizeOpenAiBaseUrl(StringUtils.isNotNullOrEmpty(endpoint.baseUrl()) ? endpoint.baseUrl() : settings.baseUrl()))
                .modelName(StringUtils.isNotNullOrEmpty(endpoint.modelName()) ? endpoint.modelName() : settings.modelName())
                .quotaCheckUrl(endpoint.quotaCheckUrl())
                .enabled(true)
                .remark(endpoint.remark());
        return new ResolvedEndpoint(copy, ChatModelFactory.create(copy, settings));
    }

    @Nonnull
    private static String describeEndpoint(@Nonnull ChatModelEndpoint endpoint) {
        return "name=" + endpoint.name()
                + ", baseUrl=" + endpoint.baseUrl()
                + ", model=" + endpoint.modelName()
                + ", key=" + maskApiKey(endpoint.apiKey());
    }

    @Nonnull
    private static String maskApiKey(String apiKey) {
        if (StringUtils.isNullOrEmptyEx(apiKey)) {
            return "(empty)";
        }
        String compact = apiKey.trim();
        if (compact.length() <= 8) {
            return "****";
        }
        return compact.substring(0, 4)
                + "..."
                + compact.substring(compact.length() - 4)
                + "#" + Integer.toHexString(compact.toLowerCase(Locale.ROOT).hashCode());
    }

    public record ResolvedEndpoint(@Nonnull ChatModelEndpoint endpoint, @Nonnull ChatModel model) {
    }

}
