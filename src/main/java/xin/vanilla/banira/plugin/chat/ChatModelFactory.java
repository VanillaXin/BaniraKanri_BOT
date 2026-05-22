package xin.vanilla.banira.plugin.chat;

import dev.langchain4j.model.chat.ChatModel;
import dev.langchain4j.model.openai.OpenAiChatModel;
import jakarta.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.config.entity.extended.ChatModelEndpoint;
import xin.vanilla.banira.config.entity.extended.ChatModelSettings;
import xin.vanilla.banira.util.StringUtils;

import java.time.Duration;
import java.util.Locale;

@Slf4j
public final class ChatModelFactory {

    private ChatModelFactory() {
    }

    @Nonnull
    public static ChatModel create(@Nonnull ChatConfig cfg) {
        ChatModelSettings settings = cfg.model();
        if (settings.endpoints() != null && !settings.endpoints().isEmpty()) {
            ChatModelEndpoint first = settings.endpoints().stream()
                    .filter(endpoint -> endpoint != null && endpoint.enabled())
                    .filter(endpoint -> StringUtils.isNotNullOrEmpty(endpoint.apiKey()))
                    .findFirst()
                    .orElseThrow(() -> new IllegalStateException("No enabled LLM endpoint"));
            return create(first, settings);
        }
        return createLegacy(settings);
    }

    @Nonnull
    public static ChatModel create(@Nonnull ChatModelEndpoint endpoint, @Nonnull ChatModelSettings shared) {
        return create(endpoint, shared, shared.temperature());
    }

    @Nonnull
    public static ChatModel create(@Nonnull ChatModelEndpoint endpoint
            , @Nonnull ChatModelSettings shared
            , double temperature
    ) {
        String modelName = StringUtils.isNotNullOrEmpty(endpoint.modelName())
                ? endpoint.modelName()
                : shared.modelName();
        String baseUrl = normalizeOpenAiBaseUrl(StringUtils.isNotNullOrEmpty(endpoint.baseUrl())
                ? endpoint.baseUrl()
                : shared.baseUrl());
        return OpenAiChatModel.builder()
                .apiKey(endpoint.apiKey())
                .modelName(StringUtils.isNullOrEmptyEx(modelName) ? "gpt-4o-mini" : modelName)
                .temperature(temperature)
                .timeout(Duration.ofSeconds(Math.max(5, shared.timeout())))
                .maxRetries(Math.max(0, shared.maxRetries()))
                .baseUrl(StringUtils.isNullOrEmptyEx(baseUrl) ? "https://api.openai.com/v1/" : baseUrl)
                .build();
    }

    @Nonnull
    private static ChatModel createLegacy(@Nonnull ChatModelSettings model) {
        return OpenAiChatModel.builder()
                .apiKey(model.apiKey())
                .modelName(StringUtils.isNullOrEmptyEx(model.modelName()) ? "gpt-4o-mini" : model.modelName())
                .temperature(model.temperature())
                .timeout(Duration.ofSeconds(model.timeout()))
                .maxRetries(model.maxRetries())
                .baseUrl(normalizeOpenAiBaseUrl(model.baseUrl()))
                .build();
    }

    @Nonnull
    public static String normalizeOpenAiBaseUrl(String baseUrl) {
        if (StringUtils.isNullOrEmptyEx(baseUrl)) {
            return "https://api.openai.com/v1/";
        }
        String normalized = baseUrl.trim();
        String lower = normalized.toLowerCase(Locale.ROOT);
        for (String suffix : new String[]{"/chat/completions", "/responses", "/completions"}) {
            if (lower.endsWith(suffix)) {
                normalized = normalized.substring(0, normalized.length() - suffix.length());
                LOGGER.warn("LLM baseUrl should be API root, not a concrete endpoint. Auto-normalized '{}' to '{}'.", baseUrl, normalized);
                break;
            }
        }
        if (!normalized.endsWith("/")) {
            normalized += "/";
        }
        return normalized;
    }

}
