package xin.vanilla.banira.plugin.chat.model;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import org.springframework.stereotype.Service;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.config.entity.extended.ChatModelEndpoint;
import xin.vanilla.banira.domain.KeyValue;
import xin.vanilla.banira.plugin.chat.ChatConfigSupport;
import xin.vanilla.banira.util.HttpUtils;
import xin.vanilla.banira.util.JsonUtils;
import xin.vanilla.banira.util.StringUtils;
import xin.vanilla.banira.util.http.HttpResponse;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;

/**
 * LLM 接口配额/余额查询
 */
@Service
public class ChatQuotaService {

    @Nonnull
    public String describeEndpoints(@Nonnull ChatConfig cfg) {
        ChatModelRouter router = new ChatModelRouter(ChatConfigSupport.normalize(cfg));
        if (router.endpoints().isEmpty()) {
            return "当前未配置可用的 LLM 接口。";
        }
        return router.endpoints().stream()
                .map(slot -> formatEndpointSummary(slot.endpoint()))
                .collect(Collectors.joining("\n"));
    }

    @Nonnull
    public String checkQuota(@Nonnull ChatConfig cfg, @Nullable String endpointName) {
        ChatModelRouter router = new ChatModelRouter(ChatConfigSupport.normalize(cfg));
        if (router.endpoints().isEmpty()) {
            return "当前未配置可用的 LLM 接口。";
        }
        if (StringUtils.isNullOrEmptyEx(endpointName)) {
            return router.endpoints().stream()
                    .map(slot -> checkSingle(slot.endpoint()))
                    .collect(Collectors.joining("\n\n"));
        }
        String normalized = endpointName.trim().toLowerCase(Locale.ROOT);
        return router.endpoints().stream()
                .filter(slot -> normalized.equals(slot.endpoint().name().trim().toLowerCase(Locale.ROOT)))
                .findFirst()
                .map(slot -> checkSingle(slot.endpoint()))
                .orElse("未找到接口：" + endpointName);
    }

    @Nonnull
    private String checkSingle(@Nonnull ChatModelEndpoint endpoint) {
        List<String> urls = buildQuotaUrls(endpoint);
        if (urls.isEmpty()) {
            return formatEndpointSummary(endpoint) + "\n状态：未配置 quotaCheckUrl，无法自动查询配额。";
        }
        Exception lastError = null;
        for (String url : urls) {
            try {
                HttpResponse response = HttpUtils.get(url,
                        new KeyValue<>("Authorization", "Bearer " + endpoint.apiKey()),
                        new KeyValue<>("Accept", "application/json")
                );
                if (response == null) {
                    continue;
                }
                if (response.statusCode() < 200 || response.statusCode() >= 300) {
                    lastError = new IllegalStateException("HTTP " + response.statusCode());
                    continue;
                }
                String body = response.getBodyAsString();
                if (StringUtils.isNullOrEmptyEx(body)) {
                    continue;
                }
                return formatEndpointSummary(endpoint) + "\n" + summarizeQuotaBody(body);
            } catch (Exception e) {
                lastError = e;
            }
        }
        String error = lastError != null ? lastError.getMessage() : "无响应";
        return formatEndpointSummary(endpoint) + "\n状态：配额查询失败（" + error + "）。";
    }

    @Nonnull
    private static List<String> buildQuotaUrls(@Nonnull ChatModelEndpoint endpoint) {
        List<String> urls = new ArrayList<>();
        if (StringUtils.isNotNullOrEmpty(endpoint.quotaCheckUrl())) {
            urls.add(endpoint.quotaCheckUrl().trim());
            return urls;
        }
        String baseUrl = endpoint.baseUrl();
        if (StringUtils.isNullOrEmptyEx(baseUrl)) {
            return urls;
        }
        String normalized = baseUrl.endsWith("/") ? baseUrl : baseUrl + "/";
        String origin = extractOrigin(normalized);
        if (origin != null) {
            urls.add(origin + "/api/user/self");
            urls.add(origin + "/api/user/profile");
        }
        urls.add(normalized + "dashboard/billing/subscription");
        urls.add(normalized + "user/balance");
        return urls.stream().distinct().toList();
    }

    @Nullable
    private static String extractOrigin(@Nonnull String baseUrl) {
        try {
            URI uri = URI.create(baseUrl);
            if (uri.getScheme() == null || uri.getHost() == null) {
                return null;
            }
            int port = uri.getPort();
            return port > 0
                    ? uri.getScheme() + "://" + uri.getHost() + ":" + port
                    : uri.getScheme() + "://" + uri.getHost();
        } catch (Exception ignored) {
            return null;
        }
    }

    @Nonnull
    private static String formatEndpointSummary(@Nonnull ChatModelEndpoint endpoint) {
        String keyHint = maskApiKey(endpoint.apiKey());
        StringBuilder builder = new StringBuilder();
        builder.append("接口：").append(endpoint.name());
        if (StringUtils.isNotNullOrEmpty(endpoint.remark())) {
            builder.append("（").append(endpoint.remark()).append('）');
        }
        builder.append('\n')
                .append("地址：").append(endpoint.baseUrl()).append('\n')
                .append("模型：").append(endpoint.modelName()).append('\n')
                .append("密钥：").append(keyHint);
        return builder.toString();
    }

    @Nonnull
    private static String maskApiKey(@Nullable String apiKey) {
        if (StringUtils.isNullOrEmptyEx(apiKey)) {
            return "(未设置)";
        }
        if (apiKey.length() <= 8) {
            return "****";
        }
        return apiKey.substring(0, 4) + "..." + apiKey.substring(apiKey.length() - 4);
    }

    @Nonnull
    private static String summarizeQuotaBody(@Nonnull String body) {
        JsonElement root = JsonUtils.parseJson(body);
        if (root == null || !root.isJsonObject()) {
            return "原始响应：\n" + truncatePreview(body);
        }
        JsonObject json = root.getAsJsonObject();
        JsonObject data = json.has("data") && json.get("data").isJsonObject()
                ? json.getAsJsonObject("data")
                : json;

        List<String> lines = new ArrayList<>();
        appendIfPresent(lines, "余额", data, "balance", "remain_quota", "remaining", "quota");
        appendIfPresent(lines, "已用", data, "used_quota", "total_usage", "usage");
        appendIfPresent(lines, "总额度", data, "hard_limit_usd", "total_quota", "total_granted", "quota_limit");
        appendIfPresent(lines, "请求数", data, "request_count", "total_requests");

        if (lines.isEmpty()) {
            return "原始响应：\n" + truncatePreview(body);
        }
        return "配额摘要：\n" + String.join("\n", lines);
    }

    private static void appendIfPresent(@Nonnull List<String> lines, @Nonnull String label
            , @Nonnull JsonObject json, @Nonnull String... keys
    ) {
        for (String key : keys) {
            if (!json.has(key) || json.get(key).isJsonNull()) {
                continue;
            }
            lines.add("- " + label + "：" + formatJsonValue(json.get(key)));
            return;
        }
    }

    @Nonnull
    private static String formatJsonValue(@Nonnull JsonElement element) {
        if (element.isJsonNull()) {
            return "";
        }
        if (element.isJsonPrimitive()) {
            return element.getAsJsonPrimitive().getAsString();
        }
        return element.toString();
    }

    @Nonnull
    private static String truncatePreview(@Nonnull String body) {
        return body.length() > 800 ? body.substring(0, 800) + "..." : body;
    }

}
