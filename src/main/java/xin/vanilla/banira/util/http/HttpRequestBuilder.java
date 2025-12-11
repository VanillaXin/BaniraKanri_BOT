package xin.vanilla.banira.util.http;

import lombok.extern.slf4j.Slf4j;
import xin.vanilla.banira.domain.KeyValue;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.HttpUtils;
import xin.vanilla.banira.util.JsonUtils;

import java.net.URI;
import java.net.URLEncoder;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

/**
 * HTTP 请求构建器
 */
@Slf4j
public class HttpRequestBuilder {

    private final String url;
    private final HttpMethod method;
    private final HttpClient client;
    private final Map<String, String> headers = new LinkedHashMap<>();
    private Object body;
    private String contentType;
    private Duration timeout;

    public HttpRequestBuilder(HttpClient client, HttpMethod method, String url) {
        this.client = client;
        this.method = method;
        this.url = url;
    }

    public HttpRequestBuilder header(String key, String value) {
        if (key != null && value != null) {
            headers.put(key, value);
        }
        return this;
    }

    @SafeVarargs
    public final HttpRequestBuilder headers(KeyValue<String, String>... headers) {
        if (CollectionUtils.isNotNullOrEmpty(headers)) {
            for (KeyValue<String, String> header : headers) {
                header(header.getKey(), header.getValue());
            }
        }
        return this;
    }

    public HttpRequestBuilder contentType(String contentType) {
        this.contentType = contentType;
        return this;
    }

    public HttpRequestBuilder body(Object body) {
        this.body = body;
        return this;
    }

    public HttpRequestBuilder jsonBody(Object body) {
        this.contentType = "application/json";
        this.body = body;
        return this;
    }

    public HttpRequestBuilder xmlBody(String xml) {
        this.contentType = "application/xml";
        this.body = xml;
        return this;
    }

    public HttpRequestBuilder formBody(Map<String, String> formData) {
        this.contentType = "application/x-www-form-urlencoded";
        this.body = formData;
        return this;
    }

    @SafeVarargs
    public final HttpRequestBuilder formBody(KeyValue<String, String>... formData) {
        this.contentType = "application/x-www-form-urlencoded";
        if (CollectionUtils.isNotNullOrEmpty(formData)) {
            this.body = Arrays.stream(formData)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toMap(KeyValue::getKey, KeyValue::getValue, (a, b) -> b, LinkedHashMap::new));
        }
        return this;
    }

    public HttpRequestBuilder timeout(Duration timeout) {
        this.timeout = timeout;
        return this;
    }

    /**
     * 同步执行
     */
    public HttpResponse execute() {
        try {
            HttpRequest request = buildRequest();
            java.net.http.HttpResponse<byte[]> response = client.send(request, java.net.http.HttpResponse.BodyHandlers.ofByteArray());
            return HttpResponse.from(response);
        } catch (Exception e) {
            LOGGER.error("HTTP {} {} failed", method, url, e);
            return new HttpResponse(500, Map.of(), null);
        }
    }

    /**
     * 异步执行
     */
    public CompletableFuture<HttpResponse> executeAsync() {
        try {
            HttpRequest request = buildRequest();
            return client.sendAsync(request, java.net.http.HttpResponse.BodyHandlers.ofByteArray())
                    .thenApply(HttpResponse::from);
        } catch (Exception e) {
            CompletableFuture<HttpResponse> future = new CompletableFuture<>();
            future.completeExceptionally(e);
            return future;
        }
    }

    private HttpRequest buildRequest() {
        String finalUrl = HttpUtils.encodeUrlIfNeeded(url);
        HttpRequest.Builder builder = HttpRequest.newBuilder()
                .uri(URI.create(finalUrl))
                .method(method.name(), createBodyPublisher());

        headers.forEach(builder::header);
        if (timeout != null) {
            builder.timeout(timeout);
        }
        return builder.build();
    }

    private HttpRequest.BodyPublisher createBodyPublisher() {
        // GET/HEAD 没有 body
        if (method == HttpMethod.GET || method == HttpMethod.HEAD) {
            return HttpRequest.BodyPublishers.noBody();
        }

        if (body instanceof HttpRequest.BodyPublisher publisher) {
            return publisher;
        }

        // contentType 自动推断
        if (contentType == null && body != null) {
            if (body instanceof Map || body instanceof KeyValue[]) {
                // Map 或 KeyValue 数组自动使用 form-urlencoded
                contentType = "application/x-www-form-urlencoded";
            } else if (!(body instanceof String) && !(body instanceof byte[])) {
                // 其他对象类型使用 JSON
                contentType = "application/json";
            }
        }

        // 按 contentType 构建
        if ("application/json".equalsIgnoreCase(contentType)) {
            String json = JsonUtils.toJsonString(body);
            headerIfAbsent("Content-Type", "application/json; charset=UTF-8");
            return HttpRequest.BodyPublishers.ofString(json == null ? "" : json, StandardCharsets.UTF_8);
        }

        if ("application/xml".equalsIgnoreCase(contentType) || "text/xml".equalsIgnoreCase(contentType)) {
            String xml = body == null ? "" : body.toString();
            headerIfAbsent("Content-Type", contentType + "; charset=UTF-8");
            return HttpRequest.BodyPublishers.ofString(xml, StandardCharsets.UTF_8);
        }

        if ("application/x-www-form-urlencoded".equalsIgnoreCase(contentType)) {
            String encoded = encodeFormBody(body);
            headerIfAbsent("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");
            return HttpRequest.BodyPublishers.ofString(encoded, StandardCharsets.UTF_8);
        }

        // 默认处理字符串与字节
        if (body instanceof byte[] bytes) {
            headerIfAbsent("Content-Type", contentType != null ? contentType : "application/octet-stream");
            return HttpRequest.BodyPublishers.ofByteArray(bytes);
        }
        String text = body == null ? "" : body.toString();
        if (contentType != null) {
            headerIfAbsent("Content-Type", contentType);
        }
        return HttpRequest.BodyPublishers.ofString(text, StandardCharsets.UTF_8);
    }

    private void headerIfAbsent(String key, String value) {
        if (!headers.containsKey(key)) {
            headers.put(key, value);
        }
    }

    private String encodeFormBody(Object body) {
        if (body == null) return "";
        Map<String, String> map = new LinkedHashMap<>();
        if (body instanceof Map<?, ?> bMap) {
            for (Map.Entry<?, ?> entry : bMap.entrySet()) {
                if (entry.getKey() != null && entry.getValue() != null) {
                    map.put(entry.getKey().toString(), entry.getValue().toString());
                }
            }
        } else if (body instanceof KeyValue<?, ?>[] kvs) {
            for (KeyValue<?, ?> kv : kvs) {
                if (kv != null && kv.getKey() != null && kv.getValue() != null) {
                    map.put(kv.getKey().toString(), kv.getValue().toString());
                }
            }
        } else {
            return URLEncoder.encode(body.toString(), StandardCharsets.UTF_8);
        }

        return map.entrySet().stream()
                .map(e -> URLEncoder.encode(e.getKey(), StandardCharsets.UTF_8) + "=" +
                        URLEncoder.encode(e.getValue(), StandardCharsets.UTF_8))
                .collect(Collectors.joining("&"));
    }
}

