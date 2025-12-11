package xin.vanilla.banira.util.http;

import lombok.Getter;
import lombok.experimental.Accessors;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * HTTP 响应封装
 */
@Getter
@Accessors(fluent = true)
public class HttpResponse {

    private final int statusCode;
    private final Map<String, List<String>> headers;
    private final byte[] bodyBytes;
    private Map<String, String> cookies;
    private String bodyString;

    public HttpResponse(int statusCode, Map<String, List<String>> headers, byte[] bodyBytes) {
        this.statusCode = statusCode;
        this.headers = headers == null ? Collections.emptyMap() : Collections.unmodifiableMap(headers);
        this.bodyBytes = bodyBytes;
    }

    public static HttpResponse from(java.net.http.HttpResponse<byte[]> response) {
        Map<String, List<String>> headerMap = new LinkedHashMap<>(response.headers().map());
        return new HttpResponse(response.statusCode(), headerMap, response.body());
    }

    /**
     * 获取指定头（忽略大小写）
     */
    public List<String> getHeaders(String name) {
        if (name == null) return Collections.emptyList();
        for (Map.Entry<String, List<String>> entry : headers.entrySet()) {
            if (entry.getKey() != null && entry.getKey().equalsIgnoreCase(name)) {
                return entry.getValue();
            }
        }
        return Collections.emptyList();
    }

    /**
     * 获取第一个头部值（忽略大小写）
     */
    public String getFirstHeader(String name) {
        return getHeaders(name).stream().findFirst().orElse(null);
    }

    /**
     * 获取响应体字符串，自动根据 charset 解析，默认 UTF-8
     */
    public String getBodyAsString() {
        if (bodyBytes == null) return null;
        if (bodyString != null) return bodyString;
        Charset charset = resolveCharset();
        bodyString = new String(bodyBytes, charset);
        return bodyString;
    }

    /**
     * 获取原始字节
     */
    public byte[] getBodyAsBytes() {
        return bodyBytes;
    }

    /**
     * 解析 Cookies
     */
    public Map<String, String> getCookies() {
        if (cookies != null) {
            return cookies;
        }
        cookies = new HashMap<>();
        List<String> setCookies = getHeaders("Set-Cookie");
        for (String setCookie : setCookies) {
            if (setCookie == null || setCookie.isEmpty()) {
                continue;
            }
            String[] parts = setCookie.split(";");
            if (parts.length == 0) continue;
            String[] kv = parts[0].split("=", 2);
            if (kv.length == 2) {
                cookies.put(kv[0].trim(), kv[1].trim());
            }
        }
        return cookies;
    }

    /**
     * 获取单个 Cookie
     */
    public String getCookie(String name) {
        return Optional.ofNullable(getCookies()).map(m -> m.get(name)).orElse(null);
    }

    private Charset resolveCharset() {
        String contentType = getFirstHeader("Content-Type");
        if (contentType == null) {
            return StandardCharsets.UTF_8;
        }
        String lower = contentType.toLowerCase(Locale.ROOT);
        int idx = lower.indexOf("charset=");
        if (idx >= 0) {
            String cs = lower.substring(idx + "charset=".length()).trim();
            try {
                return Charset.forName(cs);
            } catch (Exception ignored) {
            }
        }
        return StandardCharsets.UTF_8;
    }
}
