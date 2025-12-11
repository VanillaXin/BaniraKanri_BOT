package xin.vanilla.banira.util;

import lombok.extern.slf4j.Slf4j;
import xin.vanilla.banira.domain.KeyValue;
import xin.vanilla.banira.util.http.HttpMethod;
import xin.vanilla.banira.util.http.HttpRequestBuilder;
import xin.vanilla.banira.util.http.HttpResponse;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLEncoder;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Slf4j
public final class HttpUtils {

    private HttpUtils() {
    }

    static final HttpClient client = HttpClient.newBuilder()
            .followRedirects(HttpClient.Redirect.NEVER)
            .build();

    private static final String DEFAULT_USER_AGENT =
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36";

    /**
     * 下载远程文件到 byte[]
     */
    @SafeVarargs
    public static byte[] downloadBytes(String url, KeyValue<String, String>... headers) {
        return downloadWithRedirectHandling(url, 5, headers);
    }

    /**
     * 手动处理重定向的下载方法
     */
    @SafeVarargs
    private static byte[] downloadWithRedirectHandling(String url, int maxRedirects, KeyValue<String, String>... headers) {
        if (maxRedirects < 0) {
            LOGGER.error("Too many redirects for URL: {}", url);
            return null;
        }
        try {
            String currentUrl = encodeUrlIfNeeded(url);
            HttpRequest.Builder builder = HttpRequest.newBuilder()
                    .uri(URI.create(currentUrl))
                    .GET();
            if (CollectionUtils.isNotNullOrEmpty(headers)) {
                for (KeyValue<String, String> header : headers) {
                    builder.header(header.getKey(), header.getValue());
                }
            }
            HttpRequest request = builder.build();
            java.net.http.HttpResponse<byte[]> response = client.send(request, java.net.http.HttpResponse.BodyHandlers.ofByteArray());
            // 处理重定向
            if (response.statusCode() >= 300 && response.statusCode() < 400) {
                String redirectUrl = response.headers().firstValue("Location").orElse(null);
                if (redirectUrl != null) {
                    LOGGER.debug("Redirecting from {} to {}", url, redirectUrl);
                    return downloadWithRedirectHandling(redirectUrl, maxRedirects - 1, headers);
                }
            }
            if (response.statusCode() >= 200 && response.statusCode() < 300) {
                return response.body();
            }
        } catch (IllegalArgumentException e) {
            LOGGER.warn("URL contains illegal characters: {}, encoding and retrying", url);
            return downloadWithRedirectHandling(encodeUrlIfNeeded(url), maxRedirects - 1, headers);
        } catch (Exception e) {
            LOGGER.error("Failed to download file from {}", url, e);
        }
        return null;
    }

    /**
     * 检查并编码URL
     */
    public static String encodeUrlIfNeeded(String url) {
        try {
            // 尝试直接创建URI，如果成功说明URL是有效的
            new URI(url);
            return url;
        } catch (URISyntaxException e) {
            // URL无效，进行编码
            LOGGER.debug("Encoding invalid URL: {}", url);
            return manualUrlEncode(url);
        }
    }

    /**
     * 手动URL编码
     */
    private static String manualUrlEncode(String url) {
        try {
            // 分离协议、主机和路径部分
            int protocolEnd = url.indexOf("://");
            if (protocolEnd == -1) {
                // 没有协议，对整个URL进行编码
                return URLEncoder.encode(url, StandardCharsets.UTF_8)
                        .replace("%3A", ":")
                        .replace("%2F", "/")
                        .replace("%3F", "?")
                        .replace("%3D", "=")
                        .replace("%26", "&");
            }
            String protocol = url.substring(0, protocolEnd + 3);
            String rest = url.substring(protocolEnd + 3);
            // 分离主机和路径
            int pathStart = rest.indexOf('/');
            if (pathStart == -1) {
                // 只有主机部分，不需要编码
                return url;
            }
            String host = rest.substring(0, pathStart);
            String pathAndQuery = rest.substring(pathStart);

            // 对路径和查询参数进行编码
            String encodedPathAndQuery = encodePathAndQuery(pathAndQuery);

            return protocol + host + encodedPathAndQuery;
        } catch (Exception e) {
            LOGGER.warn("Failed to encode URL: {}, using original", url, e);
            return url;
        }
    }

    /**
     * 编码路径和查询参数
     */
    private static String encodePathAndQuery(String pathAndQuery) {
        try {
            int queryStart = pathAndQuery.indexOf('?');
            if (queryStart == -1) {
                // 只有路径，没有查询参数
                return URLEncoder.encode(pathAndQuery, StandardCharsets.UTF_8)
                        .replace("%2F", "/")
                        .replace("%3A", ":");
            }
            String path = pathAndQuery.substring(0, queryStart);
            String query = pathAndQuery.substring(queryStart + 1);

            // 编码路径
            String encodedPath = URLEncoder.encode(path, StandardCharsets.UTF_8)
                    .replace("%2F", "/")
                    .replace("%3A", ":");

            // 编码查询参数，但保留特殊字符
            String encodedQuery = URLEncoder.encode(query, StandardCharsets.UTF_8)
                    .replace("%3D", "=")
                    .replace("%26", "&")
                    .replace("%2B", "+");

            return encodedPath + "?" + encodedQuery;

        } catch (Exception e) {
            return pathAndQuery;
        }
    }


    /**
     * 获取重定向后的最终URL
     *
     * @param url 原始URL
     * @return 重定向后的最终URL
     */
    public static String getRedirectedUrl(String url) {
        return getRedirectedUrl(url, 10);
    }

    /**
     * 获取重定向后的最终URL
     *
     * @param url          原始URL
     * @param maxRedirects 最大重定向次数
     * @return 重定向后的最终URL
     */
    public static String getRedirectedUrl(String url, int maxRedirects) {
        return getRedirectedUrl(url, maxRedirects, (KeyValue<String, String>[]) null);
    }

    /**
     * 获取重定向后的最终URL
     *
     * @param url     原始URL
     * @param headers 请求头
     * @return 重定向后的最终URL
     */
    @SafeVarargs
    public static String getRedirectedUrl(String url, KeyValue<String, String>... headers) {
        return getRedirectedUrl(url, 10, headers);
    }

    /**
     * 获取重定向后的最终URL
     *
     * @param url          原始URL
     * @param maxRedirects 最大重定向次数
     * @param headers      请求头
     * @return 重定向后的最终URL
     */
    @SafeVarargs
    public static String getRedirectedUrl(String url, int maxRedirects, KeyValue<String, String>... headers) {
        String currentUrl = encodeUrlIfNeeded(url);
        int redirectCount = 0;
        try {
            while (redirectCount < maxRedirects) {
                HttpRequest.Builder builder = HttpRequest.newBuilder()
                        .uri(URI.create(currentUrl))
                        .method(HttpMethod.HEAD.name(), HttpRequest.BodyPublishers.noBody())
                        .header("User-Agent", DEFAULT_USER_AGENT);

                // 添加自定义请求头
                if (CollectionUtils.isNotNullOrEmpty(headers)) {
                    for (KeyValue<String, String> header : headers) {
                        builder.header(header.getKey(), header.getValue());
                    }
                }

                HttpRequest request = builder.build();
                java.net.http.HttpResponse<Void> response = client.send(request, java.net.http.HttpResponse.BodyHandlers.discarding());
                if (isRedirect(response.statusCode())) {
                    Optional<String> locationOpt = response.headers().firstValue("Location");
                    if (locationOpt.isEmpty()) {
                        break;
                    }
                    currentUrl = resolveRedirectLocation(currentUrl, locationOpt.get());
                    redirectCount++;
                } else {
                    break;
                }
            }
            if (redirectCount >= maxRedirects) {
                throw new IOException("Too many redirects (max: " + maxRedirects + ")");
            }
            return currentUrl;
        } catch (Exception e) {
            LOGGER.error("Failed to get redirected URL for: {}", url, e);
            return url;
        }
    }

    /**
     * 获取重定向链
     *
     * @param url 原始URL
     * @return 重定向链列表
     */
    public static List<String> getRedirectChain(String url) {
        return getRedirectChain(url, (KeyValue<String, String>[]) null);
    }

    /**
     * 获取重定向链（支持自定义请求头）
     *
     * @param url     原始URL
     * @param headers 请求头（如 Cookie 等）
     * @return 重定向链列表
     */
    @SafeVarargs
    public static List<String> getRedirectChain(String url, KeyValue<String, String>... headers) {
        List<String> redirectChain = new ArrayList<>();
        String currentUrl = encodeUrlIfNeeded(url);
        redirectChain.add(currentUrl);

        try {
            while (true) {
                HttpRequest.Builder builder = HttpRequest.newBuilder()
                        .uri(URI.create(currentUrl))
                        .method(HttpMethod.HEAD.name(), HttpRequest.BodyPublishers.noBody())
                        .header("User-Agent", DEFAULT_USER_AGENT);

                // 添加自定义请求头
                if (CollectionUtils.isNotNullOrEmpty(headers)) {
                    for (KeyValue<String, String> header : headers) {
                        builder.header(header.getKey(), header.getValue());
                    }
                }

                HttpRequest request = builder.build();
                java.net.http.HttpResponse<Void> response = client.send(request, java.net.http.HttpResponse.BodyHandlers.discarding());

                if (isRedirect(response.statusCode())) {
                    Optional<String> locationOpt = response.headers().firstValue("Location");
                    if (locationOpt.isEmpty()) {
                        break;
                    }
                    currentUrl = resolveRedirectLocation(currentUrl, locationOpt.get());
                    redirectChain.add(currentUrl);
                } else {
                    break;
                }
            }
            return redirectChain;
        } catch (Exception e) {
            LOGGER.error("Failed to get redirect chain for: {}", url, e);
            return BaniraUtils.mutableListOf(currentUrl);
        }
    }

    /**
     * 便捷 GET（返回构建器）
     */
    public static HttpRequestBuilder get(String url) {
        return new HttpRequestBuilder(client, HttpMethod.GET, url);
    }

    /**
     * 便捷 POST（返回构建器）
     */
    public static HttpRequestBuilder post(String url) {
        return new HttpRequestBuilder(client, HttpMethod.POST, url);
    }

    /**
     * 便捷 GET（直接执行）
     */
    @SafeVarargs
    public static HttpResponse get(String url, KeyValue<String, String>... headers) {
        return get(url).headers(headers).execute();
    }

    /**
     * 便捷 POST（直接执行）
     */
    @SafeVarargs
    public static HttpResponse post(String url, Object body, KeyValue<String, String>... headers) {
        return post(url).headers(headers).body(body).execute();
    }

    /**
     * 便捷 GET（字符串）
     */
    @SafeVarargs
    public static String getString(String url, KeyValue<String, String>... headers) {
        HttpResponse response = get(url, headers);
        return response != null ? response.getBodyAsString() : null;
    }

    /**
     * 便捷 POST（字符串）
     */
    @SafeVarargs
    public static String postString(String url, Object body, KeyValue<String, String>... headers) {
        HttpResponse response = post(url, body, headers);
        return response != null ? response.getBodyAsString() : null;
    }

    /**
     * 便捷 GET（字节）
     */
    @SafeVarargs
    public static byte[] getBytes(String url, KeyValue<String, String>... headers) {
        HttpResponse response = get(url, headers);
        return response != null ? response.getBodyAsBytes() : null;
    }

    /**
     * 便捷 POST（字节）
     */
    @SafeVarargs
    public static byte[] postBytes(String url, Object body, KeyValue<String, String>... headers) {
        HttpResponse response = post(url, body, headers);
        return response != null ? response.getBodyAsBytes() : null;
    }

    private static boolean isRedirect(int statusCode) {
        return statusCode == HttpURLConnection.HTTP_MOVED_PERM ||
                statusCode == HttpURLConnection.HTTP_MOVED_TEMP ||
                statusCode == HttpURLConnection.HTTP_SEE_OTHER ||
                statusCode == 307 || statusCode == 308;
    }

    private static String resolveRedirectLocation(String currentUrl, String location) throws URISyntaxException {
        if (location == null || location.isEmpty()) return currentUrl;
        URI baseUrl = new URI(currentUrl);
        if (location.startsWith("/")) {
            return baseUrl.resolve(location).toString();
        }
        if (!location.startsWith("http")) {
            return baseUrl.resolve(location).toString();
        }
        return location;
    }
}
