package xin.vanilla.banira.util;

import lombok.extern.slf4j.Slf4j;
import xin.vanilla.banira.domain.KeyValue;

import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLEncoder;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;

@Slf4j
public final class HttpUtils {

    private HttpUtils() {
    }

    private static final HttpClient client = HttpClient.newBuilder()
            .followRedirects(HttpClient.Redirect.NEVER)
            .build();

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
            HttpResponse<byte[]> response = client.send(request, HttpResponse.BodyHandlers.ofByteArray());
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
    private static String encodeUrlIfNeeded(String url) {
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
}
