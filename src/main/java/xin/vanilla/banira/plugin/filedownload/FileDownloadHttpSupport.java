package xin.vanilla.banira.plugin.filedownload;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.other.OtherConfigRegistry;
import xin.vanilla.banira.domain.KeyValue;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.HttpUtils;
import xin.vanilla.banira.util.StringUtils;

import java.io.IOException;
import java.net.*;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.List;
import java.util.Objects;

/**
 * 文件下载插件 HTTP 客户端（支持代理）
 */
@Slf4j
@Component
public class FileDownloadHttpSupport {

    private static final String DEFAULT_USER_AGENT = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36";

    private final OtherConfigRegistry otherConfigRegistry;
    private volatile HttpClient cachedClient;
    private volatile String cachedProxyUrl;

    public FileDownloadHttpSupport(OtherConfigRegistry otherConfigRegistry) {
        this.otherConfigRegistry = otherConfigRegistry;
    }

    public HttpClient client() {
        String proxyUrl = normalizeProxyUrl(settings().proxyUrl());
        HttpClient current = cachedClient;
        if (current != null && Objects.equals(cachedProxyUrl, proxyUrl)) {
            return current;
        }
        synchronized (this) {
            if (cachedClient != null && Objects.equals(cachedProxyUrl, proxyUrl)) {
                return cachedClient;
            }
            cachedClient = buildClient(proxyUrl);
            cachedProxyUrl = proxyUrl;
            return cachedClient;
        }
    }

    @SafeVarargs
    public final String getString(String url, KeyValue<String, String>... headers) {
        try {
            HttpRequest.Builder builder = HttpRequest.newBuilder()
                    .uri(URI.create(HttpUtils.encodeUrlIfNeeded(url)))
                    .GET()
                    .header("User-Agent", DEFAULT_USER_AGENT);
            if (CollectionUtils.isNotNullOrEmpty(headers)) {
                for (KeyValue<String, String> header : headers) {
                    builder.header(header.getKey(), header.getValue());
                }
            }
            HttpResponse<String> response = client().send(builder.build(), HttpResponse.BodyHandlers.ofString());
            if (response.statusCode() >= 200 && response.statusCode() < 300) {
                return response.body();
            }
        } catch (Exception e) {
            LOGGER.error("Failed to GET {}", url, e);
        }
        return null;
    }

    private FileDownloadSettings settings() {
        return otherConfigRegistry.getShared(FileDownloadSettings.class);
    }

    private HttpClient buildClient(String proxyUrl) {
        HttpClient.Builder builder = HttpClient.newBuilder()
                .followRedirects(HttpClient.Redirect.NEVER);
        ProxySelector proxySelector = buildProxySelector(proxyUrl);
        if (proxySelector != null) {
            builder.proxy(proxySelector);
            LOGGER.info("File download HTTP client using proxy: {}", proxyUrl);
        }
        return builder.build();
    }

    private ProxySelector buildProxySelector(String proxyUrl) {
        if (StringUtils.isNullOrEmptyEx(proxyUrl)) {
            return null;
        }
        URI uri = URI.create(proxyUrl);
        String scheme = StringUtils.isNullOrEmptyEx(uri.getScheme()) ? "http" : uri.getScheme().toLowerCase();
        String host = uri.getHost();
        if (StringUtils.isNullOrEmptyEx(host)) {
            host = uri.getAuthority();
        }
        if (StringUtils.isNullOrEmptyEx(host)) {
            LOGGER.warn("Invalid file download proxy url: {}", proxyUrl);
            return null;
        }
        int port = uri.getPort();
        if (port <= 0) {
            port = switch (scheme) {
                case "https" -> 443;
                case "socks", "socks5" -> 1080;
                default -> 80;
            };
        }
        InetSocketAddress address = InetSocketAddress.createUnresolved(host, port);
        if ("socks".equals(scheme) || "socks5".equals(scheme)) {
            return socksProxySelector(address);
        }
        return ProxySelector.of(address);
    }

    private ProxySelector socksProxySelector(InetSocketAddress address) {
        return new ProxySelector() {
            @Override
            public List<Proxy> select(URI uri) {
                return List.of(new Proxy(Proxy.Type.SOCKS, address));
            }

            @Override
            public void connectFailed(URI uri, SocketAddress sa, IOException ioe) {
                LOGGER.warn("SOCKS proxy connect failed: uri={}, address={}", uri, sa, ioe);
            }
        };
    }

    private String normalizeProxyUrl(String proxyUrl) {
        if (StringUtils.isNullOrEmptyEx(proxyUrl)) {
            return "";
        }
        String trimmed = proxyUrl.trim();
        if (trimmed.contains("://")) {
            return trimmed;
        }
        return "http://" + trimmed;
    }

}
