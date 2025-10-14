package xin.vanilla.banira.util;

import lombok.extern.slf4j.Slf4j;
import xin.vanilla.banira.domain.KeyValue;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

@Slf4j
public final class HttpUtils {

    private HttpUtils() {
    }

    private static final HttpClient client = HttpClient.newBuilder()
            .followRedirects(HttpClient.Redirect.ALWAYS)
            .build();

    /**
     * 下载远程文件到 byte[]
     */
    @SafeVarargs
    public static byte[] downloadBytes(String url, KeyValue<String, String>... headers) {
        try {
            HttpRequest.Builder builder = HttpRequest.newBuilder()
                    .uri(URI.create(url))
                    .GET();
            if (CollectionUtils.isNotNullOrEmpty(headers)) {
                for (KeyValue<String, String> header : headers) {
                    builder.header(header.getKey(), header.getValue());
                }
            }
            HttpRequest request = builder.build();
            HttpResponse<byte[]> response = client.send(request, HttpResponse.BodyHandlers.ofByteArray());
            if (response.statusCode() >= 200 && response.statusCode() < 300) {
                return response.body();
            }
        } catch (Exception ignored) {
            LOGGER.error("Failed to download file from {}", url);
        }
        return null;
    }
}
