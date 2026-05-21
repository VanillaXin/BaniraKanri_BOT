package xin.vanilla.banira.plugin.filedownload;

import cn.hutool.core.io.FileUtil;
import com.mikuac.shiro.dto.action.common.ActionRaw;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import jakarta.annotation.PreDestroy;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.other.OtherConfigRegistry;
import xin.vanilla.banira.domain.KeyValue;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.HttpUtils;
import xin.vanilla.banira.util.StringUtils;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.net.URI;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 文件下载与上传服务
 */
@Slf4j
@Component
public class FileDownloadService {

    private static final Pattern URL_PATTERN = Pattern.compile("https?://[\\w\\-._~:/?#\\[\\]@!$&'()*+,;=%]+", Pattern.CASE_INSENSITIVE);
    private static final Pattern CONTENT_DISPOSITION_FILENAME = Pattern.compile("filename\\*?=(?:UTF-8''|\"?)([^\";\\n]+)", Pattern.CASE_INSENSITIVE);
    private static final int MAX_FILE_NAME_LENGTH = 200;
    private static final String CACHE_DIR = "cache/file-download";

    private final OtherConfigRegistry otherConfigRegistry;
    private final FileDownloadHttpSupport fileDownloadHttpSupport;
    private final AtomicInteger activeTasks = new AtomicInteger(0);
    private final ExecutorService downloadExecutor = Executors.newCachedThreadPool(r -> {
        Thread thread = new Thread(r, "file-download-worker");
        thread.setDaemon(true);
        return thread;
    });
    private final ScheduledExecutorService cleanupExecutor = Executors.newSingleThreadScheduledExecutor(r -> {
        Thread thread = new Thread(r, "file-download-cleanup");
        thread.setDaemon(true);
        return thread;
    });

    public FileDownloadService(OtherConfigRegistry otherConfigRegistry,
                               FileDownloadHttpSupport fileDownloadHttpSupport) {
        this.otherConfigRegistry = otherConfigRegistry;
        this.fileDownloadHttpSupport = fileDownloadHttpSupport;
    }

    /**
     * 从文本中提取 HTTP(S) 链接
     */
    public List<String> extractUrls(String text) {
        if (StringUtils.isNullOrEmptyEx(text)) {
            return List.of();
        }
        Set<String> urls = new LinkedHashSet<>();
        Matcher matcher = URL_PATTERN.matcher(text);
        while (matcher.find()) {
            String url = trimUrlTail(matcher.group());
            if (isAllowedUrl(url)) {
                urls.add(url);
            }
        }
        return new ArrayList<>(urls);
    }

    /**
     * 异步下载并上传至当前对话
     *
     * @return true 表示已受理任务；false 表示未受理（并发已满等）
     */
    public boolean submit(BaniraBot bot, AnyMessageEvent event, String url) {
        return submitDirect(bot, event, url);
    }

    /**
     * 直接提交下载任务
     */
    public boolean submitDirect(BaniraBot bot, AnyMessageEvent event, String url) {
        return runWithTaskSlot(() -> processDownload(bot, event, url, settings()));
    }

    /**
     * 在任务并发槽内执行任务
     *
     * @return true 表示已受理；false 表示并发已满
     */
    public boolean runWithTaskSlot(Runnable task) {
        FileDownloadSettings settings = settings();
        if (!tryAcquire(settings.maxConcurrentTasks())) {
            return false;
        }
        downloadExecutor.execute(() -> {
            try {
                task.run();
            } finally {
                release();
            }
        });
        return true;
    }

    /**
     * 从 URL 提取展示用文件名
     */
    public String extractDisplayNameFromUrl(String url) {
        return extractFileNameFromUrl(url);
    }

    /**
     * 在当前已占用任务槽内执行下载（不再申请新槽）
     */
    public void downloadInCurrentTask(BaniraBot bot, AnyMessageEvent event, String url) {
        processDownload(bot, event, url, settings());
    }

    /**
     * 格式化文件大小
     */
    public String formatSize(long bytes) {
        return formatSizeInternal(bytes);
    }

    /**
     * 清理文件名，避免路径穿越与非法字符
     */
    public String sanitizeFileName(String fileName) {
        if (StringUtils.isNullOrEmptyEx(fileName)) {
            return "download";
        }
        String sanitized = fileName
                .replace('\0', '_')
                .replace("\\", "/");
        int slash = sanitized.lastIndexOf('/');
        if (slash >= 0) {
            sanitized = sanitized.substring(slash + 1);
        }
        sanitized = sanitized.replaceAll("[/:*?\"<>|\\x00-\\x1f]", "_").trim();
        sanitized = sanitized.replaceAll("^\\.+", "").replaceAll("\\.+$", "");
        if (sanitized.isEmpty()) {
            sanitized = "download";
        }
        if (sanitized.length() > MAX_FILE_NAME_LENGTH) {
            sanitized = sanitized.substring(0, MAX_FILE_NAME_LENGTH);
        }
        return BaniraUtils.replaceSensitiveFileName(sanitized);
    }

    /**
     * 根据 URL 与响应头解析展示文件名
     */
    public String resolveFileName(String url, String contentDisposition, List<String> autoSuffixes) {
        String fileName = extractFileNameFromContentDisposition(contentDisposition);
        if (StringUtils.isNullOrEmptyEx(fileName)) {
            fileName = extractFileNameFromUrl(url);
        }
        fileName = sanitizeFileName(fileName);
        if (!hasExtension(fileName) && autoSuffixes != null) {
            for (String suffix : autoSuffixes) {
                if (StringUtils.isNotNullOrEmpty(suffix)) {
                    String normalized = suffix.startsWith(".") ? suffix : "." + suffix;
                    return fileName + normalized;
                }
            }
        }
        return fileName;
    }

    @PreDestroy
    public void shutdown() {
        downloadExecutor.shutdown();
        cleanupExecutor.shutdown();
    }

    // region 下载流程

    private void processDownload(BaniraBot bot, AnyMessageEvent event, String url, FileDownloadSettings settings) {
        try {
            KeyValue<byte[], String> downloaded = downloadWithLimit(url, settings.maxFileSizeBytes());
            if (downloaded == null || downloaded.getKey() == null || downloaded.getKey().length == 0) {
                notifyFail(bot, event, "无法获取文件或文件为空");
                return;
            }
            String displayName = resolveFileName(url, downloaded.getValue(), settings.autoSuffixes());
            File cacheFile = saveToCache(downloaded.getKey(), displayName);
            if (cacheFile == null) {
                notifyFail(bot, event, "无法写入缓存");
                return;
            }
            boolean uploaded = uploadToConversation(bot, event, cacheFile, displayName);
            scheduleDeletion(cacheFile, settings.fileRetentionSeconds());
            if (uploaded) {
                notifySuccess(bot, event);
            } else {
                notifyFail(bot, event, "文件已下载，但上传至对话失败");
            }
        } catch (FileTooLargeException e) {
            notifyFail(bot, event, "文件大小超过限制（最大 " + formatSizeInternal(settings.maxFileSizeBytes()) + "）");
        } catch (Exception e) {
            LOGGER.error("Failed to download file from {}", url, e);
            notifyFail(bot, event, e.getMessage());
        }
    }

    private KeyValue<byte[], String> downloadWithLimit(String url, long maxBytes) throws Exception {
        KeyValue<String, String> metadata = probeDownloadMetadata(HttpUtils.encodeUrlIfNeeded(url), maxBytes);
        String currentUrl = metadata != null ? metadata.getKey() : HttpUtils.encodeUrlIfNeeded(url);
        String contentDisposition = metadata != null ? metadata.getValue() : null;

        for (int redirect = 0; redirect <= 5; redirect++) {
            HttpRequest getRequest = HttpRequest.newBuilder()
                    .uri(URI.create(currentUrl))
                    .GET()
                    .header("User-Agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36")
                    .build();
            HttpResponse<InputStream> response = fileDownloadHttpSupport.client().send(getRequest, HttpResponse.BodyHandlers.ofInputStream());
            if (isRedirect(response.statusCode())) {
                String location = response.headers().firstValue("Location").orElse(null);
                if (StringUtils.isNullOrEmptyEx(location)) {
                    return null;
                }
                currentUrl = resolveRedirectLocation(currentUrl, location);
                continue;
            }
            if (response.statusCode() < 200 || response.statusCode() >= 300) {
                return null;
            }
            if (StringUtils.isNullOrEmptyEx(contentDisposition)) {
                contentDisposition = response.headers().firstValue("Content-Disposition").orElse(null);
            }
            response.headers().firstValue("Content-Length").ifPresent(lengthText -> {
                long length = StringUtils.toLong(lengthText, -1L);
                if (length > maxBytes) {
                    throw new FileTooLargeException();
                }
            });
            try (InputStream inputStream = response.body();
                 ByteArrayOutputStream outputStream = new ByteArrayOutputStream()) {
                byte[] buffer = new byte[8192];
                long total = 0;
                int read;
                while ((read = inputStream.read(buffer)) != -1) {
                    total += read;
                    if (total > maxBytes) {
                        throw new FileTooLargeException();
                    }
                    outputStream.write(buffer, 0, read);
                }
                return new KeyValue<>(outputStream.toByteArray(), contentDisposition);
            }
        }
        return null;
    }

    private KeyValue<String, String> probeDownloadMetadata(String url, long maxBytes) throws Exception {
        String currentUrl = url;
        for (int redirect = 0; redirect <= 5; redirect++) {
            HttpRequest headRequest = HttpRequest.newBuilder()
                    .uri(URI.create(currentUrl))
                    .method("HEAD", HttpRequest.BodyPublishers.noBody())
                    .header("User-Agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36")
                    .build();
            HttpResponse<Void> headResponse = fileDownloadHttpSupport.client().send(headRequest, HttpResponse.BodyHandlers.discarding());
            if (isRedirect(headResponse.statusCode())) {
                String location = headResponse.headers().firstValue("Location").orElse(null);
                if (StringUtils.isNullOrEmptyEx(location)) {
                    return new KeyValue<>(currentUrl, null);
                }
                currentUrl = resolveRedirectLocation(currentUrl, location);
                continue;
            }
            if (headResponse.statusCode() >= 200 && headResponse.statusCode() < 300) {
                headResponse.headers().firstValue("Content-Length").ifPresent(lengthText -> {
                    long length = StringUtils.toLong(lengthText, -1L);
                    if (length > maxBytes) {
                        throw new FileTooLargeException();
                    }
                });
                return new KeyValue<>(currentUrl, headResponse.headers().firstValue("Content-Disposition").orElse(null));
            }
            return null;
        }
        return new KeyValue<>(currentUrl, null);
    }

    private File saveToCache(byte[] bytes, String displayName) {
        File dir = new File(CACHE_DIR);
        FileUtil.mkdir(dir);
        String safeStem = sanitizeFileName(stripExtension(displayName));
        String extension = extractExtension(displayName);
        String cacheName = safeStem + "_" + StringUtils.md5(bytes).substring(0, 8) + extension;
        File file = new File(dir, cacheName);
        return FileUtil.writeBytes(bytes, file);
    }

    private boolean uploadToConversation(BaniraBot bot, AnyMessageEvent event, File file, String displayName) {
        String path = file.getAbsolutePath();
        ActionRaw result;
        if (BaniraUtils.isGroupIdValid(event.getGroupId())) {
            result = bot.uploadGroupFile(event.getGroupId(), path, displayName, "");
        } else {
            result = bot.uploadPrivateFile(event.getSender().getUserId(), path, displayName);
        }
        return result != null && "ok".equalsIgnoreCase(result.getStatus());
    }

    private void scheduleDeletion(File file, long retentionSeconds) {
        long delay = Math.max(0L, retentionSeconds);
        cleanupExecutor.schedule(() -> FileUtil.del(file), delay, TimeUnit.SECONDS);
    }

    // endregion 下载流程

    // region 工具方法

    private FileDownloadSettings settings() {
        return otherConfigRegistry.getShared(FileDownloadSettings.class);
    }

    private boolean tryAcquire(int maxConcurrentTasks) {
        int limit = Math.max(1, maxConcurrentTasks);
        while (true) {
            int current = activeTasks.get();
            if (current >= limit) {
                return false;
            }
            if (activeTasks.compareAndSet(current, current + 1)) {
                return true;
            }
        }
    }

    private void release() {
        activeTasks.decrementAndGet();
    }

    private boolean isAllowedUrl(String url) {
        if (StringUtils.isNullOrEmptyEx(url)) {
            return false;
        }
        String lower = url.trim().toLowerCase();
        return lower.startsWith("http://") || lower.startsWith("https://");
    }

    private String trimUrlTail(String url) {
        if (url == null) {
            return "";
        }
        return url.replaceAll("[)\\]}>,.;!?]+$", "");
    }

    private String extractFileNameFromUrl(String url) {
        if (StringUtils.isNullOrEmptyEx(url)) {
            return "download";
        }
        String path = url;
        int queryIndex = path.indexOf('?');
        if (queryIndex >= 0) {
            path = path.substring(0, queryIndex);
        }
        int fragmentIndex = path.indexOf('#');
        if (fragmentIndex >= 0) {
            path = path.substring(0, fragmentIndex);
        }
        path = path.replace("\\", "/");
        int slash = path.lastIndexOf('/');
        if (slash >= 0 && slash < path.length() - 1) {
            return path.substring(slash + 1);
        }
        return "download";
    }

    private String extractFileNameFromContentDisposition(String contentDisposition) {
        if (StringUtils.isNullOrEmptyEx(contentDisposition)) {
            return null;
        }
        Matcher matcher = CONTENT_DISPOSITION_FILENAME.matcher(contentDisposition);
        if (!matcher.find()) {
            return null;
        }
        String encoded = matcher.group(1).trim();
        if (encoded.startsWith("\"") && encoded.endsWith("\"")) {
            encoded = encoded.substring(1, encoded.length() - 1);
        }
        try {
            return java.net.URLDecoder.decode(encoded, StandardCharsets.UTF_8);
        } catch (Exception e) {
            return encoded;
        }
    }

    private boolean hasExtension(String fileName) {
        if (StringUtils.isNullOrEmptyEx(fileName)) {
            return false;
        }
        int dot = fileName.lastIndexOf('.');
        return dot > 0 && dot < fileName.length() - 1;
    }

    private String extractExtension(String fileName) {
        if (!hasExtension(fileName)) {
            return "";
        }
        return fileName.substring(fileName.lastIndexOf('.'));
    }

    private String stripExtension(String fileName) {
        if (!hasExtension(fileName)) {
            return fileName;
        }
        return fileName.substring(0, fileName.lastIndexOf('.'));
    }

    private boolean isRedirect(int statusCode) {
        return statusCode == 301 || statusCode == 302 || statusCode == 303 || statusCode == 307 || statusCode == 308;
    }

    private String resolveRedirectLocation(String currentUrl, String location) throws Exception {
        URI base = new URI(currentUrl);
        if (location.startsWith("http://") || location.startsWith("https://")) {
            return location;
        }
        return base.resolve(location).toString();
    }

    private String formatSizeInternal(long bytes) {
        if (bytes >= 1_073_741_824L) {
            return String.format("%.1f GB", bytes / 1_073_741_824.0);
        }
        if (bytes >= 1_048_576L) {
            return String.format("%.1f MB", bytes / 1_048_576.0);
        }
        if (bytes >= 1024L) {
            return String.format("%.1f KB", bytes / 1024.0);
        }
        return bytes + " B";
    }

    private void notifySuccess(BaniraBot bot, AnyMessageEvent event) {
        FileDownloadFeedback.success(bot, event.getMessageId());
    }

    private void notifyFail(BaniraBot bot, AnyMessageEvent event, String message) {
        FileDownloadFeedback.fail(bot, event, message);
    }

    // endregion 工具方法

    private static class FileTooLargeException extends RuntimeException {
    }

}
