package xin.vanilla.banira.util;


import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserType;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.Playwright;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.UUID;

/**
 * Html -> Image
 */
public class HtmlToImageUtils {

    private static final String TEMP_DIR = System.getProperty("java.io.tmpdir");
    private static Playwright playwrightInstance;

    private static synchronized Playwright getPlaywright() {
        if (playwrightInstance == null) {
            playwrightInstance = Playwright.create();
        }
        return playwrightInstance;
    }

    private static byte[] renderToBytes(String url) {
        try (Browser browser = getPlaywright().chromium().launch(
                new BrowserType.LaunchOptions().setHeadless(true))) {

            try (Page page = browser.newPage()) {
                page.navigate(url);
                return page.screenshot(new Page.ScreenshotOptions().setFullPage(true));
            }
        }
    }

    private static void renderToFile(String url, String outputFile) {
        try (Browser browser = getPlaywright().chromium().launch(
                new BrowserType.LaunchOptions().setHeadless(true))) {

            try (Page page = browser.newPage()) {
                page.navigate(url);
                page.screenshot(new Page.ScreenshotOptions()
                        .setPath(Paths.get(outputFile))
                        .setFullPage(true));
            }
        }
    }

    /**
     * URL渲染
     */
    public static InputStream renderUrlToStream(String url) {
        return new ByteArrayInputStream(renderUrlToBytes(url));
    }

    /**
     * URL渲染
     */
    public static byte[] renderUrlToBytes(String url) {
        return renderToBytes(url);
    }

    /**
     * URL渲染
     */
    public static String renderUrlToFile(String url, String outputFile) {
        renderToFile(url, outputFile);
        return outputFile;
    }

    /**
     * HTML字符串渲染
     */
    public static String renderHtmlStringToFile(String html, String outputFile) throws IOException {
        Path tempHtml = createTempHtmlFile(html);
        try {
            String fileUrl = toFileUrl(tempHtml);
            renderToFile(fileUrl, outputFile);
            return outputFile;
        } finally {
            Files.deleteIfExists(tempHtml);
        }
    }

    /**
     * HTML字符串渲染
     */
    public static byte[] renderHtmlStringToBytes(String html) throws IOException {
        Path tempHtml = createTempHtmlFile(html);
        try {
            String fileUrl = toFileUrl(tempHtml);
            return renderToBytes(fileUrl);
        } finally {
            Files.deleteIfExists(tempHtml);
        }
    }

    /**
     * HTML字符串渲染
     */
    public static InputStream renderHtmlStringToStream(String html) throws IOException {
        return new ByteArrayInputStream(renderHtmlStringToBytes(html));
    }

    /**
     * 本地文件渲染
     */
    public static String renderFileToFile(File file, String outputFile) {
        String fileUrl = toFileUrl(file.toPath());
        renderToFile(fileUrl, outputFile);
        return outputFile;
    }

    /**
     * 本地文件渲染
     */
    public static byte[] renderFileToBytes(File file) {
        return renderToBytes(toFileUrl(file.toPath()));
    }

    /**
     * 本地文件渲染
     */
    public static InputStream renderFileToStream(File file) {
        return new ByteArrayInputStream(renderFileToBytes(file));
    }

    private static Path createTempHtmlFile(String html) throws IOException {
        Path tempHtml = Paths.get(TEMP_DIR, "html-" + UUID.randomUUID() + ".html");
        Files.write(tempHtml, html.getBytes());
        return tempHtml;
    }

    private static String toFileUrl(Path path) {
        return "file:///" + path.toAbsolutePath().toString().replace("\\", "/");
    }

    public static synchronized void close() {
        if (playwrightInstance != null) {
            playwrightInstance.close();
            playwrightInstance = null;
        }
    }
}
