package xin.vanilla.banira.util.html;

import com.microsoft.playwright.Browser;
import com.microsoft.playwright.Page;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import xin.vanilla.banira.util.BaniraUtils;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.UUID;

@Data
@NoArgsConstructor
@Accessors(chain = true)
public class HtmlScreenshotConfig {
    /**
     * 浏览器上下文配置
     */
    private Browser.NewContextOptions contextOptions;
    /**
     * 页面截图配置
     */
    private Page.ScreenshotOptions screenshotOptions;
    /**
     * 录制时长（毫秒）
     */
    private Integer duration;
    /**
     * 截图间隔（毫秒）
     */
    private Integer interval;
    /**
     * 截图前等待页面就绪的 JS 表达式
     */
    private String readyExpression;
    /**
     * 等待就绪超时（毫秒）
     */
    private Integer readyTimeout;
    /**
     * 网页地址
     */
    private String url;
    /**
     * 仅截取指定元素（如 .card），避免 viewport 留白
     */
    private String clipSelector;

    private static final String CONFIG_SCRIPT_TAG = "<script src=\"config.js\"></script>";

    public HtmlScreenshotConfig(String content) {
        if (BaniraUtils.isValidUri(content)) {
            this.url = content;
        } else if (BaniraUtils.isValidPath(content)) {
            this.url = toFileUrl(Path.of(content));
        } else {
            try {
                this.url = toFileUrl(createTempHtmlFile(content));
            } catch (IOException e) {
                this.url = content;
            }
        }
    }

    public HtmlScreenshotConfig(File file) {
        this.url = toFileUrl(file.toPath());
    }

    public Browser.NewContextOptions getContextOptions() {
        return this.contextOptions != null
                ? this.contextOptions
                : new Browser.NewContextOptions();
    }

    public Page.ScreenshotOptions getScreenshotOptions() {
        return this.screenshotOptions != null
                ? this.screenshotOptions
                : new Page.ScreenshotOptions().setFullPage(true);
    }

    public static HtmlScreenshotConfig defaultConfig() {
        return new HtmlScreenshotConfig()
                .setContextOptions(
                        new Browser.NewContextOptions()
                )
                .setScreenshotOptions(
                        new Page.ScreenshotOptions()
                                .setFullPage(true)
                );
    }

    private static String toFileUrl(Path path) {
        return "file:///" + path.toAbsolutePath().toString().replace("\\", "/");
    }

    private static final String TEMP_DIR = System.getProperty("java.io.tmpdir");

    private static Path createTempHtmlFile(String html) throws IOException {
        Path tempHtml = Paths.get(TEMP_DIR, "html-" + UUID.randomUUID() + ".html");
        Files.writeString(tempHtml, html, StandardCharsets.UTF_8);
        return tempHtml;
    }

    /**
     * 基于模板生成独立渲染文件（内联 configData），避免多线程共享 config.js 产生竞态
     */
    @Nonnull
    public static File buildInlineConfigRenderFile(@Nonnull File templateFile, @Nonnull String configJson) throws IOException {
        String template = Files.readString(templateFile.toPath(), StandardCharsets.UTF_8);
        String inlineConfig = "<script>\nconst configData = " + configJson + ";\n</script>";
        String html = template.replace(CONFIG_SCRIPT_TAG, inlineConfig);
        Path renderPath = templateFile.toPath().getParent().resolve("render-" + UUID.randomUUID() + ".html");
        Files.writeString(renderPath, html, StandardCharsets.UTF_8);
        return renderPath.toFile();
    }

    public static void deleteQuietly(@Nullable File file) {
        if (file == null) {
            return;
        }
        try {
            Files.deleteIfExists(file.toPath());
        } catch (IOException ignored) {
        }
    }
}
