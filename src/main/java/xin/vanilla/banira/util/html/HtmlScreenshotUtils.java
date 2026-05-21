package xin.vanilla.banira.util.html;


import com.microsoft.playwright.*;
import com.microsoft.playwright.options.WaitUntilState;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import lombok.extern.slf4j.Slf4j;

import java.io.File;

/**
 * Html -> Image
 */
@Slf4j
public class HtmlScreenshotUtils {

    private static Playwright playwrightInstance;

    // region private

    private static synchronized Playwright getPlaywright() {
        if (playwrightInstance == null) {
            playwrightInstance = Playwright.create();
        }
        return playwrightInstance;
    }

    // endregion private

    public static HtmlScreenshotResult render(File file) {
        return render(new HtmlScreenshotConfig(file));
    }

    public static HtmlScreenshotResult render(String url) {
        return render(new HtmlScreenshotConfig(url));
    }

    public static HtmlScreenshotResult render(HtmlScreenshotConfig config) {
        HtmlScreenshotResult result = new HtmlScreenshotResult();

        try (Browser browser = getPlaywright().chromium().launch(
                new BrowserType.LaunchOptions().setHeadless(true)
        )) {
            try (BrowserContext context = browser.newContext(config.getContextOptions())) {
                try (Page page = context.newPage()) {
                    page.navigate(config.getUrl());
                    waitForReady(page, config);

                    // 若未设置录制参数，只截取一张图片
                    if (config.getDuration() == null || config.getInterval() == null) {
                        if (config.getInterval() != null) page.waitForTimeout(config.getInterval());

                        if (config.getClipSelector() != null && !config.getClipSelector().isBlank()) {
                            result.getBytes().add(page.locator(config.getClipSelector()).screenshot());
                        } else {
                            result.getBytes().add(page.screenshot(config.getScreenshotOptions()));
                        }
                    } else {
                        // 录制指定时长的截图
                        int duration = config.getDuration();
                        int interval = config.getInterval();
                        int elapsed = 0;

                        while (elapsed < duration) {
                            // 截取当前画面
                            result.getBytes().add(page.screenshot(config.getScreenshotOptions()));

                            // 等待指定间隔
                            page.waitForTimeout(interval);
                            elapsed += interval;
                        }
                    }
                }
            }

        }
        return result;
    }

    private static void waitForReady(Page page, HtmlScreenshotConfig config) {
        if (config.getReadyExpression() == null) return;
        int timeout = config.getReadyTimeout() != null ? config.getReadyTimeout() : 8000;
        try {
            page.waitForFunction(config.getReadyExpression(), new Page.WaitForFunctionOptions().setTimeout(timeout));
        } catch (PlaywrightException e) {
            LOGGER.warn("Page ready wait timed out, screenshot will proceed: {}", config.getReadyExpression());
        }
    }

    public static synchronized void close() {
        if (playwrightInstance != null) {
            playwrightInstance.close();
            playwrightInstance = null;
        }
    }

    /**
     * 使用 Playwright 抓取远程页面（可执行 JS 挑战）
     */
    @Nullable
    public static String fetchRemoteHtml(@Nonnull String url, @Nonnull String readySelector) {
        try (Browser browser = getPlaywright().chromium().launch(
                new BrowserType.LaunchOptions().setHeadless(true)
        )) {
            try (BrowserContext context = browser.newContext(new Browser.NewContextOptions()
                    .setUserAgent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
                    .setLocale("zh-CN"))) {
                try (Page page = context.newPage()) {
                    page.navigate(url, new Page.NavigateOptions()
                            .setTimeout(45000)
                            .setWaitUntil(WaitUntilState.DOMCONTENTLOADED));
                    try {
                        page.waitForSelector(readySelector, new Page.WaitForSelectorOptions().setTimeout(25000));
                    } catch (PlaywrightException e) {
                        LOGGER.warn("Remote page selector wait timeout: {}", url);
                    }
                    return page.content();
                }
            }
        } catch (Exception e) {
            LOGGER.warn("Failed to fetch remote html: {}", url, e);
            return null;
        }
    }

    /**
     * 宽松抓取远程页面（不依赖特定选择器，等待页面加载后返回 HTML）
     */
    @Nullable
    public static String fetchRemoteHtmlLoose(@Nonnull String url, int waitMs) {
        try (Browser browser = getPlaywright().chromium().launch(
                new BrowserType.LaunchOptions().setHeadless(true)
        )) {
            try (BrowserContext context = browser.newContext(new Browser.NewContextOptions()
                    .setUserAgent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
                    .setLocale("zh-CN"))) {
                try (Page page = context.newPage()) {
                    page.navigate(url, new Page.NavigateOptions()
                            .setTimeout(45000)
                            .setWaitUntil(WaitUntilState.LOAD));
                    if (waitMs > 0) {
                        page.waitForTimeout(waitMs);
                    }
                    return page.content();
                }
            }
        } catch (Exception e) {
            LOGGER.warn("Failed to fetch remote html loosely: {}", url, e);
            return null;
        }
    }
}
