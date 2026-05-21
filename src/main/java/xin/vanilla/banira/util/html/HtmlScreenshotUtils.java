package xin.vanilla.banira.util.html;


import com.microsoft.playwright.*;
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

                        result.getBytes().add(page.screenshot(config.getScreenshotOptions()));
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
}
