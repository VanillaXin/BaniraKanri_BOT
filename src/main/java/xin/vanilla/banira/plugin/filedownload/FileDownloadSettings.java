package xin.vanilla.banira.plugin.filedownload;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import xin.vanilla.banira.config.contract.SharedConfig;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.List;

/**
 * 文件下载插件配置
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class FileDownloadSettings implements SharedConfig {

    /**
     * 最大允许的文件大小（字节）
     */
    private long maxFileSizeBytes;

    /**
     * 同时进行的下载任务数量上限
     */
    private int maxConcurrentTasks;

    /**
     * 文件保留时间（秒），上传至对话后到期自动删除
     */
    private long fileRetentionSeconds;

    /**
     * 当无法识别扩展名时自动追加的后缀（按顺序取第一个）
     */
    private List<String> autoSuffixes;

    /**
     * 下载页选择会话过期时间（秒）
     */
    private long selectionExpireSeconds;

    /**
     * 下载页 URL 重写规则（按顺序依次尝试匹配并替换）
     */
    private List<FileDownloadUrlRewriteRule> urlRewriteRules;

    /**
     * 下载页探测规则
     */
    private List<FileDownloadPageRule> pageRules;

    /**
     * HTTP 代理地址，留空则不使用代理
     * 示例：http://127.0.0.1:7890、socks5://127.0.0.1:1080
     */
    private String proxyUrl;

    {
        this.maxFileSizeBytes = 52_428_800L;
        this.maxConcurrentTasks = 3;
        this.fileRetentionSeconds = 30L;
        this.autoSuffixes = BaniraUtils.mutableListOf(".bk");
        this.selectionExpireSeconds = 300L;
        this.proxyUrl = "";
        this.urlRewriteRules = BaniraUtils.mutableListOf(
                new FileDownloadUrlRewriteRule()
                        .name("github-repo-to-releases")
                        .enabled(true)
                        .matchPattern("https://github\\.com/([^/]+)/([^/#?]+?)(?:\\.git)?/?(?:[#?].*)?$")
                        .replaceTemplate("https://github.com/$1/$2/releases")
        );
        this.pageRules = BaniraUtils.mutableListOf(
                new FileDownloadPageRule()
                        .name("github-release")
                        .enabled(true)
                        .pageUrlPattern("https://github\\.com/([^/]+)/([^/]+)/releases(?!/download)(?:/(?:tag/([^/#?]+)|latest))?/?(?:[?#].*)?")
                        .mode("json-api")
                        .apiUrlTemplate("https://api.github.com/repos/$1/$2/releases/tags/$3")
                        .apiUrlTemplateFallback("https://api.github.com/repos/$1/$2/releases/latest")
                        .fallbackWhenEmptyGroup(3)
                        .headers(BaniraUtils.mutableMapOf(
                                "Accept", "application/vnd.github+json",
                                "User-Agent", "BaniraKanri-FileDownload"
                        ))
                        .itemsPath("assets")
                        .urlPath("browser_download_url")
                        .namePath("name")
                        .sizePath("size")
                        .extraItems(BaniraUtils.mutableListOf(
                                new FileDownloadJsonItemMapping()
                                        .name("Source code (zip)")
                                        .urlPath("zipball_url"),
                                new FileDownloadJsonItemMapping()
                                        .name("Source code (tar.gz)")
                                        .urlPath("tarball_url")
                        ))
        );
    }

}
