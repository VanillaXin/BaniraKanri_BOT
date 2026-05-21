package xin.vanilla.banira.plugin.filedownload;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.List;
import java.util.Map;

/**
 * 下载页探测规则
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class FileDownloadPageRule {

    /**
     * 规则名称
     */
    private String name;

    /**
     * 是否启用
     */
    private boolean enabled;

    /**
     * 下载页 URL 匹配正则（可使用捕获组 $1、$2… 供 API 模板替换）
     */
    private String pageUrlPattern;

    /**
     * 解析模式：json-api | html-link
     */
    private String mode;

    /**
     * json-api 模式下的 API 地址模板
     */
    private String apiUrlTemplate;

    /**
     * json-api 备用 API 地址模板
     */
    private String apiUrlTemplateFallback;

    /**
     * 当指定捕获组为空时使用备用 API 模板（0 表示不启用）
     */
    private int fallbackWhenEmptyGroup;

    /**
     * json-api 请求头
     */
    private Map<String, String> headers;

    /**
     * json-api：文件列表 JSON 路径
     */
    private String itemsPath;

    /**
     * json-api：单项内下载地址路径
     */
    private String urlPath;

    /**
     * json-api：单项内名称路径
     */
    private String namePath;

    /**
     * json-api：单项内大小路径（可选）
     */
    private String sizePath;

    /**
     * json-api：响应根对象上的额外下载项
     */
    private List<FileDownloadJsonItemMapping> extraItems;

    /**
     * html-link：页面 HTML 中匹配下载链接的正则，第 1 组为 URL，第 2 组为名称
     */
    private String htmlLinkPattern;

    {
        this.name = "";
        this.enabled = true;
        this.pageUrlPattern = "";
        this.mode = "json-api";
        this.apiUrlTemplate = "";
        this.apiUrlTemplateFallback = "";
        this.fallbackWhenEmptyGroup = 0;
        this.headers = BaniraUtils.mutableMapOf();
        this.itemsPath = "";
        this.urlPath = "";
        this.namePath = "";
        this.sizePath = "";
        this.extraItems = BaniraUtils.mutableListOf();
        this.htmlLinkPattern = "";
    }

}
