package xin.vanilla.banira.plugin.filedownload;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * 下载页 URL 重写规则（嗅探前将用户输入地址规范化为下载页地址）
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class FileDownloadUrlRewriteRule {

    /**
     * 规则名称
     */
    private String name;

    /**
     * 是否启用
     */
    private boolean enabled;

    /**
     * 待匹配 URL 正则（需完整匹配）
     */
    private String matchPattern;

    /**
     * 替换模板，使用 $1、$2… 引用捕获组
     */
    private String replaceTemplate;

    {
        this.name = "";
        this.enabled = true;
        this.matchPattern = "";
        this.replaceTemplate = "";
    }

}
