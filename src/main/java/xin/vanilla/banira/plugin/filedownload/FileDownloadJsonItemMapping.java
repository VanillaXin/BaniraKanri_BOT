package xin.vanilla.banira.plugin.filedownload;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * JSON 响应中的额外下载项映射（用于数组外的单条资源）
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class FileDownloadJsonItemMapping {

    /**
     * 展示名称
     */
    private String name;

    /**
     * 下载地址 JSON 路径
     */
    private String urlPath;

    /**
     * 文件大小 JSON 路径（可选）
     */
    private String sizePath;

    {
        this.name = "";
        this.urlPath = "";
        this.sizePath = "";
    }

}
