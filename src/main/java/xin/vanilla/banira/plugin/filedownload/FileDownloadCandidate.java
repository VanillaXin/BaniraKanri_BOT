package xin.vanilla.banira.plugin.filedownload;

import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * 可下载文件候选项
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
public class FileDownloadCandidate {

    /**
     * 展示名称
     */
    private String name;

    /**
     * 下载直链
     */
    private String url;

    /**
     * 文件大小（字节），未知时为 -1
     */
    private long size;

    {
        this.name = "";
        this.url = "";
        this.size = -1L;
    }

}
