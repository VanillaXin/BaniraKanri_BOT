package xin.vanilla.banira.plugin.filedownload;

import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.util.ArrayList;
import java.util.List;

/**
 * 下载文件选择会话
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
public class FileDownloadSelectionSession {

    /**
     * 选择提示消息 ID（用户需回复此消息）
     */
    private Long messageId;

    /**
     * 发起选择的用户
     */
    private Long userId;

    /**
     * 群号，私聊为 0
     */
    private Long groupId;

    /**
     * 来源页面 URL
     */
    private String sourceUrl;

    /**
     * 可选项
     */
    private List<FileDownloadCandidate> candidates;

    /**
     * 过期时间戳（毫秒）
     */
    private long expireAt;

    {
        this.messageId = 0L;
        this.userId = 0L;
        this.groupId = 0L;
        this.sourceUrl = "";
        this.candidates = new ArrayList<>();
        this.expireAt = 0L;
    }

    public boolean expired() {
        return expireAt > 0 && System.currentTimeMillis() > expireAt;
    }

}
