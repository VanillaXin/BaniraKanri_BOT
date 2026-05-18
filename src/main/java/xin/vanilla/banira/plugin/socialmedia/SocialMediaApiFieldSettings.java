package xin.vanilla.banira.plugin.socialmedia;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.List;

/**
 * 社交媒体接口字段映射配置
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class SocialMediaApiFieldSettings {

    private String successCodePath;
    private Integer successCode;
    private String titlePath;
    private String coverPath;
    private String descPath;
    private String authorNamePath;
    private String authorAvatarPath;
    private String videoPath;
    private String audioPath;
    private String likePath;
    private String summaryPath;
    /**
     * 多路径回退（命中第一个非空字段即生效）
     */
    private List<String> coverPathFallback;
    /**
     * 多路径回退（命中第一个非空字段即生效）
     */
    private List<String> videoPathFallback;

    {
        this.successCodePath = "code";
        this.successCode = 200;
        this.titlePath = "";
        this.coverPath = "";
        this.descPath = "";
        this.authorNamePath = "";
        this.authorAvatarPath = "";
        this.videoPath = "";
        this.audioPath = "";
        this.likePath = "";
        this.summaryPath = "";
        this.coverPathFallback = BaniraUtils.mutableListOf();
        this.videoPathFallback = BaniraUtils.mutableListOf();
    }
}
