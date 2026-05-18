package xin.vanilla.banira.plugin.socialmedia;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

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
    }
}
