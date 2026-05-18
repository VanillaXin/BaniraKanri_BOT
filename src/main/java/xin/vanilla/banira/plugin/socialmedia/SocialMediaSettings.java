package xin.vanilla.banira.plugin.socialmedia;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.List;

/**
 * 社交媒体插件配置
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class SocialMediaSettings {

    /**
     * 外部自定义接口列表
     */
    private List<SocialMediaApiSettings> apis;

    {
        this.apis = BaniraUtils.mutableListOf(
                new SocialMediaApiSettings()
                        .parser("bilibili")
                        .api("https://api.mir6.com/api/bzjiexi?type=json&url=%s")
                        .fields(new SocialMediaApiFieldSettings()
                                .successCodePath("code")
                                .successCode(200)
                                .titlePath("title")
                                .coverPath("imgurl")
                                .descPath("desc")
                                .authorNamePath("user.name")
                                .authorAvatarPath("user.user_img")
                                .videoPath("data.[0].video_url")
                                .audioPath("")
                                .likePath("")
                                .summaryPath("")
                        ),
                new SocialMediaApiSettings()
                        .parser("douyin")
                        .api("https://www.douyin.com/aweme/v1/web/aweme/detail/?aweme_id=%s")
                        .targetMode("awemeId")
                        .headers(BaniraUtils.mutableMapOf(
                                "accept", "application/json, text/plain, */*",
                                "origin", "https://open.douyin.com",
                                "referer", "https://www.douyin.com/",
                                "user-agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
                        ))
                        .fields(new SocialMediaApiFieldSettings()
                                .successCodePath("status_code")
                                .successCode(0)
                                .titlePath("aweme_detail.desc")
                                .coverPath("")
                                .coverPathFallback(BaniraUtils.mutableListOf(
                                        "aweme_detail.video.dynamic_cover.url_list.[0]",
                                        "aweme_detail.video.cover.url_list.[0]"
                                ))
                                .descPath("")
                                .authorNamePath("aweme_detail.author.nickname")
                                .authorAvatarPath("aweme_detail.author.avatar_thumb.url_list.[0]")
                                .videoPath("")
                                .videoPathFallback(BaniraUtils.mutableListOf(
                                        "aweme_detail.video.download_addr.url_list.[0]",
                                        "aweme_detail.video.play_addr.url_list.[0]"
                                ))
                                .audioPath("")
                                .likePath("aweme_detail.statistics.digg_count")
                                .summaryPath("")
                        ),
                new SocialMediaApiSettings()
                        .parser("douyin")
                        .api("https://apis.jxcxin.cn/api/douyin?url=%s")
                        .fields(new SocialMediaApiFieldSettings()
                                .successCodePath("code")
                                .successCode(200)
                                .titlePath("data.title")
                                .coverPath("data.cover")
                                .descPath("")
                                .authorNamePath("data.author")
                                .authorAvatarPath("data.avatar")
                                .videoPath("data.url")
                                .audioPath("")
                                .likePath("data.like")
                                .summaryPath("")
                        )
        );
    }
}
