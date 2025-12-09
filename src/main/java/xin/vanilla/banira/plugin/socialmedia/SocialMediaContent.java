package xin.vanilla.banira.plugin.socialmedia;

import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true, fluent = true)
public class SocialMediaContent {
    /**
     * 链接
     */
    private String url;
    /**
     * 作者ID
     */
    private String authorId;
    /**
     * 作者名称
     */
    private String authorName;
    /**
     * 作者头像
     */
    private String authorAvatar;
    /**
     * 粉丝数
     */
    private int fans;
    /**
     * 发布时间
     */
    private long time;
    /**
     * 点赞数
     */
    private int like;
    /**
     * 投币数
     */
    private int coin;
    /**
     * 收藏数
     */
    private int collect;
    /**
     * 分享数
     */
    private int share;
    /**
     * 播放数
     */
    private int play;
    /**
     * 正在看
     */
    private int playing;
    /**
     * 弹幕数
     */
    private int danmaku;
    /**
     * 封面
     */
    private String cover;
    /**
     * 标题
     */
    private String title;
    /**
     * 简介
     */
    private String desc;
    /**
     * 音频
     */
    private String audio;
    /**
     * 视频
     */
    private String video;
    /**
     * 总结
     */
    private String summary;


    private String msg;
}
