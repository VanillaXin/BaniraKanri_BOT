package xin.vanilla.banira.plugin.mcmodcomment;

import lombok.Getter;
import lombok.Setter;

/**
 * 评论信息
 */
@Getter
@Setter
public class CommentInfo {
    /**
     * 评论ID
     */
    private String id;

    /**
     * 楼层
     */
    private String floor;

    /**
     * 内容
     */
    private String content;

    /**
     * 用户ID
     */
    private String userId;

    /**
     * 用户名
     */
    private String userName;

    /**
     * 时间
     */
    private String time;
}

