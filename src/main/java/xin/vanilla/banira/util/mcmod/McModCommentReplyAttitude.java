package xin.vanilla.banira.util.mcmod;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * MCMod 评论回复态度/表情信息
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
public class McModCommentReplyAttitude {
    /**
     * 点赞
     */
    private Integer up;
    /**
     * 笑哭
     */
    private Integer grintears;
    /**
     * 爱心
     */
    private Integer heart;
    /**
     * 脸红
     */
    private Integer flushed;
    /**
     * 点踩
     */
    private Integer down;
    /**
     * 柠檬
     */
    private Integer lemon;
    /**
     * 马头
     */
    private Integer horsehead;
    /**
     * 心碎
     */
    private Integer heartbroken;
    /**
     * 愤怒
     */
    private Integer angry;
    /**
     * 疲惫
     */
    private Integer tired;
    /**
     * 雪花
     */
    private Integer snowflake;
    /**
     * 握手
     */
    private Integer handshake;
    /**
     * 使用（可能为null）
     */
    private Object use;
}
