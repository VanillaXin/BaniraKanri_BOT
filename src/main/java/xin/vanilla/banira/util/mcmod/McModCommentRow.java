package xin.vanilla.banira.util.mcmod;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * MCMod 评论行
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
public class McModCommentRow {
    /**
     * 用户信息
     */
    private McModCommentUser user;
    /**
     * 楼层
     */
    private String floor;
    /**
     * 评论ID
     */
    private String id;
    /**
     * 时间信息
     */
    private McModCommentTime time;
    /**
     * 态度/表情信息
     */
    private McModCommentAttitude attitude;
    /**
     * 回复数量
     */
    @JsonProperty("reply_count")
    private String replyCount;
    /**
     * 评论内容（HTML格式）
     */
    private String content;
}
