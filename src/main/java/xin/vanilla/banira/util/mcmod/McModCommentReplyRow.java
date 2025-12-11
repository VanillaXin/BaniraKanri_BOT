package xin.vanilla.banira.util.mcmod;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * MCMod 评论回复行
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
public class McModCommentReplyRow {
    /**
     * 用户信息
     */
    private McModCommentUser user;
    /**
     * 回复的用户信息（当回复的是另一个用户时存在）
     */
    @JsonProperty("reply_user")
    private McModCommentReplyUser replyUser;
    /**
     * 回复ID
     */
    private String id;
    /**
     * 时间信息
     */
    private McModCommentTime time;
    /**
     * 态度/表情信息
     */
    private McModCommentReplyAttitude attitude;
    /**
     * 回复内容
     */
    private String content;
}
