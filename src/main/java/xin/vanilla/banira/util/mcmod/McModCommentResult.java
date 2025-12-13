package xin.vanilla.banira.util.mcmod;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * MCMod 评论结果
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
public class McModCommentResult {
    /**
     * 是否登录
     */
    @JsonProperty("is_login")
    private boolean isLogin;
    /**
     * 分页信息
     */
    private McModCommentPage page;
    /**
     * 评论列表
     */
    private List<McModCommentRow> row;

    public McModCommentResult setReplyId(String replyId) {
        for (McModCommentRow row : this.row) {
            row.setParentId(replyId);
        }
        return this;
    }

    public McModCommentResult setCommentType(EnumContentType commentType) {
        for (McModCommentRow row : this.row) {
            row.setCommentType(commentType);
        }
        return this;
    }

    public McModCommentResult setContainerId(String containerId) {
        for (McModCommentRow row : this.row) {
            row.setContainerId(containerId);
        }
        return this;
    }

    public McModCommentResult set(EnumContentType commentType, String containerId, String replyId) {
        for (McModCommentRow row : this.row) {
            row.setCommentType(commentType)
                    .setContainerId(containerId)
                    .setParentId(replyId);
        }
        return this;
    }
}
