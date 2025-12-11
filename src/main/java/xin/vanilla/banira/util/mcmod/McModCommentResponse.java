package xin.vanilla.banira.util.mcmod;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * MCMod 评论操作响应
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
public class McModCommentResponse {
    /**
     * 状态码：0 表示成功
     */
    private Integer state;

    /**
     * 是否成功
     *
     * @return true 如果 state 为 0
     */
    public boolean isSuccess() {
        return state != null && state == 0;
    }
}
