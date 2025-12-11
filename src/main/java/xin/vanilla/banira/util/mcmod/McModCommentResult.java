package xin.vanilla.banira.util.mcmod;

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
     * 分页信息
     */
    private McModCommentPage page;
    /**
     * 评论列表
     */
    private List<McModCommentRow> row;
}
