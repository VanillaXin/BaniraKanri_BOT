package xin.vanilla.banira.util.mcmod;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * MCMod 评论时间信息
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
public class McModCommentTime {
    /**
     * 时间源（完整时间）
     */
    private String source;
    /**
     * 时间范围（相对时间，如 "6时前"）
     */
    private String range;
}
