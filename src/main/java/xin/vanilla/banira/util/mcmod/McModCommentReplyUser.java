package xin.vanilla.banira.util.mcmod;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * MCMod 评论回复用户信息（简化版，只包含ID和名称）
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
public class McModCommentReplyUser {
    /**
     * 用户ID
     */
    private String id;
    /**
     * 用户名
     */
    private String name;
}
