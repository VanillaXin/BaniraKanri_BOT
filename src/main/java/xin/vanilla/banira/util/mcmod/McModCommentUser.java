package xin.vanilla.banira.util.mcmod;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * MCMod 评论用户信息
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
public class McModCommentUser {
    /**
     * 用户ID
     */
    private String id;
    /**
     * 用户名
     */
    private String name;
    /**
     * 头像信息
     */
    private McModCommentAvatar avatar;
    /**
     * sp值
     */
    private Integer sp;
    /**
     * 等级
     */
    private Integer lv;
}
