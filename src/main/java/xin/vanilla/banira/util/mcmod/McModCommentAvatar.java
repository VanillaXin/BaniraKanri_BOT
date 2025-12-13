package xin.vanilla.banira.util.mcmod;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import xin.vanilla.banira.util.McModUtils;

/**
 * MCMod 评论用户头像
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
public class McModCommentAvatar {
    /**
     * 头像图片URL
     */
    private String img;

    public McModCommentAvatar setImg(String img) {
        this.img = McModUtils.fixUrl(img);
        return this;
    }
}
