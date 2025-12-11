package xin.vanilla.banira.util.mcmod;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * MCMod 用户卡片徽章信息
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
public class McModUserCardBadge {
    /**
     * 徽章标题
     */
    private String title;
    /**
     * 徽章内容（HTML格式）
     */
    private String content;
    /**
     * 徽章来源说明
     */
    private String source;
    /**
     * 徽章图标文件名
     */
    private String icon;
    /**
     * 获得时间
     */
    private String time;
}
