package xin.vanilla.banira.util.mcmod;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * MCMod 用户卡片追踪器信息
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
public class McModUserCardTracker {
    /**
     * 标题
     */
    private String title;
    /**
     * 值
     */
    private String value;
    /**
     * 排名
     */
    private String rank;
}
