package xin.vanilla.banira.util.mcmod;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * MCMod 用户卡片经验信息
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
public class McModUserCardExp {
    /**
     * 总经验值（字符串，可能包含逗号）
     */
    private String total;
    /**
     * 当前经验值
     */
    private String yet;
    /**
     * 经验率
     */
    private Integer rate;
}
