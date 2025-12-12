package xin.vanilla.banira.util.mcmod;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * MCMod 首页分类模组列表结果
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
public class McModIndexCategoryResult {
    /**
     * 左侧模组列表
     */
    private McModIndexCategoryLeft left;
    
    /**
     * 右侧排行榜
     */
    private McModIndexCategoryRight right;
}
