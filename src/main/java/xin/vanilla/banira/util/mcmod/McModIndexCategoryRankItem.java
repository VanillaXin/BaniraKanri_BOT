package xin.vanilla.banira.util.mcmod;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * MCMod 首页分类排行榜项
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
public class McModIndexCategoryRankItem {
    /**
     * 排名
     */
    private int rank;

    /**
     * mod ID
     */
    private long modId;
    /**
     * mod 简称
     */
    private String shortName;
    /**
     * mod 主要名称
     */
    private String mainName;
    /**
     * mod 次要名称
     */
    private String secondaryName;

    /**
     * 模组封面图片URL
     */
    private String coverImageUrl;

    /**
     * 指数值
     */
    private String indexValue;
}
