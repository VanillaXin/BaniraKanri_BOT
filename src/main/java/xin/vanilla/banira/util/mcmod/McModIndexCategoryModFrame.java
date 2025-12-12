package xin.vanilla.banira.util.mcmod;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * MCMod 首页分类模组卡片
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
public class McModIndexCategoryModFrame {
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
     * 浏览数
     */
    private String viewCount;

    /**
     * 推荐数
     */
    private String recommendCount;

    /**
     * 收藏数
     */
    private String favoriteCount;

    /**
     * 物品列表
     */
    private List<McModIndexCategoryItem> items;
}
