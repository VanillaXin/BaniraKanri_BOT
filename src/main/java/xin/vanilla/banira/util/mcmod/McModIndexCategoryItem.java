package xin.vanilla.banira.util.mcmod;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * MCMod 首页分类物品项
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
public class McModIndexCategoryItem {
    /**
     * 物品ID
     */
    private long itemId;
    
    /**
     * 物品名称
     */
    private String itemName;
    
    /**
     * 物品图标URL
     */
    private String iconUrl;
    
    /**
     * 物品链接
     */
    private String itemUrl;
}
