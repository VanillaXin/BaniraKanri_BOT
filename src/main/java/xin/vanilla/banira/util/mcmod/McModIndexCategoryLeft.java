package xin.vanilla.banira.util.mcmod;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import xin.vanilla.banira.util.McModUtils;

import java.util.List;

/**
 * MCMod 首页分类左侧模组列表
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
public class McModIndexCategoryLeft {
    /**
     * 分类标题
     */
    private String title;

    /**
     * 分类描述
     */
    private String description;

    /**
     * 分类链接
     */
    private String categoryUrl;

    /**
     * 模组卡片列表
     */
    private List<McModIndexCategoryModFrame> modFrames;

    public String getCategoryUrl() {
        return McModUtils.fixUrl(this.categoryUrl);
    }
}
