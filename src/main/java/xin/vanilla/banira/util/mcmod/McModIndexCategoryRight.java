package xin.vanilla.banira.util.mcmod;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * MCMod 首页分类右侧排行榜
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
public class McModIndexCategoryRight {
    /**
     * 今日排行榜
     */
    private List<McModIndexCategoryRankItem> dayRank;
    
    /**
     * 本周排行榜
     */
    private List<McModIndexCategoryRankItem> weekRank;
    
    /**
     * 本月排行榜
     */
    private List<McModIndexCategoryRankItem> monthRank;
}
