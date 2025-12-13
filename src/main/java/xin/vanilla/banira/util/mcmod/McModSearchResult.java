package xin.vanilla.banira.util.mcmod;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * MCMod 搜索结果项
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
public class McModSearchResult {
    /**
     * 副标题
     */
    private String subtitle;
    /**
     * 标题
     */
    private String title;
    /**
     * 链接
     */
    private String link;
    /**
     * 摘要
     */
    private String summary;
    /**
     * 快照时间
     */
    private Date snapshotTime;
}
