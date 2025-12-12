package xin.vanilla.banira.util.mcmod;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * MCMod 评论分页信息
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
public class McModCommentPage {
    /**
     * 第一页
     */
    private Integer home;
    /**
     * 上一页
     */
    private Integer prev;
    /**
     * 下一页
     */
    private Integer next;
    /**
     * 最后一页
     */
    private Integer end;
    /**
     * 页码列表
     */
    private List<Integer> list;
    /**
     * 名称
     */
    private String name;
    /**
     * 当前页
     */
    @JsonProperty("now_page")
    private Integer nowPage;
    /**
     * 总页数
     */
    @JsonProperty("total_page")
    private Integer totalPage;
    /**
     * 计数语言模板
     */
    @JsonProperty("count_lang")
    private String countLang;
    /**
     * 总行数（字符串）
     */
    @JsonProperty("total_row")
    private String totalRow;
}
