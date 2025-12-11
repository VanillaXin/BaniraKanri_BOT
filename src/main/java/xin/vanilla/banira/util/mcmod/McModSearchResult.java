package xin.vanilla.banira.util.mcmod;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import xin.vanilla.banira.util.StringUtils;

/**
 * MCMod 搜索结果
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
public class McModSearchResult {
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

    public String toFormatString() {
        String format = "";
        if (StringUtils.isNotNullOrEmpty(this.shortName)) {
            format += "[" + this.shortName + "] ";
        }
        if (StringUtils.isNotNullOrEmpty(this.mainName)) {
            format += this.mainName + " ";
        }
        if (StringUtils.isNotNullOrEmpty(this.secondaryName)) {
            format += "(" + this.secondaryName + ")";
        }
        return format.trim();
    }
}
