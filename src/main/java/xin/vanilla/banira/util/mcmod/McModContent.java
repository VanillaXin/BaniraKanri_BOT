package xin.vanilla.banira.util.mcmod;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import xin.vanilla.banira.util.McModUtils;
import xin.vanilla.banira.util.StringUtils;

/**
 * MCMod 搜索结果
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
public class McModContent {
    /**
     * 类型
     */
    private EnumContentType type;
    /**
     * ID
     */
    private long id;
    /**
     * 简称
     */
    private String shortName;
    /**
     * 主要名称
     */
    private String mainName;
    /**
     * 次要名称
     */
    private String secondaryName;
    /**
     * 封面图片URL
     */
    private String coverImageUrl;
    /**
     * 详情页URL
     */
    private String detailUrl;

    public McModContent(EnumContentType type, long id, String shortName, String mainName, String secondaryName) {
        this.type = type;
        this.id = id;
        this.shortName = shortName;
        this.mainName = mainName;
        this.secondaryName = secondaryName;
    }

    public String getDetailUrl() {
        return StringUtils.isNotNullOrEmpty(this.detailUrl) ? this.detailUrl : McModUtils.getUrl(this.type, String.valueOf(this.id));
    }

    public String getFormattedName() {
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
