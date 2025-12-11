package xin.vanilla.banira.util.mcmod;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * MCMod 用户卡片结果
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
public class McModUserCardResult {
    /**
     * 用户名
     */
    private String username;
    /**
     * 头像URL
     */
    private String avatar;
    /**
     * 在线状态：-1=隐身，0=离线，1=在线
     */
    private Integer online;
    /**
     * 签名
     */
    private String sign;
    /**
     * 等级
     */
    private Integer rank;
    /**
     * 经验信息
     */
    private McModUserCardExp exp;
    /**
     * 追踪器列表
     */
    private List<McModUserCardTracker> tracker;
    /**
     * 徽章信息（key为徽章ID，value为徽章详情）
     */
    private List<McModUserCardBadge> badge;
}
