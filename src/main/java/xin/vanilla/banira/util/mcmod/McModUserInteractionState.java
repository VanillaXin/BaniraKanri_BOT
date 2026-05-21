package xin.vanilla.banira.util.mcmod;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * MCMod 当前登录用户对详情页内容的互动状态
 */
@Data
@Accessors(chain = true, fluent = true)
public class McModUserInteractionState {
    /**
     * 页面包含登录用户互动区（common-fuc-group）
     */
    private boolean loggedIn;
    /**
     * 已推荐
     */
    private boolean pushed;
    /**
     * 推荐冷却中
     */
    private boolean pushCooldown;
    /**
     * 已收藏
     */
    private boolean favorited;
    /**
     * 已关注（模组）
     */
    private boolean subscribed;
    /**
     * 已投红票
     */
    private boolean redVoted;
    /**
     * 已投黑票
     */
    private boolean blackVoted;
}
