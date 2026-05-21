package xin.vanilla.banira.util.mcmod;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * MCMod 投票确保结果（通过探测式调用得出）
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
public class McModCardVoteEnsureResult {
    /**
     * 探测前的投票状态
     */
    private EnumVoteState stateBefore;
    /**
     * 操作完成后的投票状态
     */
    private EnumVoteState stateAfter;
    /**
     * 最终响应
     */
    private McModCardVoteResponse response;
    /**
     * 实际发起的 API 调用次数
     */
    private int apiCalls;
    /**
     * 是否命中内存缓存（未发起 API）
     */
    private boolean fromCache;
    /**
     * 是否因冷却中而拒绝请求
     */
    private boolean cooldownBlocked;

    public boolean isSuccess() {
        if (cooldownBlocked) {
            return false;
        }
        if (fromCache) {
            return true;
        }
        return response != null && response.isSuccess();
    }

    /**
     * 本次是否新投上票（之前未投，最终为已投）
     */
    public boolean isNewlyVoted() {
        return stateBefore == EnumVoteState.NOT_VOTED && stateAfter == EnumVoteState.VOTED;
    }

    /**
     * 本次是否取消了投票（之前已投，最终为未投）
     */
    public boolean isNewlyUnvoted() {
        return stateBefore == EnumVoteState.VOTED && stateAfter == EnumVoteState.NOT_VOTED;
    }

    /**
     * 之前已处于目标状态，探测后恢复，无实质变化
     */
    public boolean isUnchanged() {
        return stateBefore == stateAfter;
    }
}
