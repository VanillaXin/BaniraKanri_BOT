package xin.vanilla.banira.util.mcmod;

import lombok.Getter;
import lombok.experimental.Accessors;

/**
 * MCMod 投票状态
 */
@Getter
@Accessors(fluent = true)
public enum EnumVoteState {
    /**
     * 已投该类型票
     */
    VOTED,
    /**
     * 未投该类型票
     */
    NOT_VOTED,
}
