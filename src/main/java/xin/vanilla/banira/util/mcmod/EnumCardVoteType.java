package xin.vanilla.banira.util.mcmod;

import lombok.Getter;
import lombok.experimental.Accessors;

/**
 * MCMod 卡片投票类型
 */
@Getter
@Accessors(fluent = true)
public enum EnumCardVoteType {
    /**
     * 红票（推荐）
     */
    RED("1"),
    /**
     * 黑票
     */
    BLACK("0"),
    ;

    private final String value;

    EnumCardVoteType(String value) {
        this.value = value;
    }
}
