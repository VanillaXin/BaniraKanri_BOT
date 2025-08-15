package xin.vanilla.banira.coder;

import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.enums.EnumCodeType;

public interface BaniraCoder {

    String MSG_CODE_START = "[bkode:";
    String MSG_CODE_END = "]";
    String EVENT_CODE_START = "<bkode:";
    String EVENT_CODE_END = ">";
    String ARG_SEPARATOR = ":";

    /**
     * 是否群管指令
     */
    default boolean isKanri() {
        return false;
    }

    /**
     * 优先级
     */
    default int getPriority() {
        return Integer.MAX_VALUE;
    }

    EnumCodeType getType();

    boolean match(String msg);

    BaniraCodeContext execute(BaniraCodeContext context);

}
