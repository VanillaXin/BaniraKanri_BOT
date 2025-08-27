package xin.vanilla.banira.coder.common;

import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.enums.EnumCodeType;

import java.util.List;

public interface BaniraCoder {

    String CODE_START = "[bkode:";
    String CODE_END = "]";
    Character ARG_SEPARATOR = ',';
    Character VAL_SEPARATOR = ':';

    List<String> getExample();

    String getName();

    String getDesc();

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

    default boolean notMatch(BaniraCode code) {
        return !match(code.getType());
    }

    default BaniraCodeContext fail(BaniraCodeContext context, BaniraCode code, String placeholder) {
        return context.msg(context.msg().replace(placeholder, code.getRaw()));
    }


    EnumCodeType getType();

    boolean match(String msg);

    BaniraCodeContext execute(BaniraCodeContext context, BaniraCode code, String placeholder);

}
