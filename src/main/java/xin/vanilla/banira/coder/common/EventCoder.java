package xin.vanilla.banira.coder.common;

import com.google.gson.JsonObject;

import java.util.List;

public interface EventCoder {
    String CODE_START = "(bkode:";
    String CODE_END = ")";
    Character VAL_SEPARATOR = ':';

    List<String> getExample();

    String getName();

    String getDesc();

    String build(JsonObject data);

    default String escape(String msg) {
        String start = CODE_START.replace(VAL_SEPARATOR.toString(), "\\" + VAL_SEPARATOR);
        return msg.replace(CODE_END, start);
    }

    default String unescape(String msg) {
        String start = CODE_START.replace(VAL_SEPARATOR.toString(), "\\" + VAL_SEPARATOR);
        return msg.replace(start, CODE_START);
    }
}
