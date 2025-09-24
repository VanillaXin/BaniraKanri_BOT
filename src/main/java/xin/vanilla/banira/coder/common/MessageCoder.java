package xin.vanilla.banira.coder.common;

import com.google.gson.JsonObject;
import com.mikuac.shiro.common.utils.ShiroUtils;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.enums.EnumCodeType;
import xin.vanilla.banira.util.JsonUtils;
import xin.vanilla.banira.util.RegexpHelper;
import xin.vanilla.banira.util.StringUtils;

import java.util.List;
import java.util.Random;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public interface MessageCoder {

    String CODE_START = "[bkode:";
    String CODE_END = "]";
    Character ARG_SEPARATOR = ',';
    Character VAL_SEPARATOR = ':';
    Random RANDOM = new Random();
    Pattern READ_VAL_PATTERN = new RegexpHelper().groupIgByName("g", "\\$\\w+").compile();

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
        return Integer.MAX_VALUE / 2;
    }

    /**
     * 是否需要转义CQ码
     */
    default boolean needEscape() {
        return false;
    }

    EnumCodeType getType();

    boolean match(String msg);

    String execute(BaniraCodeContext context, BaniraCode code, String placeholder);


    default boolean notMatch(BaniraCode code) {
        return !match(code.getType());
    }

    default String fail(BaniraCodeContext context, BaniraCode code, String placeholder) {
        context.msg(context.msg().replace(placeholder, code.getRaw()));
        return code.getRaw();
    }

    /**
     * 获取参数并拼接运行时参数
     */
    default String getValue(BaniraCodeContext context, BaniraCode code, String... keys) {
        JsonObject data = code.getData();
        String result = "";
        if (data != null) {
            result = JsonUtils.getString(data, "value", "");
            if (StringUtils.isNullOrEmpty(result)) {
                for (String key : keys) {
                    result = JsonUtils.getString(data, key, "");
                    if (StringUtils.isNotNullOrEmpty(result)) break;
                }
            }
            String $r = JsonUtils.getString(data, "$r", "");
            if (StringUtils.isNotNullOrEmpty(result) && "$auto".equals($r)) {
                Matcher matcher = READ_VAL_PATTERN.matcher(result);
                while (matcher.find()) {
                    String key = matcher.group("g");
                    if (context.values().containsKey(key)) {
                        result = result.replace(key, context.values().get(key));
                    } else if (context.values().containsKey(key.substring(1))) {
                        result = result.replace(key, context.values().get(key.substring(1)));
                    }
                }
            } else if (context.values().containsKey($r)) {
                if (StringUtils.isNullOrEmpty(result)) result = "$r";
                result = result.replace(result, context.values().get($r));
            }

        }
        return result;
    }

    /**
     * 设置运行时参数集
     */
    default void writeValue(BaniraCodeContext context, BaniraCode code, String value) {
        JsonObject data = code.getData();
        if (data != null && StringUtils.isNotNullOrEmpty(value)) {
            String $w = JsonUtils.getString(data, "$w", "");
            if (StringUtils.isNotNullOrEmpty($w)) {
                context.values().put($w, value);
            }
        }
    }

    /**
     * 获取参数
     */
    default String getArg(BaniraCode code, String... keys) {
        JsonObject data = code.getData();
        String result = "";
        if (data != null) {
            for (String key : keys) {
                result = JsonUtils.getString(data, key, "");
                if (StringUtils.isNotNullOrEmpty(result)) break;
            }
        }
        return result;
    }

    /**
     * 是否为空<br>
     * 仅用于判断存在$w时是否将值从消息中删除
     */
    default boolean hasVoid(BaniraCode code) {
        return code.getData() != null && "true".equals(JsonUtils.getString(code.getData(), "$void", "false"));
    }

    /**
     * 转义CQ码防止注入
     */
    default String replaceResult(BaniraCode code, String result) {
        return hasVoid(code) ? "" : needEscape() ? ShiroUtils.escape2(result) : result;
    }

    default boolean hasWrite(BaniraCode code) {
        JsonObject data = code.getData();
        return data != null && StringUtils.isNotNullOrEmpty(JsonUtils.getString(data, "$w", null));
    }

}
