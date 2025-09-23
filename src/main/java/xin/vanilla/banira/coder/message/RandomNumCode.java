package xin.vanilla.banira.coder.message;

import com.google.gson.JsonObject;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.coder.common.BaniraCode;
import xin.vanilla.banira.coder.common.MessageCoder;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.enums.EnumCodeType;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.List;
import java.util.Set;

/**
 * 随机数
 */
@Component
public class RandomNumCode implements MessageCoder {

    @Override
    public List<String> getExample() {
        return List.of(
                CODE_START + CollectionUtils.getRandomElement(types) + CODE_END
        );
    }

    @Override
    public String getName() {
        return "随机数";
    }

    @Override
    public String getDesc() {
        return "生成指定范围的随机数";
    }

    @Override
    public boolean isKanri() {
        return true;
    }

    @Override
    public EnumCodeType getType() {
        return EnumCodeType.MSG;
    }

    private static final Set<String> types = BaniraUtils.mutableSetOf(
            "randomnum", "randnum", "rn"
    );

    @Override
    public boolean match(String msg) {
        return types.contains(msg);
    }

    @Override
    public String execute(BaniraCodeContext context, BaniraCode code, String placeholder) {
        if (notMatch(code)) return "";
        JsonObject data = code.getData();
        if (data == null) return fail(context, code, placeholder);

        String range = getValue(context, code);
        if (StringUtils.isNullOrEmpty(range)) return fail(context, code, placeholder);
        String[] split = range.replace("_", "~")
                .replace("+", "~")
                .replace("#", "~")
                .split("~");
        if (split.length > 2 || split.length == 0) return fail(context, code, placeholder);
        String randVal;
        if (split.length == 1) {
            if (split[0].contains(".")) {
                randVal = String.valueOf(RANDOM.nextDouble(StringUtils.toDouble(split[0])));
            } else {
                randVal = String.valueOf(RANDOM.nextInt(StringUtils.toInt(split[0])));
            }
        } else {
            if (split[0].contains(".") || split[1].contains(".")) {
                randVal = String.valueOf(RANDOM.nextDouble(StringUtils.toDouble(split[0]), StringUtils.toDouble(split[1])));
            } else {
                randVal = String.valueOf(RANDOM.nextInt(StringUtils.toInt(split[0]), StringUtils.toInt(split[1])));
            }
        }
        context.msg(context.msg().replace(placeholder, replaceResult(code, randVal)));
        return randVal;
    }

}
