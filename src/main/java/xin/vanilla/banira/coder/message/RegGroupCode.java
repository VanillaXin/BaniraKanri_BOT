package xin.vanilla.banira.coder.message;

import com.google.gson.JsonObject;
import com.mikuac.shiro.common.utils.MessageConverser;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.coder.common.BaniraCode;
import xin.vanilla.banira.coder.common.MessageCoder;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.enums.EnumCodeType;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.RegexpHelper;
import xin.vanilla.banira.util.StringUtils;

import java.util.List;
import java.util.Set;

/**
 * 正则捕获组
 */
@Component
public class RegGroupCode implements MessageCoder {

    @Override
    public List<String> getExample() {
        return List.of(
                CODE_START + CollectionUtils.getRandomElement(types) + VAL_SEPARATOR + "1" + CODE_END
                , CODE_START + CollectionUtils.getRandomElement(types) + VAL_SEPARATOR + "name" + CODE_END
        );
    }

    @Override
    public String getName() {
        return "正则捕获组";
    }

    @Override
    public String getDesc() {
        return "获取正则匹配关键词中指定捕获组的内容";
    }

    /**
     * 是否需要转义CQ码
     */
    @Override
    public boolean needEscape() {
        return true;
    }

    @Override
    public EnumCodeType getType() {
        return EnumCodeType.MSG;
    }

    private static final Set<String> types = BaniraUtils.mutableSetOf(
            "regexgroup", "reggroup", "regroup"
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

        String groupKey = getValue(context, code);
        if (StringUtils.isNullOrEmptyEx(groupKey)) return fail(context, code, placeholder);

        String prefix = getArg(code, "p", "prefix");
        String suffix = getArg(code, "s", "suffix");

        String match = RegexpHelper.extractParams(context.keywordRecord().getKeyword()
                , MessageConverser.arraysToString(context.originalMsg())
                , "$" + groupKey
        );
        match = prefix + match + suffix;
        if (StringUtils.isNullOrEmptyEx(match)) return fail(context, code, placeholder);
        context.msg(context.msg().replace(placeholder, replaceResult(code, match)));
        return match;
    }

}
