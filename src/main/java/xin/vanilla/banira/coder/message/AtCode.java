package xin.vanilla.banira.coder.message;

import com.mikuac.shiro.common.utils.MsgUtils;
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
 * 艾特某人
 */
@Component
public class AtCode implements MessageCoder {

    @Override
    public List<String> getExample() {
        return List.of(
                CODE_START + CollectionUtils.getRandomElement(types) + VAL_SEPARATOR + "123456789" + CODE_END
        );
    }

    @Override
    public String getName() {
        return "艾特某人";
    }

    @Override
    public String getDesc() {
        return "艾特某人";
    }

    @Override
    public EnumCodeType getType() {
        return EnumCodeType.MSG;
    }

    private static final Set<String> types = BaniraUtils.mutableSetOf(
            "at", "@"
    );

    @Override
    public boolean match(String msg) {
        return types.contains(msg);
    }

    @Override
    public String execute(BaniraCodeContext context, BaniraCode code, String placeholder) {
        if (notMatch(code)) return "";
        String user = getValue(context, code);
        if (StringUtils.isNullOrEmptyEx(user)) return fail(context, code, placeholder);
        long userId = StringUtils.toLong(user);
        if (!BaniraUtils.isUserIdValid(userId)) return fail(context, code, placeholder);
        String atMsg = MsgUtils.builder().at(userId).build();
        context.msg(context.msg().replace(placeholder, replaceResult(code, atMsg)));
        return atMsg;
    }

}
