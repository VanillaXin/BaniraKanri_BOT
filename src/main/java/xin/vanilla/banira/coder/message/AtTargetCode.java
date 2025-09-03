package xin.vanilla.banira.coder.message;

import com.mikuac.shiro.common.utils.MsgUtils;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.coder.common.BaniraCode;
import xin.vanilla.banira.coder.common.MessageCoder;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.enums.EnumCodeType;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;

import java.util.List;
import java.util.Set;

/**
 * 艾特目标
 */
@Component
public class AtTargetCode implements MessageCoder {

    @Override
    public List<String> getExample() {
        return List.of(
                CODE_START + CollectionUtils.getRandomElement(types) + CODE_END
        );
    }

    @Override
    public String getName() {
        return "艾特目标";
    }

    @Override
    public String getDesc() {
        return "艾特事件作用目标";
    }

    @Override
    public EnumCodeType getType() {
        return EnumCodeType.MSG;
    }

    private static final Set<String> types = BaniraUtils.mutableSetOf(
            "attarget", "att", "@target", "@t"
    );

    @Override
    public boolean match(String msg) {
        return types.contains(msg);
    }

    @Override
    public BaniraCodeContext execute(BaniraCodeContext context, BaniraCode code, String placeholder) {
        if (notMatch(code)) return context;
        String atMsg = context.target() != null && context.target() > 0
                ? MsgUtils.builder().at(context.target()).build()
                : "";
        return context.msg(context.msg().replace(placeholder, atMsg));
    }

}
