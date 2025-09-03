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
 * 艾特自己
 */
@Component
public class AtBotCode implements MessageCoder {

    @Override
    public List<String> getExample() {
        return List.of(
                CODE_START + CollectionUtils.getRandomElement(types) + CODE_END
        );
    }

    @Override
    public String getName() {
        return "艾特自己";
    }

    @Override
    public String getDesc() {
        return "艾特姬气人自己";
    }

    @Override
    public EnumCodeType getType() {
        return EnumCodeType.MSG;
    }

    private static final Set<String> types = BaniraUtils.mutableSetOf(
            "atbot", "atb", "@bot", "@b"
    );

    @Override
    public boolean match(String msg) {
        return types.contains(msg);
    }

    @Override
    public BaniraCodeContext execute(BaniraCodeContext context, BaniraCode code, String placeholder) {
        if (notMatch(code)) return context;
        String atMsg = context.bot() != null && context.bot().getSelfId() > 0
                ? MsgUtils.builder().at(context.bot().getSelfId()).build()
                : "";
        return context.msg(context.msg().replace(placeholder, atMsg));
    }

}
