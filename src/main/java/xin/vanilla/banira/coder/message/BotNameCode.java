package xin.vanilla.banira.coder.message;

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
 * 自己昵称
 */
@Component
public class BotNameCode implements MessageCoder {

    @Override
    public List<String> getExample() {
        return List.of(
                CODE_START + CollectionUtils.getRandomElement(types) + CODE_END
        );
    }

    @Override
    public String getName() {
        return "自己昵称";
    }

    @Override
    public String getDesc() {
        return "获取姬气人自己的昵称";
    }

    @Override
    public EnumCodeType getType() {
        return EnumCodeType.MSG;
    }

    private static final Set<String> types = BaniraUtils.mutableSetOf(
            "botname", "bname"
    );

    @Override
    public boolean match(String msg) {
        return types.contains(msg);
    }

    @Override
    public BaniraCodeContext execute(BaniraCodeContext context, BaniraCode code, String placeholder) {
        if (notMatch(code)) return context;
        String name = context.bot().getUserNameEx(context.group(), context.bot().getSelfId());
        return context.msg(context.msg().replace(placeholder, name));
    }

}
