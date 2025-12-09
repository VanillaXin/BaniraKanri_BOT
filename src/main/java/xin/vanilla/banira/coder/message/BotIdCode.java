package xin.vanilla.banira.coder.message;

import com.mikuac.shiro.enums.MsgTypeEnum;
import com.mikuac.shiro.model.ArrayMsg;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.coder.common.BaniraCode;
import xin.vanilla.banira.coder.common.MessageCoder;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.enums.EnumCodeType;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * 姬气人ID
 */
@Component
public class BotIdCode implements MessageCoder {

    public static final String DEFAULT_CODE = CODE_START + "bid" + CODE_END;

    @Override
    public List<String> getExample() {
        return List.of(
                CODE_START + CollectionUtils.getRandomElement(types) + CODE_END
        );
    }

    @Override
    public String getName() {
        return "姬气人ID";
    }

    @Override
    public String getDesc() {
        return "获取姬气人自己的QQ";
    }

    @Override
    public EnumCodeType getType() {
        return EnumCodeType.MSG;
    }

    private static final Set<String> types = BaniraUtils.mutableSetOf(
            "botid", "bid"
    );

    @Override
    public boolean match(String msg) {
        return types.contains(msg);
    }

    @Override
    public String execute(BaniraCodeContext context, BaniraCode code, String placeholder) {
        if (notMatch(code)) return "";
        String id = String.valueOf(context.bot().getSelfId());
        context.msg(context.msg().replace(placeholder, replaceResult(code, id)));
        return id;
    }

    @Override
    public boolean matchEncode(ArrayMsg msg) {
        return msg.getType() == MsgTypeEnum.text;
    }

    @Override
    public ArrayMsg executeEncode(BaniraCodeContext context, ArrayMsg msg) {
        String stringData = msg.toCQCode();
        String regex = "(?<!\\d)(" + context.bot().getSelfId() + ")(?!\\d)";
        if (!stringData.matches(regex)) return msg;
        stringData = stringData.replaceAll(regex, DEFAULT_CODE);
        ArrayMsg arrayMsg = new ArrayMsg();
        arrayMsg.setType(MsgTypeEnum.text);
        arrayMsg.setData(Map.of(MsgTypeEnum.text.name(), stringData));
        context.msg(context.msg().replace(msg.toCQCode(), stringData));
        return arrayMsg;
    }

}
