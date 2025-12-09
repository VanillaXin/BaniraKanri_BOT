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
 * 发送者ID
 */
@Component
public class SenderIdCode implements MessageCoder {

    public static final String DEFAULT_CODE = CODE_START + "sid" + CODE_END;

    @Override
    public List<String> getExample() {
        return List.of(
                CODE_START + CollectionUtils.getRandomElement(types) + CODE_END
        );
    }

    @Override
    public String getName() {
        return "发送者ID";
    }

    @Override
    public String getDesc() {
        return "获取消息发送者的QQ";
    }

    @Override
    public EnumCodeType getType() {
        return EnumCodeType.MSG;
    }

    private static final Set<String> types = BaniraUtils.mutableSetOf(
            "senderid", "sid"
    );

    @Override
    public boolean match(String msg) {
        return types.contains(msg);
    }

    @Override
    public String execute(BaniraCodeContext context, BaniraCode code, String placeholder) {
        if (notMatch(code)) return "";
        if (!BaniraUtils.isUserIdValid(context.sender())) return fail(context, code, placeholder);
        String id = String.valueOf(context.sender());
        context.msg(context.msg().replace(placeholder, replaceResult(code, id)));
        return id;
    }

    @Override
    public boolean matchEncode(ArrayMsg msg) {
        return msg.getType() == MsgTypeEnum.text;
    }

    @Override
    public ArrayMsg executeEncode(BaniraCodeContext context, ArrayMsg msg) {
        if (context.sender() == null) return msg;
        String stringData = msg.toCQCode();
        String regex = "(?<!\\d)(" + context.sender() + ")(?!\\d)";
        if (!stringData.matches(regex)) return msg;
        stringData = stringData.replaceAll(regex, DEFAULT_CODE);
        ArrayMsg arrayMsg = new ArrayMsg();
        arrayMsg.setType(MsgTypeEnum.text);
        arrayMsg.setData(Map.of(MsgTypeEnum.text.name(), stringData));
        context.msg(context.msg().replace(msg.toCQCode(), stringData));
        return arrayMsg;
    }

}
