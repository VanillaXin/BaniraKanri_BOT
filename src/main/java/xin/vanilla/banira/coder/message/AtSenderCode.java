package xin.vanilla.banira.coder.message;

import com.mikuac.shiro.common.utils.MsgUtils;
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
 * 艾特发送者
 */
@Component
public class AtSenderCode implements MessageCoder {

    private static final String DEFAULT_CODE = CODE_START + "@s" + CODE_END;

    @Override
    public List<String> getExample() {
        return List.of(
                CODE_START + CollectionUtils.getRandomElement(types) + CODE_END
        );
    }

    @Override
    public String getName() {
        return "艾特发送者";
    }

    @Override
    public String getDesc() {
        return "艾特消息发送者";
    }

    /**
     * 优先级
     */
    @Override
    public int getPriority() {
        return MessageCoder.super.getPriority() - 1;
    }

    @Override
    public EnumCodeType getType() {
        return EnumCodeType.MSG;
    }

    private static final Set<String> types = BaniraUtils.mutableSetOf(
            "atsender", "atuser", "ats", "atu", "@user", "@sender", "@@", "@s", "@u"
    );

    @Override
    public boolean match(String msg) {
        return types.contains(msg);
    }

    @Override
    public String execute(BaniraCodeContext context, BaniraCode code, String placeholder) {
        if (notMatch(code)) return "";
        String atMsg = context.sender() != null && context.sender() > 0
                ? MsgUtils.builder().at(context.sender()).build()
                : "";
        context.msg(context.msg().replace(placeholder, replaceResult(code, atMsg)));
        return atMsg;
    }

    @Override
    public boolean matchEncode(ArrayMsg msg) {
        return msg.getType() == MsgTypeEnum.at;
    }

    @Override
    public ArrayMsg executeEncode(BaniraCodeContext context, ArrayMsg msg) {
        long qq = msg.getLongData("qq");
        if (context.sender() == null || context.sender() != qq) return msg;
        ArrayMsg arrayMsg = new ArrayMsg();
        arrayMsg.setType(MsgTypeEnum.text);
        arrayMsg.setData(Map.of(MsgTypeEnum.text.name(), DEFAULT_CODE));
        context.msg(context.msg().replace(msg.toCQCode(), DEFAULT_CODE));
        return arrayMsg;
    }

}
