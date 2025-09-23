package xin.vanilla.banira.coder.message;

import org.springframework.stereotype.Component;
import xin.vanilla.banira.coder.common.BaniraCode;
import xin.vanilla.banira.coder.common.MessageCoder;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.enums.EnumCodeType;

import java.util.List;

@Component
public class TextCode implements MessageCoder {

    @Override
    public List<String> getExample() {
        return List.of();
    }

    @Override
    public String getName() {
        return "";
    }

    @Override
    public String getDesc() {
        return "";
    }

    @Override
    public EnumCodeType getType() {
        return EnumCodeType.MSG;
    }

    @Override
    public boolean match(String msg) {
        return "text".equals(msg);
    }

    @Override
    public String execute(BaniraCodeContext context, BaniraCode code, String placeholder) {
        return "";
    }

}
