package xin.vanilla.banira.coder;

import org.springframework.stereotype.Component;
import xin.vanilla.banira.coder.common.BaniraCode;
import xin.vanilla.banira.coder.common.BaniraCoder;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.enums.EnumCodeType;

import java.util.List;

@Component
public class TextCode implements BaniraCoder {

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
    public BaniraCodeContext execute(BaniraCodeContext context, BaniraCode code, String placeholder) {
        return context;
    }

}
