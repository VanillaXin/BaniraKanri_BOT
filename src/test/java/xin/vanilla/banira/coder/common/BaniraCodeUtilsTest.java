package xin.vanilla.banira.coder.common;

import org.junit.jupiter.api.Test;
import xin.vanilla.banira.util.JsonUtils;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

class BaniraCodeUtilsTest {

    @Test
    void parsePathParamWithArrayIndex() {
        String msg = "[bkode:img,value:https://example.com/api,$r:$auto,path:[0].items.[0].src]";
        List<BaniraCode> codes = BaniraCodeUtils.getAllBaniraCode(msg);

        BaniraCode imgCode = codes.stream()
                .filter(code -> "img".equals(code.getType()))
                .findFirst()
                .orElseThrow();

        assertEquals("[0].items.[0].src", JsonUtils.getString(imgCode.getData(), "path"));
    }

}
