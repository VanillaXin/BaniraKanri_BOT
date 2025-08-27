package xin.vanilla.banira.coder.common;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.domain.BaniraCodeContext;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

@Component
public class BaniraCodeHandler {

    @Autowired(required = false)
    private List<BaniraCoder> coders = new ArrayList<>();

    public BaniraCodeContext decode(BaniraCodeContext context) {
        BaniraCodeContext clone = context.clone();
        List<BaniraCode> codeList = BaniraCodeUtils.getAllBaniraCode(clone.msg());
        if (codeList.isEmpty()) return clone;
        BaniraCode textCode = BaniraCodeUtils.getTextBaniraCode(codeList);
        if (textCode == null) return clone;
        clone.msg(textCode.getData().get("text").getAsString());

        for (BaniraCoder coder : coders.stream()
                .filter(coder -> !coder.isKanri())
                .sorted(Comparator.comparingInt(BaniraCoder::getPriority))
                .toList()) {
            for (int i = 0; i < codeList.size(); i++) {
                BaniraCode code = codeList.get(i);
                if (coder.match(code.getType())) {
                    clone = coder.execute(clone, code, BaniraCodeUtils.placeholder(i));
                }
            }
        }
        return clone;
    }

    public BaniraCodeContext decodeWithKanri(BaniraCodeContext context) {
        BaniraCodeContext clone = context.clone();
        return clone;
    }

}
