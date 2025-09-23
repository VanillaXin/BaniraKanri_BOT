package xin.vanilla.banira.coder.common;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.domain.BaniraCodeContext;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

@Slf4j
@Component
public class BaniraCodeHandler {

    @Autowired(required = false)
    private List<MessageCoder> coders = new ArrayList<>();

    public BaniraCodeContext decode(BaniraCodeContext context) {
        BaniraCodeContext clone = context.clone();
        List<BaniraCode> codeList = BaniraCodeUtils.getAllBaniraCode(clone.msg());
        if (codeList.isEmpty()) return clone;
        BaniraCode textCode = BaniraCodeUtils.getTextBaniraCode(codeList);
        if (textCode == null) return clone;
        clone.msg(textCode.getData().get("text").getAsString());

        double size = codeList.size();
        for (int i = 0; i < size; i++) {
            BaniraCode code = codeList.get(i);
            int finalI = i;
            for (MessageCoder coder : coders.stream()
                    // .filter(coder -> !coder.isKanri())
                    // 将有$w的coder放在前面
                    .sorted(Comparator.comparingDouble(c -> c.getPriority() + (c.hasWrite(code) ? (finalI / size) : (size / (size + 1)))))
                    .toList()) {
                if (coder.match(code.getType())) {
                    try {
                        String back = coder.execute(clone, code, BaniraCodeUtils.placeholder(i));
                        coder.writeValue(clone, code, back);
                    } catch (Exception e) {
                        coder.fail(clone, code, BaniraCodeUtils.placeholder(i));
                        LOGGER.error("Failed to decode banira code", e);
                    }
                }
            }
        }
        return clone;
    }

}
