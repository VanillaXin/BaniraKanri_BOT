package xin.vanilla.banira.coder;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.domain.BaniraCodeContext;

import java.util.ArrayList;
import java.util.List;

@Component
public class BaniraCodeHandler {

    @Autowired(required = false)
    private List<BaniraCoder> coders = new ArrayList<>();

    public BaniraCodeContext decode(BaniraCodeContext context) {
        BaniraCodeContext clone = context.clone();
        return clone;
    }

    public BaniraCodeContext decodeWithKanri(BaniraCodeContext context) {
        BaniraCodeContext clone = context.clone();
        return clone;
    }

}
