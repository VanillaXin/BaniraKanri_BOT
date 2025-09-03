package xin.vanilla.banira.coder.event;

import com.google.gson.JsonObject;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.coder.common.EventCoder;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.JsonUtils;

import java.util.List;
import java.util.Set;

/**
 * 戳一戳
 */
@Component
public class PokeCode implements EventCoder {

    @Override
    public List<String> getExample() {
        String type = CollectionUtils.getRandomElement(types);
        return List.of(CODE_START + type + CODE_END +
                CODE_START + type + VAL_SEPARATOR + "sender" + VAL_SEPARATOR + "123456" + CODE_END +
                CODE_START + type + VAL_SEPARATOR + "target" + VAL_SEPARATOR + "123456" + CODE_END
        );
    }

    @Override
    public String getName() {
        return "戳一戳";
    }

    @Override
    public String getDesc() {
        return "某人被双击头像时触发该事件";
    }

    private static final Set<String> types = BaniraUtils.mutableSetOf(
            "poke", "tap"
    );

    @Override
    public String build(JsonObject data) {
        Long targetId = JsonUtils.getLong(data, "targetId");
        Long senderId = JsonUtils.getLong(data, "senderId");
        StringBuilder sb = new StringBuilder();
        for (String type : types) {
            sb.append(CODE_START).append(type).append(CODE_END);
            sb.append(CODE_START).append(type).append(VAL_SEPARATOR).append("sender").append(VAL_SEPARATOR).append(senderId).append(CODE_END);
            sb.append(CODE_START).append(type).append(VAL_SEPARATOR).append("target").append(VAL_SEPARATOR).append(targetId).append(CODE_END);
        }
        return sb.toString();
    }

}
