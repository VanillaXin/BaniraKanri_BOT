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
 * 进群
 */
@Component
public class InGroupCode implements EventCoder {

    @Override
    public List<String> getExample() {
        String type = CollectionUtils.getRandomElement(types);
        return List.of(CODE_START + type + CODE_END +
                CODE_START + type + VAL_SEPARATOR + "group" + VAL_SEPARATOR + "123456" + CODE_END +
                CODE_START + type + VAL_SEPARATOR + "user" + VAL_SEPARATOR + "123456" + CODE_END +
                CODE_START + type + VAL_SEPARATOR + "operator" + VAL_SEPARATOR + "123456" + CODE_END
        );
    }

    @Override
    public String getName() {
        return "加入群聊";
    }

    @Override
    public String getDesc() {
        return "某人加入群聊时触发该事件";
    }

    private static final Set<String> types = BaniraUtils.mutableSetOf(
            "join", "enter"
    );

    @Override
    public String build(JsonObject data) {
        Long group = JsonUtils.getLong(data, "group");
        Long userId = JsonUtils.getLong(data, "userId");
        Long operatorId = JsonUtils.getLong(data, "operatorId");
        StringBuilder sb = new StringBuilder();
        for (String type : types) {
            sb.append(CODE_START).append(type).append(CODE_END);
            sb.append(CODE_START).append(type).append(VAL_SEPARATOR).append("group").append(VAL_SEPARATOR).append(group).append(CODE_END);
            sb.append(CODE_START).append(type).append(VAL_SEPARATOR).append("user").append(VAL_SEPARATOR).append(userId).append(CODE_END);
            sb.append(CODE_START).append(type).append(VAL_SEPARATOR).append("operator").append(VAL_SEPARATOR).append(operatorId).append(CODE_END);
        }
        return sb.toString();
    }

}
