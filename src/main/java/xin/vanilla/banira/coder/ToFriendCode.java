package xin.vanilla.banira.coder;

import com.google.gson.JsonObject;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.coder.common.BaniraCode;
import xin.vanilla.banira.coder.common.BaniraCoder;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.enums.EnumCodeType;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.JsonUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.List;
import java.util.Set;

/**
 * 转发至好友
 */
@Component
public class ToFriendCode implements BaniraCoder {

    @Override
    public List<String> getExample() {
        return List.of(
                CODE_START + "tf" + VAL_SEPARATOR + "123456789" + CODE_END
        );
    }

    @Override
    public String getName() {
        return "转发至好友";
    }

    @Override
    public String getDesc() {
        return "将消息回复目标改为指定好友";
    }

    @Override
    public EnumCodeType getType() {
        return EnumCodeType.MSG;
    }

    private static final Set<String> types = BaniraUtils.mutableSetOf(
            "tf", "2f", "tofriend", "2friend"
    );

    @Override
    public boolean match(String msg) {
        return types.contains(msg);
    }

    @Override
    public BaniraCodeContext execute(BaniraCodeContext context, BaniraCode code, String placeholder) {
        if (notMatch(code)) return context;
        JsonObject data = code.getData();
        if (data == null) return fail(context, code, placeholder);
        String friend = JsonUtils.getString(data, "value");
        if (StringUtils.isNullOrEmptyEx(friend)) return fail(context, code, placeholder);
        long friendId = StringUtils.toLong(friend);
        if (!BaniraUtils.isFriendIdValid(friendId)) return fail(context, code, placeholder);
        return context.target(friendId).msg(context.msg().replace(placeholder, ""));
    }

}
