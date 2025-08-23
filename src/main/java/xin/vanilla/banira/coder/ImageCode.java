package xin.vanilla.banira.coder;

import com.google.gson.JsonObject;
import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.common.utils.ShiroUtils;
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
 * 图片
 */
@Component
public class ImageCode implements BaniraCoder {

    @Override
    public List<String> getExample() {
        return List.of(
                CODE_START + "image" + ARG_SEPARATOR + "value" + VAL_SEPARATOR + ShiroUtils.getUserAvatar(123456789, 0) + CODE_END
                , CODE_START + "pic" + VAL_SEPARATOR + "pic/reimu.png" + CODE_END
        );
    }

    @Override
    public String getName() {
        return "图片";
    }

    @Override
    public String getDesc() {
        return "在消息中插入图片";
    }

    @Override
    public EnumCodeType getType() {
        return EnumCodeType.MSG;
    }

    private final Set<String> types = BaniraUtils.mutableSetOf(
            "image", "pic", "img"
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
        String url = JsonUtils.getString(data, "url");
        if (StringUtils.isNullOrEmptyEx(url)) url = JsonUtils.getString(data, "value");
        if (StringUtils.isNullOrEmptyEx(url)) return fail(context, code, placeholder);
        return context.setMsg(context.getMsg().replace(placeholder, MsgUtils.builder().img(url).build()));
    }

}
