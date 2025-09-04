package xin.vanilla.banira.coder.message;

import com.google.gson.JsonObject;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.coder.common.BaniraCode;
import xin.vanilla.banira.coder.common.MessageCoder;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.enums.EnumCodeType;
import xin.vanilla.banira.plugin.kanri.KanriHandler;
import xin.vanilla.banira.plugin.kanri.MuteCommand;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.JsonUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.List;
import java.util.Set;

/**
 * 禁言
 */
@Component
public class MuteCode implements MessageCoder {

    @Resource
    private MuteCommand muteCommand;

    @Override
    public List<String> getExample() {
        return List.of(
                CODE_START + CollectionUtils.getRandomElement(types) + CODE_END
        );
    }

    @Override
    public String getName() {
        return "禁言";
    }

    @Override
    public String getDesc() {
        return "禁言发送者指定时长(秒)";
    }

    @Override
    public boolean isKanri() {
        return true;
    }

    @Override
    public EnumCodeType getType() {
        return EnumCodeType.MSG;
    }

    private static final Set<String> types = BaniraUtils.mutableSetOf(
            "mute", "ban", "禁言"
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
        String timeString = JsonUtils.getString(data, "value");
        if (StringUtils.isNullOrEmpty(timeString)) return fail(context, code, placeholder);
        String[] split = timeString.replace("_", "-")
                .replace("~", "-")
                .replace("+", "-")
                .replace("#", "-")
                .split("-");
        if (split.length > 2) return fail(context, code, placeholder);
        double time;
        if (split.length == 1) {
            time = StringUtils.toInt(split[0]);
        } else {
            time = RANDOM.nextInt(StringUtils.toInt(split[0]), StringUtils.toInt(split[1]));
        }
        if (time <= 0) return fail(context, code, placeholder);

        KanriContext kanriContext = new KanriContext(new AnyMessageEvent()
                , context.bot()
                , context.group()
                , context.opId()
                , context.msgId()
                , ""
                , ""
        ).coder(true);

        String[] args = {String.valueOf(context.sender()), String.valueOf(time / 60d)};

        if (muteCommand.execute(kanriContext, args) != KanriHandler.FAIL) {
            return context.msg(context.msg().replace(placeholder, ""));
        } else {
            return fail(context, code, placeholder);
        }

    }

}
