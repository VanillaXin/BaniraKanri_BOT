package xin.vanilla.banira.coder.message;

import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.coder.common.BaniraCode;
import xin.vanilla.banira.coder.common.MessageCoder;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.enums.EnumCodeType;
import xin.vanilla.banira.plugin.kanri.KanriHandler;
import xin.vanilla.banira.plugin.kanri.RecallCommand;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;

import java.util.List;
import java.util.Set;

/**
 * 撤回
 */
@Component
public class RecallCode implements MessageCoder {

    @Resource
    private RecallCommand recallCommand;

    @Override
    public List<String> getExample() {
        return List.of(
                CODE_START + CollectionUtils.getRandomElement(types) + CODE_END
        );
    }

    @Override
    public String getName() {
        return "撤回";
    }

    @Override
    public String getDesc() {
        return "撤回触发的消息";
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
            "recall", "withdraw", "rec"
    );

    @Override
    public boolean match(String msg) {
        return types.contains(msg);
    }

    @Override
    public String execute(BaniraCodeContext context, BaniraCode code, String placeholder) {
        if (notMatch(code)) return "";

        KanriContext kanriContext = new KanriContext(new AnyMessageEvent()
                , context.bot()
                , context.group()
                , context.opId()
                , context.msgId()
                , ""
                , ""
        ).coder(true);

        String[] args = {String.valueOf(1)};

        if (recallCommand.execute(kanriContext, args) != KanriHandler.FAIL) {
            context.msg(context.msg().replace(placeholder, ""));
            return "";
        } else {
            return fail(context, code, placeholder);
        }

    }

}
