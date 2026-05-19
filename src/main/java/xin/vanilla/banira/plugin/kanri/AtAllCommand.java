package xin.vanilla.banira.plugin.kanri;

import com.mikuac.shiro.common.utils.MsgUtils;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.InstructionsConfig;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.enums.EnumPermission;
import xin.vanilla.banira.plugin.help.HelpTopic;
import xin.vanilla.banira.plugin.help.HelpTopics;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.function.Supplier;

/**
 * AT全体成员
 */
@Component
public class AtAllCommand implements KanriHandler {

    @Resource
    private Supplier<InstructionsConfig> insConfig;

    @Nonnull
    @Override
    public HelpTopic getHelpSubTopic() {
        String detail = "用法：\n" + BaniraUtils.getKanriInsPrefixWithSpace() + getAction();
        return HelpTopics.of("艾特全体", "艾特全体成员。", 17, getAction()).detail(detail);
    }

    @Override
    public boolean botHasPermission(@Nonnull KanriContext context) {
        return context.bot().isGroupOwnerOrAdmin(context.group());
    }

    @Override
    public boolean hasPermission(@Nonnull KanriContext context) {
        return context.bot().hasPermission(context.group(), context.sender(), EnumPermission.ATAL);
    }

    @Nonnull
    @Override
    public List<String> getAction() {
        return Objects.requireNonNullElseGet(insConfig.get().base().atAll(), ArrayList::new);
    }

    @Override
    public int execute(@Nonnull KanriContext context, @Nonnull String[] args) {
        int replyId = -1;
        if (BaniraUtils.hasReply(context.event().getArrayMsg())) {
            replyId = BaniraUtils.getReplyId(context.event().getArrayMsg()).intValue();
        }
        if (replyId < 0) {
            replyId = context.msgId();
        }
        if (replyId < 0) {
            return FAIL;
        }

        context.bot().sendGroupMsg(context.group()
                , MsgUtils.builder()
                        .reply(replyId)
                        .atAll()
                        .build()
                , false
        );
        return SUCCESS;
    }

}
