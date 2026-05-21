package xin.vanilla.banira.plugin.kanri;

import com.mikuac.shiro.dto.action.common.ActionData;
import com.mikuac.shiro.dto.action.common.ActionList;
import com.mikuac.shiro.dto.action.common.MsgId;
import com.mikuac.shiro.dto.action.response.EssenceMsgResp;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.InstructionsConfig;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.enums.EnumPermission;
import xin.vanilla.banira.config.entity.basic.BaseInstructionsConfig;
import xin.vanilla.banira.plugin.help.HelpTopic;
import xin.vanilla.banira.plugin.help.HelpTopics;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.function.Supplier;

/**
 * 设置精华消息
 */
@Component
public class EssenceCommand implements KanriHandler {

    @Resource
    private Supplier<InstructionsConfig> insConfig;

    @Nonnull
    @Override
    public HelpTopic getHelpSubTopic() {
        String prefix = BaniraUtils.getKanriInsPrefixWithSpace();
        BaseInstructionsConfig base = insConfig.get().base();
        String actionHint = HelpTopics.formatAliasChoices(getAction());
        String addHint = HelpTopics.formatAliasChoices(base.add());
        String delHint = HelpTopics.formatAliasChoices(base.del());
        return HelpTopics.of("群精华消息", "添加或删除群精华消息。", 23, getAction())
                .child(HelpTopics.opAdd(base,
                        "用法1：\n" + prefix + actionHint + " " + addHint + " <精华消息>\n\n"
                                + "用法2：(回复要添加的内容)\n" + prefix + actionHint + " " + addHint))
                .child(HelpTopics.opDel(base,
                        "用法1：\n" + prefix + actionHint + " " + delHint + " <精华消息>\n\n"
                                + "用法2：(回复要删除的内容)\n" + prefix + actionHint + " " + delHint));
    }

    @Override
    public boolean botHasPermission(@Nonnull KanriContext context) {
        return context.bot().isGroupOwnerOrAdmin(context.group());
    }

    @Override
    public boolean hasPermission(@Nonnull KanriContext context) {
        return context.bot().hasAnyPermissions(context.group(), context.sender(), EnumPermission.AESS, EnumPermission.RESS);
    }

    @Nonnull
    @Override
    public List<String> getAction() {
        return Objects.requireNonNullElseGet(insConfig.get().kanri().essence(), List::of);
    }

    @Override
    public int execute(@Nonnull KanriContext context, @Nonnull String[] args) {
        // 解析操作
        Boolean operate = null;
        if (args.length > 0) {
            if (insConfig.get().base().add().contains(args[0])) {
                operate = true;
            } else if (insConfig.get().base().del().contains(args[0])) {
                operate = false;
            }
        }

        // 解析目标
        int target;
        // 有回复
        if (BaniraUtils.hasReply(context.event().getArrayMsg())) {
            // 没有参数 或 参数只有添加/删除
            if (args.length == 0 || (args.length == 1 && operate != null)) {
                target = BaniraUtils.getReplyId(context.event().getArrayMsg()).intValue();
            } else {
                return FAIL;
            }
        }
        // 没有回复 且 有内容
        else if (args.length > 0) {
            String content;
            if (Boolean.TRUE.equals(operate)) {
                content = String.join("", Arrays.copyOfRange(args, 1, args.length));
            } else if (operate == null) {
                content = context.content();
            } else {
                return FAIL;
            }
            ActionData<MsgId> msgId = context.bot().sendGroupMsg(context.group(), content, false);
            if (context.bot().isActionDataMsgIdNotEmpty(msgId)) {
                target = context.bot().getActionDataMsgId(msgId);
                operate = true;
            } else {
                target = 0;
            }
        } else {
            target = 0;
        }

        // 判断权限
        if (operate == null
                && !context.bot().hasAllPermissions(context.group(), context.sender()
                , EnumPermission.AESS
                , EnumPermission.RESS)
        ) return NO_OP;
        else if (Boolean.TRUE.equals(operate)
                && !context.bot().hasPermission(context.group(), context.sender()
                , EnumPermission.AESS)
        ) return NO_OP;
        else if (Boolean.FALSE.equals(operate)
                && !context.bot().hasPermission(context.group(), context.sender()
                , EnumPermission.RESS)
        ) return NO_OP;

        if (target > 0) {
            boolean add = Objects.requireNonNullElseGet(operate, () -> {
                boolean result = true;
                ActionList<EssenceMsgResp> essenceMsgList = context.bot().getEssenceMsgList(context.group());
                if (essenceMsgList != null && CollectionUtils.isNotNullOrEmpty(essenceMsgList.getData())) {
                    result = essenceMsgList.getData().stream()
                            .noneMatch(data -> data.getMessageId() != null && target == data.getMessageId());
                }
                return result;
            });

            if (add) {
                context.bot().setEssenceMsg(target);
            } else {
                context.bot().deleteEssenceMsg(target);
            }
        }

        return target > 0 ? SUCCESS : FAIL;
    }

}
