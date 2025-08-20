package xin.vanilla.banira.plugin.kanri;

import com.mikuac.shiro.dto.action.common.ActionData;
import com.mikuac.shiro.dto.action.common.ActionList;
import com.mikuac.shiro.dto.action.common.MsgId;
import com.mikuac.shiro.dto.action.response.EssenceMsgResp;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.GlobalConfig;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.enums.EnumPermission;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.Objects;
import java.util.Set;
import java.util.function.Supplier;

/**
 * 设置精华消息
 */
@Component
public class EssenceCommand implements KanriHandler {

    @Resource
    private Supplier<GlobalConfig> globalConfig;

    @Nonnull
    @Override
    public String getHelpInfo(String type) {
        if (this.getAction().stream().anyMatch(s -> StringUtils.isNullOrEmptyEx(type) || s.equalsIgnoreCase(type))) {
            return "设置群精华消息：\n\n" +
                    "用法1：\n" +
                    "添加：\n" +
                    BaniraUtils.getKanriInsPrefixWithSpace() +
                    this.getAction() + " " +
                    globalConfig.get().instConfig().base().add() + " " +
                    "<精华消息>" +
                    "<QQ号|艾特> ... " + "<名片>" + "\n\n" +
                    "用法2：(回复要设置的内容)\n" +
                    "添加：\n" +
                    BaniraUtils.getKanriInsPrefixWithSpace() +
                    this.getAction() +
                    globalConfig.get().instConfig().base().add() +
                    "删除：\n" +
                    BaniraUtils.getKanriInsPrefixWithSpace() +
                    this.getAction() + " " +
                    globalConfig.get().instConfig().base().del()
                    ;
        }
        return "";
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
    public Set<String> getAction() {
        return Objects.requireNonNullElseGet(globalConfig.get().instConfig().kanri().essence(), Set::of);
    }

    @Override
    public int execute(@Nonnull KanriContext context, @Nonnull String[] args) {
        // 解析操作
        Boolean operate = null;
        if (args.length > 0) {
            if (globalConfig.get().instConfig().base().add().contains(args[0])) {
                operate = true;
            } else if (globalConfig.get().instConfig().base().del().contains(args[0])) {
                operate = false;
            }
        }

        // 解析目标
        int target;
        if (BaniraUtils.hasReply(context.event().getArrayMsg()) && args.length == 0) {
            target = (int) BaniraUtils.getReplyQQ(context.bot(), context.group(), context.event().getArrayMsg());
        } else if (args.length > 0) {
            ActionData<MsgId> msgId = context.bot().sendGroupMsg(context.group(), context.content(), false);
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

        return target > 0 ? FAIL : SUCCESS;
    }

}
