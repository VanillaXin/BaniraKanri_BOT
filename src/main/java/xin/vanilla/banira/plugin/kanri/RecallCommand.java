package xin.vanilla.banira.plugin.kanri;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.GlobalConfig;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.enums.EnumPermission;
import xin.vanilla.banira.mapper.param.MessageRecordQueryParam;
import xin.vanilla.banira.service.IMessageRecordManager;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.Objects;
import java.util.Set;
import java.util.function.Supplier;

/**
 * 撤回消息
 */
@Component
public class RecallCommand implements KanriHandler {

    @Resource
    private Supplier<GlobalConfig> globalConfig;
    @Resource
    private IMessageRecordManager messageRecordManager;

    @Override
    public boolean botHasPermission(@Nonnull KanriContext context) {
        // 可以撤回自己的所以直接返回true
        return true;
    }

    @Override
    public boolean hasPermission(@Nonnull KanriContext context) {
        return context.bot().hasPermission(context.group(), context.sender(), EnumPermission.RECA);
    }

    @Nonnull
    @Override
    public Set<String> getAction() {
        return Objects.requireNonNullElseGet(globalConfig.get().instConfig().kanri().withdraw(), Set::of);
    }

    @Override
    public int execute(@Nonnull KanriContext context, @Nonnull String[] args) {
        // 解析目标
        Set<Integer> targets = BaniraUtils.mutableSetOf(context.msgId());

        int start;
        if (BaniraUtils.hasReply(context.event().getArrayMsg())) {
            start = BaniraUtils.getReplyId(context.event().getArrayMsg()).intValue();
            if (args.length == 0) targets.add(start);
        } else if (args.length > 0) {
            start = context.msgId();
        } else {
            return FAIL;
        }

        MessageRecord startMessage = messageRecordManager.getMessageRecord(context.group(), start);

        for (String arg : args) {
            String[] split = arg.replaceAll("[-：_~+]", ":")
                    .replaceAll("[`,，.。、/\\\\]", "")
                    .split(":");
            if (split.length > 2) return FAIL;
            MessageRecordQueryParam param = new MessageRecordQueryParam();
            param.setGroupId(context.group());
            param.setIdByLt(startMessage.getId());
            // 撤回指定第几条
            if (split.length == 1) {
                long index = StringUtils.toLong(split[0], -1);
                if (index == 0) {
                    targets.add(start);
                    continue;
                } else if (index > 0) {
                    param.setLimit(1);
                    param.setOffset(index - 1);
                } else {
                    return FAIL;
                }
            }
            // 撤回指定消息第 l 条后的 r 条消息
            else if (arg.contains("+")) {
                long startIndex = StringUtils.toLong(split[0], -1);
                long limit = StringUtils.toLong(split[1], -1);
                if (startIndex < 0 || limit < 0) return FAIL;
                param.setLimit(limit);
                param.setOffset(Math.max(0, startIndex - 1));
            }
            // 撤回指定范围第 l 至 r 条消息
            else {
                long startIndex = StringUtils.toLong(split[0], -1);
                long endIndex = StringUtils.toLong(split[1], -1);
                if (startIndex < 0 || endIndex < 0 || endIndex < startIndex) return FAIL;
                param.setLimit(endIndex - startIndex + 1);
                param.setOffset(Math.max(0, startIndex - 1));
            }
            param.addOrderBy(MessageRecordQueryParam.ORDER_ID, false);
            messageRecordManager.getMessageRecordList(param).stream()
                    .map(data -> StringUtils.toInt(data.getMsgId()))
                    .filter(data -> data > 0)
                    .forEach(targets::add);
        }

        for (Integer targetId : targets) {
            context.bot().deleteMsg(targetId);
        }

        return targets.isEmpty() ? FAIL : SUCCESS;
    }

}
