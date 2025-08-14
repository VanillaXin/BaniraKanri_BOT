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
    public boolean hasPermission(@Nonnull KanriContext context) {
        return context.bot().hasPermission(context.group(), context.sender(), EnumPermission.RECA);
    }

    @Nonnull
    @Override
    public Set<String> getAction() {
        return globalConfig.get().instConfig().kanri().withdraw();
    }

    @Override
    public int execute(@Nonnull KanriContext context, @Nonnull String[] args) {
        // 解析目标
        Set<Integer> targets = BaniraUtils.mutableSetOf(context.msgId());

        int start;
        if (BaniraUtils.hasReplay(context.event().getArrayMsg())) {
            start = BaniraUtils.getReplayId(context.event().getArrayMsg()).intValue();
        } else if (args.length > 0) {
            start = context.msgId();
        } else {
            return FAIL;
        }

        MessageRecord startMessage = messageRecordManager.getMessageRecord(context.group(), start);

        for (String arg : args) {
            String[] split = arg.replace("-", ":")
                    .replace("：", ":")
                    .split(":");
            if (split.length > 2) return FAIL;
            MessageRecordQueryParam param = new MessageRecordQueryParam();
            param.setGroupId(context.group());
            param.setIdByLt(startMessage.getId());
            if (split.length == 1) {
                if (StringUtils.toLong(split[0]) == 0) {
                    targets.add(start);
                    continue;
                } else {
                    param.setLimit(1);
                    param.setOffset(StringUtils.toLong(split[0]) - 1);
                }
            } else {
                long startIndex = Math.max(0, StringUtils.toLong(split[0]) - 1);
                long endIndex = Math.max(0, StringUtils.toLong(split[1]) - 1);
                param.setLimit(endIndex - startIndex + 1);
                param.setOffset(startIndex);
            }
            param.addOrderBy(MessageRecordQueryParam.ORDER_ATTR_ID, false);
            messageRecordManager.getMessageRecordLimitList(param).stream()
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
