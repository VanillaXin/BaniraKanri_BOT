package xin.vanilla.banira.plugin.timer;

import com.mikuac.shiro.common.utils.MessageConverser;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.coder.common.BaniraCodeHandler;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.TimerRecord;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.DateUtils;
import xin.vanilla.banira.util.StringUtils;

@Slf4j
@Component
public class TimerTaskExecutor implements ITimerTaskExecutor {

    @Resource
    private BaniraCodeHandler codeHandler;

    @Override
    public void execute(TimerRecord task) {
        LOGGER.info("Executing timer task: {}", buildSafeTaskSummary(task));

        BaniraBot bot = BaniraUtils.getBot(task.getBotId());
        if (bot == null) return;

        BaniraCodeContext decode = codeHandler.decode(
                new BaniraCodeContext(bot
                        , MessageConverser.stringToArray(task.getReplyMsg())
                        , task.getGroupId()
                        , task.getCreatorId()
                        , task.getCreatorId()
                )
                        .opId(task.getCreatorId())
                        .msg(task.getReplyMsg())
                        .time(DateUtils.getTimestamp(null))
        );

        if (BaniraUtils.isGroupIdValid(decode.group())) {
            bot.sendGroupMsg(decode.group(), decode.msg(), false);
        } else if (BaniraUtils.isUserIdValid(decode.sender())) {
            bot.sendPrivateMsg(decode.sender(), decode.msg(), false);
        }

        LOGGER.info("Executed timer task: {}", buildSafeTaskSummary(task));
    }

    private String buildSafeTaskSummary(TimerRecord task) {
        if (task == null) {
            return "task=null";
        }
        int msgLength = StringUtils.isNullOrEmptyEx(task.getReplyMsg()) ? 0 : task.getReplyMsg().length();
        return String.format("id=%s, botId=%s, groupId=%s, creatorId=%s, cron=%s, replyMsgLength=%d",
                task.getId(), task.getBotId(), task.getGroupId(), task.getCreatorId(), task.getCron(), msgLength);
    }

}
