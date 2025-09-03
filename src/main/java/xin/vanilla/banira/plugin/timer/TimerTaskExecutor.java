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
import xin.vanilla.banira.util.JsonUtils;

@Slf4j
@Component
public class TimerTaskExecutor implements ITimerTaskExecutor {

    @Resource
    private BaniraCodeHandler codeHandler;

    @Override
    public void execute(TimerRecord task) {
        String taskJsonStrong = JsonUtils.toJsonString(task);
        LOGGER.info("Executing timer task: {}", taskJsonStrong);

        BaniraBot bot = BaniraUtils.getBot(task.getBotId());
        if (bot == null) return;

        BaniraCodeContext decode = codeHandler.decode(
                new BaniraCodeContext(bot, MessageConverser.stringToArray(task.getReplyMsg()))
                        .group(task.getGroupId())
                        .sender(task.getCreatorId())
                        .target(task.getCreatorId())
                        .opId(task.getCreatorId())
                        .msg(task.getReplyMsg())
                        .time(DateUtils.getTimestamp(null))
        );

        if (BaniraUtils.isGroupIdValid(decode.group())) {
            bot.sendGroupMsg(decode.group(), decode.msg(), false);
        } else if (BaniraUtils.isUserIdValid(decode.target())) {
            bot.sendPrivateMsg(decode.target(), decode.msg(), false);
        }

        LOGGER.info("Executed timer task: {}", taskJsonStrong);
    }

}
