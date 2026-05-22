package xin.vanilla.banira.plugin.timer;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Service;
import xin.vanilla.banira.domain.PageResult;
import xin.vanilla.banira.domain.TimerRecord;
import xin.vanilla.banira.mapper.param.TimerRecordQueryParam;
import xin.vanilla.banira.service.ITimerRecordManager;
import xin.vanilla.banira.util.StringUtils;

/**
 * 定时任务查询，供插件指令与 AI 能力共用
 */
@Service
public class TimerQueryService {

    @Resource
    private ITimerRecordManager timerRecordManager;

    @Nonnull
    public String listTimers(long botId, long groupId, @Nonnull String keyword, long page) {
        if (page <= 0) {
            page = 1;
        }
        PageResult<TimerRecord> pageList = timerRecordManager.getTimerRecordPagedList(
                new TimerRecordQueryParam(true, page, 20)
                        .setBotId(botId)
                        .setGroupId(0L, groupId)
                        .setEnable(true)
                        .addKeyWord(StringUtils.isNotNullOrEmpty(keyword) ? String.format("%%%s%%", keyword) : null)
                        .addOrderBy(TimerRecordQueryParam.ORDER_ID, true)
        );
        if (pageList.isEmpty()) {
            return "未查询到定时任务";
        }
        StringBuilder builder = new StringBuilder();
        builder.append("定时任务总数：").append(pageList.getTotal())
                .append("，当前页：").append(pageList.getPage())
                .append('/').append(pageList.getTotalPages()).append('\n');
        for (TimerRecord record : pageList.getRecords()) {
            builder.append("\n#").append(record.getId())
                    .append(" 群").append(record.getGroupId())
                    .append('\n')
                    .append("Cron：").append(record.getCron()).append('\n')
                    .append("内容：").append(record.getReplyMsg()).append('\n');
        }
        return builder.toString().trim();
    }

}
