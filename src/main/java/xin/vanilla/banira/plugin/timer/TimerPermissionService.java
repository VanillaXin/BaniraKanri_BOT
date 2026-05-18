package xin.vanilla.banira.plugin.timer;

import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.domain.TimerRecord;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CronUtils;

@Component
public class TimerPermissionService {

    public String validateAddPermission(BaniraBot bot, AnyMessageEvent event, TimerRecord timerRecord, String cron) {
        boolean owner = BaniraUtils.isOwner(event.getUserId());
        boolean butler = BaniraUtils.isButler(event.getUserId());
        boolean maid = BaniraUtils.isMaid(event.getGroupId(), event.getUserId());
        boolean inGroup = bot.isInGroup(timerRecord.getGroupId(), event.getUserId());
        boolean groupOwner = inGroup && bot.isGroupOwner(event.getGroupId(), event.getUserId());
        boolean groupAdmin = inGroup && bot.isGroupAdmin(event.getGroupId(), event.getUserId());
        boolean globalOp = owner || butler;
        boolean groupOp = groupOwner || groupAdmin || maid;
        boolean op = globalOp || groupOp;
        boolean groupIdEquals = timerRecord.getGroupId().equals(event.getGroupId());
        boolean groupIdValid = BaniraUtils.isGroupIdValid(timerRecord.getGroupId());
        if (groupIdValid && !op) {
            return "添加失败：权限不足";
        }
        if (groupIdValid && !groupIdEquals && !globalOp && !inGroup) {
            return "添加失败：权限不足(不在目标群)";
        }
        if (!CronUtils.isValidCron(cron)) {
            return "添加失败：cron表达式不合法";
        }
        if (CronUtils.hasTooShortInterval(cron, 30, 20)) {
            return "添加失败：表达式执行间隔过短";
        }
        return "";
    }
}
