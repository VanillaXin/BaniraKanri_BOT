package xin.vanilla.banira.plugin.timer;

import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import xin.vanilla.banira.domain.TimerRecord;
import xin.vanilla.banira.plugin.common.BaniraBot;

class TimerPermissionServiceTest {

    @Test
    void shouldRejectInvalidCron() {
        TimerPermissionService service = new TimerPermissionService();
        BaniraBot bot = Mockito.mock(BaniraBot.class);
        AnyMessageEvent event = Mockito.mock(AnyMessageEvent.class);
        Mockito.when(event.getUserId()).thenReturn(10001L);
        Mockito.when(event.getGroupId()).thenReturn(20001L);
        Mockito.when(bot.isInGroup(Mockito.anyLong(), Mockito.anyLong())).thenReturn(false);

        TimerRecord record = new TimerRecord()
                .setGroupId(20001L)
                .setCreatorId(10001L);

        String reason = service.validateAddPermission(bot, event, record, "invalid cron");
        Assertions.assertTrue(reason.contains("cron表达式不合法"));
    }
}
