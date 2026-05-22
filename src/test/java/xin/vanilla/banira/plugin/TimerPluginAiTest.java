package xin.vanilla.banira.plugin;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Method;

class TimerPluginAiTest {

    @Test
    void shouldAppendYearToOneShotCronWithoutYear() throws Exception {
        Method method = TimerPlugin.class.getDeclaredMethod("normalizeAiCron", String.class);
        method.setAccessible(true);

        String cron = (String) method.invoke(null, "0 30 12 25 5 ?");

        Assertions.assertEquals(7, cron.split("\\s+").length);
    }

    @Test
    void shouldOptimizeReminderMessageByScene() throws Exception {
        Method method = TimerPlugin.class.getDeclaredMethod("optimizeReminderMessage", String.class);
        method.setAccessible(true);

        Assertions.assertEquals("该起床了", method.invoke(null, "提醒：起床"));
        Assertions.assertEquals("记得吃饭", method.invoke(null, "吃饭"));
        Assertions.assertEquals("请记得开会", method.invoke(null, "开会"));
    }
}
