package xin.vanilla.banira.util;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class CronUtilsTest {

    @Test
    void shouldValidateCronExpression() {
        Assertions.assertTrue(CronUtils.isValidCron("0/30 * * * * ?"));
        Assertions.assertFalse(CronUtils.isValidCron("invalid cron"));
    }

    @Test
    void shouldDetectTooShortInterval() {
        Assertions.assertTrue(CronUtils.hasTooShortInterval("0/1 * * * * ?", 30, 20));
        Assertions.assertFalse(CronUtils.hasTooShortInterval("0 0/5 * * * ?", 30, 20));
    }
}
