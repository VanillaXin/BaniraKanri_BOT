package xin.vanilla.banira.plugin.chat;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class KanriMuteIntentTest {

    @Test
    void shouldExtractOneHourAsSixtyMinutes() {
        Assertions.assertEquals(60, KanriMuteIntent.extractMinutes("禁他1个小时", 10));
        Assertions.assertEquals(30, KanriMuteIntent.extractMinutes("半小时", 10));
    }

    @Test
    void shouldDetectDurationOnlyReply() {
        Assertions.assertTrue(KanriMuteIntent.hasMuteDurationOnlyReply("1个小时"));
        Assertions.assertTrue(KanriMuteIntent.hasMuteDurationOnlyReply("30天"));
    }

    @Test
    void shouldExtractThirtyDaysAsMinutes() {
        Assertions.assertEquals(30 * 24 * 60, KanriMuteIntent.extractMinutes("30天", 10));
    }

    @Test
    void shouldExtractOneMonthAsThirtyDays() {
        Assertions.assertTrue(KanriMuteIntent.hasMuteDurationOnlyReply("一月"));
        Assertions.assertEquals(30 * 24 * 60, KanriMuteIntent.extractMinutes("禁我一月", 10));
    }
}
