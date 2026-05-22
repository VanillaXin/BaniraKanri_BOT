package xin.vanilla.banira.plugin.chat;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.plugin.common.BaniraBot;

import java.util.List;

class KanriMuteTargetResolverTest {

    @Test
    void shouldResolveMentionedTargetAndCurrentDuration() {
        BaniraBot bot = Mockito.mock(BaniraBot.class);

        KanriMuteTargetResolver.ResolvedMute resolved = KanriMuteTargetResolver.resolve(
                bot,
                1L,
                999L,
                111L,
                List.of(),
                "[CQ:at,qq=999] [CQ:at,qq=222] 10分钟",
                null
        );

        Assertions.assertNotNull(resolved);
        Assertions.assertEquals(List.of(222L), resolved.targets());
        Assertions.assertEquals(10, resolved.minutes());
    }

    @Test
    void shouldUseRecentDurationOnlyReplyForMentionedTarget() {
        MessageRecord duration = new MessageRecord();
        duration.setSenderId(111L);
        duration.setMsgRecode("1个小时");
        BaniraBot bot = Mockito.mock(BaniraBot.class);

        KanriMuteTargetResolver.ResolvedMute resolved = KanriMuteTargetResolver.resolve(
                bot,
                1L,
                999L,
                111L,
                List.of(duration),
                "[CQ:at,qq=999] [CQ:at,qq=222] 再试试",
                null
        );

        Assertions.assertNotNull(resolved);
        Assertions.assertEquals(List.of(222L), resolved.targets());
        Assertions.assertEquals(60, resolved.minutes());
    }

    @Test
    void shouldNotInferSelfTargetFromColloquialMessage() {
        BaniraBot bot = Mockito.mock(BaniraBot.class);

        KanriMuteTargetResolver.ResolvedMute resolved = KanriMuteTargetResolver.resolve(
                bot,
                1L,
                999L,
                111L,
                List.of(),
                "[CQ:at,qq=999] 杀我",
                null
        );

        Assertions.assertNull(resolved);
    }
}
