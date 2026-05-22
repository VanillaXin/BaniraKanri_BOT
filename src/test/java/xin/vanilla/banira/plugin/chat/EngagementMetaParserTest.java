package xin.vanilla.banira.plugin.chat;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import xin.vanilla.banira.config.entity.extended.ChatReplySettings;

class EngagementMetaParserTest {

    @Test
    void parsesSilentFollowInterest() {
        EngagementMetaParser.ParsedEngagement parsed = EngagementMetaParser.parse(
                "嗯\n[ENGAGE reply=no interest=72]"
        );
        Assertions.assertFalse(parsed.meta().shouldReply());
        Assertions.assertEquals(72, parsed.meta().interest());
        Assertions.assertEquals("嗯", parsed.speech());
    }

    @Test
    void stripsEngageTagFromSpeech() {
        EngagementMetaParser.ParsedEngagement parsed = EngagementMetaParser.parse(
                "行啊\n[ENGAGE reply=yes interest=40]"
        );
        Assertions.assertTrue(parsed.meta().shouldReply());
        Assertions.assertEquals(40, parsed.meta().interest());
        Assertions.assertFalse(parsed.speech().contains("[ENGAGE"));
    }

    @Test
    void parsesPipeSeparatedEngageTag() {
        EngagementMetaParser.ParsedEngagement parsed = EngagementMetaParser.parse(
                "算了\n[ENGAGE reply=yes|interest=30]"
        );

        Assertions.assertTrue(parsed.meta().shouldReply());
        Assertions.assertEquals(30, parsed.meta().interest());
        Assertions.assertEquals("算了", parsed.speech());
    }

    @Test
    void infersReplyWhenTagMissing() {
        EngagementMetaParser.ParsedEngagement parsed = EngagementMetaParser.parse("收到");
        Assertions.assertTrue(parsed.meta().shouldReply());
        Assertions.assertEquals("收到", parsed.speech());
    }
}
