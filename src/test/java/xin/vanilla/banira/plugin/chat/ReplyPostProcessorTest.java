package xin.vanilla.banira.plugin.chat;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import xin.vanilla.banira.config.entity.extended.ChatReplySettings;

class ReplyPostProcessorTest {

    @Test
    void shouldTreatMaxReplyCharsAsSoftBudgetForModerateSpeech() {
        ChatReplySettings settings = new ChatReplySettings()
                .maxReplyChars(20)
                .maxCharsPerPart(30)
                .maxSplitParts(2)
                .maxForwardLength(60);
        String text = "alpha beta gamma delta epsilon zeta";

        String result = ReplyPostProcessor.process(text, settings);

        Assertions.assertEquals(text, result);
    }

    @Test
    void shouldStripMarkdownEmphasisFromSpeech() {
        String result = ReplyPostProcessor.process("**重点**是 `竹叶清`，不是****别的。", new ChatReplySettings());

        Assertions.assertEquals("重点是 竹叶清，不是****别的", result);
    }

    @Test
    void shouldStripMarkdownListMarkersFromSpeechLines() {
        String result = ReplyPostProcessor.process("""
                - 先看名字
                - 再查资料
                1. 最后确认
                """, new ChatReplySettings());

        Assertions.assertEquals("""
                先看名字
                再查资料
                最后确认""", result);
    }

    @Test
    void shouldKeepMaskedSecretsWithAsterisks() {
        String result = ReplyPostProcessor.process("接口 key 是 tp-****abc", new ChatReplySettings());

        Assertions.assertEquals("接口 key 是 tp-****abc", result);
    }

    @Test
    void shouldStripCodeFenceMarkersFromSpeech() {
        String result = ReplyPostProcessor.process("""
                ```java
                class Demo {}
                ```
                """, new ChatReplySettings());

        Assertions.assertEquals("class Demo {}", result);
    }

    @Test
    void shouldStripInternalMetaLinesFromSpeech() {
        String result = ReplyPostProcessor.process("""
                我在
                [ENGAGE reply=yes|interest=30]
                [PREFLIGHT invoke=no interest=0]
                """, new ChatReplySettings());

        Assertions.assertEquals("我在", result);
    }

    @Test
    void shouldNotSplitClosingQuoteIntoSeparateLine() {
        String result = ReplyPostProcessor.process("""
                从前有个AI，群友让她讲个笑话，她想了半天回了句："我不会讲笑话。"
                ——你看，笑话这不就来了吗。
                """, new ChatReplySettings());

        Assertions.assertEquals("""
                从前有个AI，群友让她讲个笑话，她想了半天回了句："我不会讲笑话"
                你看，笑话这不就来了吗""", result);
    }

    @Test
    void shouldMergeDanglingClosingQuoteLine() {
        String result = ReplyPostProcessor.process("""
                从前有个AI，群友让她讲个笑话，她想了半天回了句："我不会讲笑话
                "
                ——你看，笑话这不就来了吗
                """, new ChatReplySettings());

        Assertions.assertEquals("""
                从前有个AI，群友让她讲个笑话，她想了半天回了句："我不会讲笑话"
                你看，笑话这不就来了吗""", result);
    }
}
