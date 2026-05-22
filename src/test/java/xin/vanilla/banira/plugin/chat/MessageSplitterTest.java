package xin.vanilla.banira.plugin.chat;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.List;

class MessageSplitterTest {

    @Test
    void shouldKeepDanglingClosingQuoteWithPreviousPart() {
        List<String> parts = MessageSplitter.split("""
                从前有个AI，群友让她讲个笑话，她想了半天回了句："我不会讲笑话
                "
                你看，笑话这不就来了吗
                """, 260, 6);

        Assertions.assertEquals(List.of(
                "从前有个AI，群友让她讲个笑话，她想了半天回了句：\"我不会讲笑话\"",
                "你看，笑话这不就来了吗"
        ), parts);
    }
}
