package xin.vanilla.banira.plugin.kanri;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.List;

class KanriExecuteReportTest {

    @Test
    void shouldDescribePartialMuteForAgent() {
        KanriExecuteReport report = new KanriExecuteReport(
                KanriHandler.SUCCESS,
                "禁言",
                List.of("吉祥物", "我是mjj"),
                List.of("aaaa", "bbbb"),
                10
        );

        String message = report.toAgentMessage();

        Assertions.assertTrue(message.contains("部分完成"));
        Assertions.assertTrue(message.contains("吉祥物"));
        Assertions.assertTrue(message.contains("aaaa"));
        Assertions.assertTrue(message.contains("口语化"));
    }
}
