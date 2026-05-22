package xin.vanilla.banira.plugin.kanri;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class KanriServiceTest {

    @Test
    void shouldExposeAgentMessageForBotNoAdmin() {
        KanriExecuteReport report = KanriExecuteReport.withMessage(
                KanriHandler.BOT_NO_OP,
                "禁言",
                "机器人没有执行该操作的权限（需要机器人为 QQ 群主或群管）。"
        );

        Assertions.assertTrue(report.toAgentMessage().contains("机器人没有执行"));
    }
}
