package xin.vanilla.banira.plugin.chat;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class PreflightMetaParserTest {

    @Test
    void parsesInvokeDecision() {
        PreflightMetaParser.PreflightDecision decision = PreflightMetaParser.parse(
                "[PREFLIGHT invoke=no interest=18]"
        );
        Assertions.assertFalse(decision.invoke());
        Assertions.assertEquals(18, decision.interest());
    }
}
