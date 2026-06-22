package xin.vanilla.banira.plugin.chat.memory;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import xin.vanilla.banira.config.entity.extended.ChatConfig;

class MemoryExtractorTest {

    @Test
    void shouldNormalizeOwnerNamingPreferenceWithExplicitSubject() {
        String normalized = MemoryExtractor.normalizeOwnerBehaviorInstruction(
                "白茶酱，以后叫我月酱，别叫我主人",
                new ChatConfig()
        );

        Assertions.assertTrue(normalized.contains("主人账号"));
        Assertions.assertTrue(normalized.contains("称呼主人账号为「月酱」"));
        Assertions.assertTrue(normalized.contains("不要称呼主人账号为「主人」"));
        Assertions.assertFalse(normalized.contains("以后叫我"));
    }

    @Test
    void shouldRejectNonOwnerClaimingOwnerTitleAsMemory() {
        Assertions.assertTrue(MemoryScopePolicy.isUnsafeOwnerTitleClaim(
                123456L,
                "当前用户希望被叫主人"
        ));
    }

    @Test
    void shouldRejectCapabilityFailureAsLowImportanceMemory() {
        Assertions.assertFalse(MemoryExtractor.isWorthLowImportanceMemory(
                "好吧实话说了，改我自己群名片这个操作好像确实没权限，之前那次其实也没成功"
        ));
        Assertions.assertFalse(MemoryExtractor.isWorthLowImportanceMemory(
                "抱歉，看起来我目前没有禁言的权限，操作被拦住了"
        ));
        Assertions.assertTrue(MemoryExtractor.isWorthLowImportanceMemory(
                "我刚才说的是那条截图里的内容，不是你本人"
        ));
    }
}
