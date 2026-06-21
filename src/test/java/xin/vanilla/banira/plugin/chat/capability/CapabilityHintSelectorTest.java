package xin.vanilla.banira.plugin.chat.capability;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.List;

class CapabilityHintSelectorTest {

    @Test
    void shouldSelectRelevantCapabilitiesForReminder() {
        List<AiCapability> selected = CapabilityHintSelector.selectRelevant(List.of(
                new AiCapability().name("web_search").description("搜索公开网页资料"),
                new AiCapability().name("create_timer").description("创建定时提醒"),
                new AiCapability().name("draw_today_wife").description("抽老婆"),
                new AiCapability().name("search_mcmod").description("搜索 MC百科")
        ), "五分钟后提醒我起床");

        Assertions.assertFalse(selected.isEmpty());
        Assertions.assertEquals("create_timer", selected.getFirst().name());
        Assertions.assertTrue(selected.stream().noneMatch(cap -> "draw_today_wife".equals(cap.name())));
    }

    @Test
    void shouldSelectKanriCapabilityForGroupNameIntent() {
        List<AiCapability> selected = CapabilityHintSelector.selectRelevant(List.of(
                new AiCapability().name("web_search").description("搜索公开网页资料"),
                new AiCapability().name("execute_kanri").description("执行允许的群管动作"),
                new AiCapability().name("get_group_summary").description("查询群基础信息")
        ), "把群名称改成测试群");

        Assertions.assertFalse(selected.isEmpty());
        Assertions.assertEquals("execute_kanri", selected.getFirst().name());
    }
}
