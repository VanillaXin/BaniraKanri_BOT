package xin.vanilla.banira.plugin.chat.capability;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import xin.vanilla.banira.plugin.chat.agent.AgentContext;

import java.util.Map;

class CapabilityInvocationPolicyTest {

    @Test
    void shouldAllowMcmodSearchOnlyWhenCurrentMessageStillAsksMcmod() {
        AgentContext changedTopic = new AgentContext().userMessage("帮我在模组水仙辞下方留言：好酷");

        CapabilityInvocationPolicy.Decision blocked = CapabilityInvocationPolicy.evaluate(
                changedTopic,
                new AiCapability().name("search_mcmod"),
                "search_mcmod",
                Map.of("type", "author", "keyword", "Flechazo")
        );

        Assertions.assertFalse(blocked.allowed());
        Assertions.assertTrue(blocked.reason().contains("当前最新消息"));

        CapabilityInvocationPolicy.Decision allowed = CapabilityInvocationPolicy.evaluate(
                new AgentContext().userMessage("帮我搜索MC百科作者Flechazo"),
                new AiCapability().name("search_mcmod"),
                "search_mcmod",
                Map.of("type", "author", "keyword", "Flechazo")
        );

        Assertions.assertTrue(allowed.allowed());
    }

    @Test
    void shouldBlockMutatingMcmodVoteWhenTargetIsOnlyFromHistory() {
        CapabilityInvocationPolicy.Decision blocked = CapabilityInvocationPolicy.evaluate(
                new AgentContext().userMessage("笑死我了"),
                new AiCapability().name("vote_mcmod").mutating(true),
                "vote_mcmod",
                Map.of("type", "mod", "keywordOrId", "水仙辞", "vote", "black")
        );

        Assertions.assertFalse(blocked.allowed());
    }

    @Test
    void shouldAllowTimerCreationOnlyWithCurrentReminderContent() {
        CapabilityInvocationPolicy.Decision blocked = CapabilityInvocationPolicy.evaluate(
                new AgentContext().userMessage("私聊吗"),
                new AiCapability().name("create_timer").mutating(true),
                "create_timer",
                Map.of("cron", "0 0 12 25 5 ? 2026", "message", "吃饭", "target", "private")
        );

        Assertions.assertFalse(blocked.allowed());

        CapabilityInvocationPolicy.Decision allowed = CapabilityInvocationPolicy.evaluate(
                new AgentContext().userMessage("半个小时后提醒我吃饭"),
                new AiCapability().name("create_timer").mutating(true),
                "create_timer",
                Map.of("cron", "0 0 12 25 5 ? 2026", "message", "吃饭", "target", "private")
        );

        Assertions.assertTrue(allowed.allowed());
    }

    @Test
    void shouldNotAllowConfirmationFromOldParametersWithoutCurrentConfirmText() {
        CapabilityInvocationPolicy.Decision blocked = CapabilityInvocationPolicy.evaluate(
                new AgentContext().userMessage("那算了"),
                new AiCapability().name("execute_rcon").mutating(true),
                "execute_rcon",
                Map.of("server", "1", "command", "stop", "confirm", "true")
        );

        Assertions.assertFalse(blocked.allowed());
    }

    @Test
    void shouldNotLetBareConfirmationBypassCurrentTargetCheck() {
        CapabilityInvocationPolicy.Decision blocked = CapabilityInvocationPolicy.evaluate(
                new AgentContext().userMessage("确认"),
                new AiCapability().name("execute_rcon").mutating(true),
                "execute_rcon",
                Map.of("server", "1", "command", "stop", "confirm", "true")
        );

        Assertions.assertFalse(blocked.allowed());
        Assertions.assertTrue(blocked.reason().contains("RCON"));
    }

    @Test
    void shouldAllowWeatherOnlyWhenCurrentMessageExists() {
        CapabilityInvocationPolicy.Decision blocked = CapabilityInvocationPolicy.evaluate(
                new AgentContext().userMessage(""),
                new AiCapability().name("get_weather"),
                "get_weather",
                Map.of("location", "成都")
        );
        CapabilityInvocationPolicy.Decision allowed = CapabilityInvocationPolicy.evaluate(
                new AgentContext().userMessage("白茶酱帮我看看成都今天的天气"),
                new AiCapability().name("get_weather"),
                "get_weather",
                Map.of("location", "成都")
        );

        Assertions.assertFalse(blocked.allowed());
        Assertions.assertTrue(allowed.allowed());
    }

    @Test
    void shouldBlockWebSearchForGeneralAdviceWithoutExplicitSearchIntent() {
        CapabilityInvocationPolicy.Decision blocked = CapabilityInvocationPolicy.evaluate(
                new AgentContext().userMessage("有没有好用的电脑拼音输入法"),
                new AiCapability().name("web_search"),
                "web_search",
                Map.of("query", "电脑拼音输入法 推荐")
        );

        Assertions.assertFalse(blocked.allowed());
    }

    @Test
    void shouldAllowMuteWhenUserAffirmsWithShortConfirmation() {
        CapabilityInvocationPolicy.Decision allowed = CapabilityInvocationPolicy.evaluate(
                new AgentContext().userMessage("行"),
                new AiCapability().name("execute_kanri").mutating(true),
                "execute_kanri",
                Map.of("action", "mute", "args", "123456789 10", "confirm", "true")
        );

        Assertions.assertTrue(allowed.allowed());
    }

    @Test
    void shouldAllowMuteWhenUserDelegatesDuration() {
        CapabilityInvocationPolicy.Decision allowed = CapabilityInvocationPolicy.evaluate(
                new AgentContext().userMessage("你看着来吧"),
                new AiCapability().name("execute_kanri").mutating(true),
                "execute_kanri",
                Map.of("action", "mute", "args", "111111111 222222222 10", "confirm", "true")
        );

        Assertions.assertTrue(allowed.allowed());
    }

    @Test
    void shouldAllowMuteBatchIntentWithoutExplicitMuteWord() {
        CapabilityInvocationPolicy.Decision allowed = CapabilityInvocationPolicy.evaluate(
                new AgentContext().userMessage("那几个都禁了"),
                new AiCapability().name("execute_kanri").mutating(true),
                "execute_kanri",
                Map.of("action", "mute", "args", "111111111 222222222 10", "confirm", "true")
        );

        Assertions.assertTrue(allowed.allowed());
    }

    @Test
    void shouldAllowColloquialMuteHimWithNumericTargets() {
        CapabilityInvocationPolicy.Decision allowed = CapabilityInvocationPolicy.evaluate(
                new AgentContext().userMessage("@白茶酱 禁他1个小时，让他还嘚瑟"),
                new AiCapability().name("execute_kanri").mutating(true),
                "execute_kanri",
                Map.of("action", "mute", "args", "123456789 60", "confirm", "true")
        );

        Assertions.assertTrue(allowed.allowed());
    }

    @Test
    void shouldBlockColloquialMuteWithoutTargetsInArgs() {
        CapabilityInvocationPolicy.Decision blocked = CapabilityInvocationPolicy.evaluate(
                new AgentContext().userMessage("@白茶酱 禁他1个小时"),
                new AiCapability().name("execute_kanri").mutating(true),
                "execute_kanri",
                Map.of("action", "mute", "args", "60", "confirm", "true")
        );

        Assertions.assertFalse(blocked.allowed());
    }

    @Test
    void shouldAllowSelfMuteRequestViaLlmToolCall() {
        CapabilityInvocationPolicy.Decision allowed = CapabilityInvocationPolicy.evaluate(
                new AgentContext().userMessage("[CQ:at,qq=10000] 禁我10分钟").senderId(30000L),
                new AiCapability().name("execute_kanri").mutating(true),
                "execute_kanri",
                Map.of("action", "mute", "args", "30000 10", "confirm", "true")
        );

        Assertions.assertTrue(allowed.allowed());
    }

    @Test
    void shouldAllowStructuredSelfMuteWithoutKeywordMatching() {
        CapabilityInvocationPolicy.Decision allowed = CapabilityInvocationPolicy.evaluate(
                new AgentContext().userMessage("[CQ:at,qq=10000] 送我去小黑屋").senderId(30000L),
                new AiCapability().name("execute_kanri").mutating(true),
                "execute_kanri",
                Map.of("action", "mute", "args", "30000 10", "confirm", "true", "selfTarget", "true")
        );

        Assertions.assertTrue(allowed.allowed());
    }

    @Test
    void shouldBlockStructuredSelfMuteWhenTargetIsNotSender() {
        CapabilityInvocationPolicy.Decision blocked = CapabilityInvocationPolicy.evaluate(
                new AgentContext().userMessage("[CQ:at,qq=10000] 送我去小黑屋").senderId(30000L),
                new AiCapability().name("execute_kanri").mutating(true),
                "execute_kanri",
                Map.of("action", "mute", "args", "40000 10", "confirm", "true", "selfTarget", "true")
        );

        Assertions.assertFalse(blocked.allowed());
    }

    @Test
    void shouldBlockStructuredSelfUnmute() {
        CapabilityInvocationPolicy.Decision blocked = CapabilityInvocationPolicy.evaluate(
                new AgentContext().userMessage("[CQ:at,qq=10000] 放我出来").senderId(30000L),
                new AiCapability().name("execute_kanri").mutating(true),
                "execute_kanri",
                Map.of("action", "loud", "args", "30000", "confirm", "true", "selfTarget", "true")
        );

        Assertions.assertFalse(blocked.allowed());
    }

    @Test
    void shouldBlockSelfMuteTargetingBotAccount() {
        AgentContext ctx = new AgentContext()
                .bot(Mockito.mock(xin.vanilla.banira.plugin.common.BaniraBot.class, invocation -> {
                    if ("getSelfId".equals(invocation.getMethod().getName())) {
                        return 10000L;
                    }
                    return null;
                }))
                .senderId(30000L)
                .userMessage("[CQ:at,qq=10000] 禁我10分钟");
        CapabilityInvocationPolicy.Decision blocked = CapabilityInvocationPolicy.evaluate(
                ctx,
                new AiCapability().name("execute_kanri").mutating(true),
                "execute_kanri",
                Map.of("action", "mute", "args", "10000 10", "confirm", "true")
        );

        Assertions.assertFalse(blocked.allowed());
        Assertions.assertTrue(blocked.reason().contains("机器人"));
    }

    @Test
    void shouldBlockRepeatMuteWhenKanriAlreadySucceeded() {
        CapabilityInvocationPolicy.Decision blocked = CapabilityInvocationPolicy.evaluate(
                new AgentContext().userMessage("@白茶酱 禁我10分钟").kanriMuteSucceeded(true),
                new AiCapability().name("execute_kanri").mutating(true),
                "execute_kanri",
                Map.of("action", "mute", "args", "30000 10", "confirm", "true")
        );

        Assertions.assertFalse(blocked.allowed());
    }

    @Test
    void shouldAllowWebSearchForExplicitPublicFactQuery() {
        CapabilityInvocationPolicy.Decision allowed = CapabilityInvocationPolicy.evaluate(
                new AgentContext().userMessage("帮我查李依然 李いせん的学历"),
                new AiCapability().name("web_search"),
                "web_search",
                Map.of("query", "李依然 李いせん 学历")
        );

        Assertions.assertTrue(allowed.allowed());
    }

    @Test
    void shouldAllowWebSearchForShortFollowUpSearchCommand() {
        CapabilityInvocationPolicy.Decision allowed = CapabilityInvocationPolicy.evaluate(
                new AgentContext().userMessage("@香草白茶 你搜搜"),
                new AiCapability().name("web_search"),
                "web_search",
                Map.of("query", "韬定律")
        );

        Assertions.assertTrue(allowed.allowed());
    }

    @Test
    void shouldStillBlockWebSearchStaleQueryWithoutSearchFollowUp() {
        CapabilityInvocationPolicy.Decision blocked = CapabilityInvocationPolicy.evaluate(
                new AgentContext().userMessage("笑死我了"),
                new AiCapability().name("web_search"),
                "web_search",
                Map.of("query", "韬定律")
        );

        Assertions.assertFalse(blocked.allowed());
    }

    @Test
    void shouldBlockWebSearchForResolvedResultChatter() {
        CapabilityInvocationPolicy.Decision blocked = CapabilityInvocationPolicy.evaluate(
                new AgentContext().userMessage("白茶酱终于找到答案啦，现在知道了韬定律是什么"),
                new AiCapability().name("web_search"),
                "web_search",
                Map.of("query", "韬定律")
        );

        Assertions.assertFalse(blocked.allowed());
    }
}
