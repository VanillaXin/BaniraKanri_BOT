package xin.vanilla.banira.plugin.chat.capability;

import dev.langchain4j.data.message.ImageContent;
import dev.langchain4j.data.message.UserMessage;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.enums.EnumMessageType;
import xin.vanilla.banira.plugin.chat.agent.AgentContext;
import xin.vanilla.banira.plugin.chat.memory.MemoryRetriever;
import xin.vanilla.banira.plugin.chat.model.ChatQuotaService;
import xin.vanilla.banira.service.IAiMemoryManager;
import xin.vanilla.banira.service.IMessageRecordManager;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Base64;

class AiToolBridgeTest {

    private static final byte[] ONE_PIXEL_PNG = Base64.getDecoder().decode(
            "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+/p9sAAAAASUVORK5CYII="
    );

    @Test
    void shouldBlockHistoryDrivenMcmodSearchWhenCurrentMessageChangedTopic() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        AiToolBridge bridge = bridge(registry, "帮我在模组水仙辞下方留言：好酷");

        String result = bridge.searchMcmod("author", "Flechazo");

        Assertions.assertTrue(result.contains("当前最新消息"));
        Mockito.verifyNoInteractions(registry);
    }

    @Test
    void shouldAllowMcmodSearchWhenCurrentMessageExplicitlyAsks() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        Mockito.when(registry.execute(Mockito.any(), Mockito.any(), Mockito.eq("search_mcmod"), Mockito.anyMap()))
                .thenReturn(AiDirectResult.sent("已发送。"));
        AiToolBridge bridge = bridge(registry, "帮我搜索MC百科作者Flechazo");

        String result = bridge.searchMcmod("author", "Flechazo");

        Assertions.assertTrue(AiDirectResult.isDirect(result));
        Mockito.verify(registry).execute(Mockito.any(), Mockito.any(), Mockito.eq("search_mcmod"), Mockito.anyMap());
    }

    @Test
    void shouldAllowMcmodVoteOnlyWhenCurrentMessageHasSameTarget() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        Mockito.when(registry.execute(Mockito.any(), Mockito.any(), Mockito.eq("vote_mcmod"), Mockito.anyMap()))
                .thenReturn(AiDirectResult.sent("已投票。"));
        AiToolBridge bridge = bridge(registry, "那就帮我给水仙辞点个黑票");

        String result = bridge.voteMcmod("mod", "水仙辞", "black");

        Assertions.assertTrue(AiDirectResult.isDirect(result));
        Mockito.verify(registry).execute(Mockito.any(), Mockito.any(), Mockito.eq("vote_mcmod"), Mockito.anyMap());
    }

    @Test
    void shouldBlockHistoryDrivenMcmodVoteWhenTargetIsFromOldTopic() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        AiToolBridge bridge = bridge(registry, "笑死我了");

        String result = bridge.voteMcmod("mod", "水仙辞", "black");

        Assertions.assertTrue(result.contains("当前最新消息"));
        Mockito.verifyNoInteractions(registry);
    }

    @Test
    void shouldAllowMcmodPushOnlyWhenCurrentMessageHasSameTarget() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        Mockito.when(registry.execute(Mockito.any(), Mockito.any(), Mockito.eq("push_mcmod"), Mockito.anyMap()))
                .thenReturn(AiDirectResult.sent("已推荐。"));
        AiToolBridge bridge = bridge(registry, "帮我给水仙辞点个推荐");

        String result = bridge.pushMcmod("mod", "水仙辞");

        Assertions.assertTrue(AiDirectResult.isDirect(result));
        Mockito.verify(registry).execute(Mockito.any(), Mockito.any(), Mockito.eq("push_mcmod"), Mockito.anyMap());
    }

    @Test
    void shouldAllowMcmodCommentOnlyWhenCurrentMessageContainsContent() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        Mockito.when(registry.execute(Mockito.any(), Mockito.any(), Mockito.eq("comment_mcmod"), Mockito.anyMap()))
                .thenReturn(AiDirectResult.sent("已留言。"));
        AiToolBridge bridge = bridge(registry, "帮我在模组水仙辞下方留言：好酷");

        String result = bridge.commentMcmod("mod", "水仙辞", "好酷");

        Assertions.assertTrue(AiDirectResult.isDirect(result));
        Mockito.verify(registry).execute(Mockito.any(), Mockito.any(), Mockito.eq("comment_mcmod"), Mockito.anyMap());
    }

    @Test
    void shouldAllowMcmodCommentReplyOnlyWhenCurrentMessageContainsContentAndId() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        Mockito.when(registry.execute(Mockito.any(), Mockito.any(), Mockito.eq("reply_mcmod_comment"), Mockito.anyMap()))
                .thenReturn(AiDirectResult.sent("已回复。"));
        AiToolBridge bridge = bridge(registry, "帮我回复评论123：收到");

        String result = bridge.replyMcmodComment("mod", "456", "123", "收到");

        Assertions.assertTrue(AiDirectResult.isDirect(result));
        Mockito.verify(registry).execute(Mockito.any(), Mockito.any(), Mockito.eq("reply_mcmod_comment"), Mockito.anyMap());
    }

    @Test
    void shouldBlockMcmodCommentDeleteWithoutCurrentId() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        AiToolBridge bridge = bridge(registry, "删掉刚刚那个评论");

        String result = bridge.deleteMcmodComment("123");

        Assertions.assertTrue(result.contains("当前最新消息"));
        Mockito.verifyNoInteractions(registry);
    }

    @Test
    void shouldBlockMuteWhenCurrentMessageDoesNotMentionTarget() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        AiToolBridge bridge = bridge(registry, "笑死我了");

        String result = bridge.muteGroupMember("123456", "10");

        Assertions.assertTrue(result.contains("当前最新消息"));
        Mockito.verifyNoInteractions(registry);
    }

    @Test
    void shouldAllowMuteWhenCurrentMessageMentionsTarget() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        Mockito.when(registry.execute(Mockito.any(), Mockito.any(), Mockito.eq("execute_kanri"), Mockito.anyMap()))
                .thenReturn("禁言 已执行。");
        AiToolBridge bridge = bridge(registry, "把[CQ:at,qq=123456]禁言10分钟");

        String result = bridge.muteGroupMember("123456", "10");

        Assertions.assertEquals("禁言 已执行。", result);
        Mockito.verify(registry).execute(Mockito.any(), Mockito.any(), Mockito.eq("execute_kanri"), Mockito.anyMap());
    }

    @Test
    void shouldBlockGenericMutatingCapabilityWithoutCurrentIntent() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        AiCapability capability = new AiCapability().name("execute_rcon").mutating(true);
        Mockito.when(registry.resolve(Mockito.anyLong(), Mockito.eq("execute_rcon"))).thenReturn(capability);
        AiToolBridge bridge = bridge(registry, "笑死我了");

        String result = bridge.invokeCapability("execute_rcon", "server=1,command=stop,confirm=true");

        Assertions.assertTrue(result.contains("已阻止"));
        Mockito.verify(registry, Mockito.never()).execute(Mockito.any(), Mockito.any(), Mockito.eq("execute_rcon"), Mockito.anyMap());
    }

    @Test
    void shouldBlockWebSearchWhenCurrentMessageChangedTopic() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        AiToolBridge bridge = bridge(registry, "笑死我了");

        String result = bridge.webSearch("李依然 李いせん 学历");

        Assertions.assertTrue(result.contains("网页搜索"));
        Mockito.verifyNoInteractions(registry);
    }

    @Test
    void shouldAllowWebSearchWhenCurrentMessageContainsTarget() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        Mockito.when(registry.execute(Mockito.any(), Mockito.any(), Mockito.eq("web_search"), Mockito.anyMap()))
                .thenReturn("查询：李依然 李いせん 学历");
        AiToolBridge bridge = bridge(registry, "帮我查李依然 李いせん的学历");

        String result = bridge.webSearch("李依然 李いせん 学历");

        Assertions.assertTrue(result.contains("查询"));
        Mockito.verify(registry).execute(Mockito.any(), Mockito.any(), Mockito.eq("web_search"), Mockito.anyMap());
    }

    @Test
    void shouldAllowGenericMutatingCapabilityOnExplicitConfirmation() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        AiCapability capability = new AiCapability().name("execute_rcon").mutating(true);
        Mockito.when(registry.resolve(Mockito.anyLong(), Mockito.eq("execute_rcon"))).thenReturn(capability);
        Mockito.when(registry.execute(Mockito.any(), Mockito.any(), Mockito.eq("execute_rcon"), Mockito.anyMap()))
                .thenReturn("已执行");
        AiToolBridge bridge = bridge(registry, "确认执行");

        String result = bridge.invokeCapability("execute_rcon", "server=1,command=stop,confirm=true");

        Assertions.assertEquals("已执行", result);
        Mockito.verify(registry).execute(Mockito.any(), Mockito.any(), Mockito.eq("execute_rcon"), Mockito.anyMap());
    }

    @Test
    void shouldExecutePendingActionOnSameUserConfirmation() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        AiCapability capability = new AiCapability()
                .name("execute_rcon")
                .mutating(true)
                .requireConfirmation(true);
        Mockito.when(registry.resolve(Mockito.anyLong(), Mockito.eq("execute_rcon"))).thenReturn(capability);
        Mockito.when(registry.execute(Mockito.any(), Mockito.any(), Mockito.eq("execute_rcon"), Mockito.anyMap()))
                .thenReturn("已执行");

        AiToolBridge first = bridge(registry, context("执行 RCON 服务器1命令 stop"));
        String pending = first.executeRcon("1", "stop", "false");
        Assertions.assertTrue(pending.contains("确认"));

        AiToolBridge second = bridge(registry, context("确认"));
        String result = second.executeRcon("old", "old", "true");

        Assertions.assertEquals("已执行", result);
        Mockito.verify(registry).execute(Mockito.any(), Mockito.any(), Mockito.eq("execute_rcon"), Mockito.argThat(map ->
                "1".equals(map.get("server")) && "stop".equals(map.get("command")) && "true".equals(map.get("confirm"))));
    }

    @Test
    void shouldBlockTimerCreationWhenCurrentMessageIsOnlyFollowUpQuestion() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        AiToolBridge bridge = bridge(registry, "私聊吗");

        String result = bridge.createTimer("0 0 12 25 5 ? 2026", "吃饭", "private");

        Assertions.assertTrue(result.contains("已阻止"));
        Mockito.verifyNoInteractions(registry);
    }

    @Test
    void shouldAllowTimerCreationWhenCurrentMessageExplicitlyRequestsReminder() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        Mockito.when(registry.execute(Mockito.any(), Mockito.any(), Mockito.eq("create_timer"), Mockito.anyMap()))
                .thenReturn("好，12:00 私聊提醒你：记得吃饭");
        AiToolBridge bridge = bridge(registry, "半个小时后提醒我吃饭");

        String result = bridge.createTimer("0 0 12 25 5 ? 2026", "吃饭", "private");

        Assertions.assertTrue(result.contains("私聊提醒"));
        Mockito.verify(registry).execute(Mockito.any(), Mockito.any(), Mockito.eq("create_timer"), Mockito.anyMap());
    }

    @Test
    void shouldAllowTimerCreationForMentionedUser() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        Mockito.when(registry.execute(Mockito.any(), Mockito.any(), Mockito.eq("create_timer"), Mockito.anyMap()))
                .thenReturn("好，12:00 在群里 @ 某人：该起床了");
        AiToolBridge bridge = bridge(registry, "俩分钟后提醒[CQ:at,qq=123456789] 起床");

        String result = bridge.createTimerForUser("0 0 12 25 5 ? 2026", "起床", "123456789");

        Assertions.assertTrue(result.contains("在群里"));
        Mockito.verify(registry).execute(Mockito.any(), Mockito.any(), Mockito.eq("create_timer"), Mockito.argThat(map ->
                "123456789".equals(map.get("targetUser")) && "group".equals(map.get("target"))));
    }

    @Test
    void shouldAddForwardReferenceForLongContent() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        java.util.List<String> references = new java.util.ArrayList<>();
        AiToolBridge bridge = new AiToolBridge(
                new AgentContext().userMessage("帮我写个 DFS 模板"),
                new ChatConfig(),
                registry,
                Mockito.mock(MemoryRetriever.class),
                Mockito.mock(IAiMemoryManager.class),
                Mockito.mock(ChatQuotaService.class),
                3,
                references
        );

        String result = bridge.addForwardReference("DFS 标准模板", "```java\nclass Demo {}\n```");

        Assertions.assertTrue(result.contains("合并转发"));
        Assertions.assertEquals(1, references.size());
        Assertions.assertTrue(references.getFirst().contains("class Demo"));
    }

    @Test
    void shouldUploadLongHtmlReferenceAsFileInsteadOfForwardText() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        java.util.List<String> references = new java.util.ArrayList<>();
        AgentContext ctx = context("@bot build this page as html");
        AiToolBridge bridge = new AiToolBridge(
                ctx,
                new ChatConfig(),
                registry,
                Mockito.mock(MemoryRetriever.class),
                Mockito.mock(IAiMemoryManager.class),
                Mockito.mock(ChatQuotaService.class),
                3,
                references
        );
        String html = """
                <!DOCTYPE html>
                <html>
                <head>
                    <meta charset="UTF-8">
                    <style>
                    body { margin: 0; font-family: sans-serif; background: #111; color: #eee; }
                    .panel { display: grid; grid-template-columns: 1fr 1fr; gap: 12px; padding: 24px; }
                    .card { border: 1px solid #333; border-radius: 6px; padding: 16px; }
                    </style>
                </head>
                <body>
                    <main class="panel">
                        <section class="card">token input</section>
                        <section class="card">quota output</section>
                    </main>
                    <script>
                    const items = Array.from({ length: 20 }, (_, i) => i);
                    function render() { return items.map(i => `<div>${i}</div>`).join(""); }
                    </script>
                </body>
                </html>
                """ + "/* filler */\n".repeat(120);

        String result = bridge.addForwardReference("token panel html", html);

        Assertions.assertTrue(result.contains("代码文件"));
        Assertions.assertTrue(references.isEmpty());
        Mockito.verify(ctx.bot()).uploadGroupFile(
                Mockito.eq(20000L),
                Mockito.anyString(),
                Mockito.endsWith(".html"),
                Mockito.eq("")
        );
    }

    @Test
    void shouldNotAddInternalPolicyTextAsForwardReference() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        java.util.List<String> references = new java.util.ArrayList<>();
        AiToolBridge bridge = new AiToolBridge(
                new AgentContext().userMessage("你认识她吗"),
                new ChatConfig(),
                registry,
                Mockito.mock(MemoryRetriever.class),
                Mockito.mock(IAiMemoryManager.class),
                Mockito.mock(ChatQuotaService.class),
                3,
                references
        );

        String result = bridge.addForwardReference("完整内容", "当前最新消息没有明确要求网页搜索，已阻止沿用旧搜索目标。");

        Assertions.assertTrue(result.contains("已忽略"));
        Assertions.assertTrue(references.isEmpty());
    }

    @Test
    void shouldNotAddRawSearchResultAsForwardReference() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        java.util.List<String> references = new java.util.ArrayList<>();
        AiToolBridge bridge = new AiToolBridge(
                new AgentContext().userMessage("夏生先生是谁"),
                new ChatConfig(),
                registry,
                Mockito.mock(MemoryRetriever.class),
                Mockito.mock(IAiMemoryManager.class),
                Mockito.mock(ChatQuotaService.class),
                3,
                references
        );

        String result = bridge.addForwardReference(
                "完整内容",
                "查询：夏生先生 高性能萝卜子\n1. 亚托莉\n摘要：角色资料\n链接：https://example.com"
        );

        Assertions.assertTrue(result.contains("已忽略"));
        Assertions.assertTrue(references.isEmpty());
    }

    @Test
    void shouldRedirectSelfMuteTargetToSenderInsteadOfBotAt() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        Mockito.when(registry.execute(Mockito.any(), Mockito.any(), Mockito.eq("execute_kanri"), Mockito.anyMap()))
                .thenReturn("禁言 已执行。");
        AgentContext ctx = context("[CQ:at,qq=10000] 禁我10分钟");
        AiToolBridge bridge = bridge(registry, ctx);

        String result = bridge.muteGroupMember("[CQ:at,qq=10000]", "10");

        Assertions.assertEquals("禁言 已执行。", result);
        Mockito.verify(registry).execute(Mockito.any(), Mockito.any(), Mockito.eq("execute_kanri"), Mockito.argThat(map ->
                "30000 10".equals(map.get("args")) && "true".equals(map.get("confirm"))));
    }

    @Test
    void shouldConvertSelfMuteMonthDurationBeforeKanriExecution() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        Mockito.when(registry.execute(Mockito.any(), Mockito.any(), Mockito.eq("execute_kanri"), Mockito.anyMap()))
                .thenReturn("禁言 已执行。");
        AgentContext ctx = context("[CQ:at,qq=10000] 禁我一月");
        AiToolBridge bridge = bridge(registry, ctx);

        String result = bridge.muteGroupMember("[CQ:at,qq=10000]", "一月");

        Assertions.assertEquals("禁言 已执行。", result);
        Mockito.verify(registry).execute(Mockito.any(), Mockito.any(), Mockito.eq("execute_kanri"), Mockito.argThat(map ->
                "30000 43200".equals(map.get("args")) && "true".equals(map.get("confirm"))));
    }

    @Test
    void shouldNormalizeGenericSelfMuteKanriAction() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        Mockito.when(registry.execute(Mockito.any(), Mockito.any(), Mockito.eq("execute_kanri"), Mockito.anyMap()))
                .thenReturn("禁言 已执行。");
        AgentContext ctx = context("[CQ:at,qq=10000] 禁我一月");
        AiToolBridge bridge = bridge(registry, ctx);

        String result = bridge.executeKanriAction("mute", "一月", "false");

        Assertions.assertEquals("禁言 已执行。", result);
        Mockito.verify(registry).execute(Mockito.any(), Mockito.any(), Mockito.eq("execute_kanri"), Mockito.argThat(map ->
                "30000 43200".equals(map.get("args")) && "true".equals(map.get("confirm"))));
    }

    @Test
    void shouldUseStructuredSelfMuteToolWithoutPhraseMatching() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        Mockito.when(registry.execute(Mockito.any(), Mockito.any(), Mockito.eq("execute_kanri"), Mockito.anyMap()))
                .thenReturn("禁言 已执行。");
        AgentContext ctx = context("[CQ:at,qq=10000] 送我去小黑屋");
        AiToolBridge bridge = bridge(registry, ctx);

        String result = bridge.muteSelf("10分钟");

        Assertions.assertEquals("禁言 已执行。", result);
        Mockito.verify(registry).execute(Mockito.any(), Mockito.any(), Mockito.eq("execute_kanri"), Mockito.argThat(map ->
                "30000 10".equals(map.get("args"))
                        && "true".equals(map.get("confirm"))
                        && "true".equals(map.get("selfTarget"))));
    }

    @Test
    void shouldBlockRepeatMuteWhenKanriAlreadySucceeded() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        AgentContext ctx = context("@白茶酱 禁我10分钟").kanriMuteSucceeded(true);
        AiToolBridge bridge = bridge(registry, ctx);

        String result = bridge.muteGroupMember("30000", "10");

        Assertions.assertTrue(result.contains("本回合已由系统执行"));
        Mockito.verifyNoInteractions(registry);
    }

    @Test
    void shouldSetGroupCardForStructuredMentionTarget() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        Mockito.when(registry.execute(Mockito.any(), Mockito.any(), Mockito.eq("execute_kanri"), Mockito.anyMap()))
                .thenReturn("设置群名片已执行：洛裘。");
        AgentContext ctx = context("[CQ:at,qq=10000] 把[CQ:at,qq=123456] 的群名片改正常一点");
        AiToolBridge bridge = bridge(registry, ctx);

        String result = bridge.setGroupCard("[CQ:at,qq=123456]", "洛 裘");

        Assertions.assertEquals("设置群名片已执行：洛裘。", result);
        Mockito.verify(registry).execute(Mockito.any(), Mockito.any(), Mockito.eq("execute_kanri"), Mockito.argThat(map ->
                "card".equals(map.get("action"))
                        && "123456 洛裘".equals(map.get("args"))
                        && "true".equals(map.get("confirm"))));
    }

    @Test
    void shouldSetSenderGroupCardWhenUserSaysMyCardEvenIfBotMentioned() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        Mockito.when(registry.execute(Mockito.any(), Mockito.any(), Mockito.eq("execute_kanri"), Mockito.anyMap()))
                .thenReturn("设置群名片已执行：辉小月。");
        AgentContext ctx = context("[CQ:at,qq=10000] 你把我的群名片改成辉小月试试");
        AiToolBridge bridge = bridge(registry, ctx);

        String result = bridge.setGroupCard("[CQ:at,qq=10000]", "辉小月");

        Assertions.assertEquals("设置群名片已执行：辉小月。", result);
        Mockito.verify(registry).execute(Mockito.any(), Mockito.any(), Mockito.eq("execute_kanri"), Mockito.argThat(map ->
                "card".equals(map.get("action"))
                        && "30000 辉小月".equals(map.get("args"))
                        && "true".equals(map.get("confirm"))));
    }

    @Test
    void shouldNormalizeGenericCardActionForSenderWhenUserSaysMyCard() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        Mockito.when(registry.execute(Mockito.any(), Mockito.any(), Mockito.eq("execute_kanri"), Mockito.anyMap()))
                .thenReturn("设置群名片已执行：辉小月。");
        AgentContext ctx = context("[CQ:at,qq=10000] 你把我的群名片改成辉小月试试");
        AiToolBridge bridge = bridge(registry, ctx);

        String result = bridge.executeKanriAction("card", "10000 辉小月", "true");

        Assertions.assertEquals("设置群名片已执行：辉小月。", result);
        Mockito.verify(registry).execute(Mockito.any(), Mockito.any(), Mockito.eq("execute_kanri"), Mockito.argThat(map ->
                "card".equals(map.get("action"))
                        && "30000 辉小月".equals(map.get("args"))
                        && "true".equals(map.get("confirm"))));
    }

    @Test
    void shouldSetBotCardWhenUserExplicitlySaysYourOwnCardEvenWithMyCardInSameMessage() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        Mockito.when(registry.execute(Mockito.any(), Mockito.any(), Mockito.eq("execute_kanri"), Mockito.anyMap()))
                .thenReturn("设置群名片已执行：测试青茶。");
        AgentContext ctx = context("[CQ:at,qq=10000] 把你自己的群名片改为测试青茶，把我的群名片改为测试月");
        AiToolBridge bridge = bridge(registry, ctx);

        String result = bridge.setGroupCard("[CQ:at,qq=10000]", "测试青茶");

        Assertions.assertEquals("设置群名片已执行：测试青茶。", result);
        Mockito.verify(registry).execute(Mockito.any(), Mockito.any(), Mockito.eq("execute_kanri"), Mockito.argThat(map ->
                "card".equals(map.get("action"))
                        && "10000 测试青茶".equals(map.get("args"))
                        && "true".equals(map.get("confirm"))));
    }

    @Test
    void shouldSetSenderCardByCardValueWhenYourOwnCardAppearsInSameMessage() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        Mockito.when(registry.execute(Mockito.any(), Mockito.any(), Mockito.eq("execute_kanri"), Mockito.anyMap()))
                .thenReturn("设置群名片已执行：测试月。");
        AgentContext ctx = context("[CQ:at,qq=10000] 把你自己的群名片改为测试青茶，把我的群名片改为测试月");
        AiToolBridge bridge = bridge(registry, ctx);

        String result = bridge.setGroupCard("[CQ:at,qq=10000]", "测试月");

        Assertions.assertEquals("设置群名片已执行：测试月。", result);
        Mockito.verify(registry).execute(Mockito.any(), Mockito.any(), Mockito.eq("execute_kanri"), Mockito.argThat(map ->
                "card".equals(map.get("action"))
                        && "30000 测试月".equals(map.get("args"))
                        && "true".equals(map.get("confirm"))));
    }

    @Test
    void shouldNormalizeGenericCardActionForBotWhenUserSaysYourOwnCard() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        Mockito.when(registry.execute(Mockito.any(), Mockito.any(), Mockito.eq("execute_kanri"), Mockito.anyMap()))
                .thenReturn("设置群名片已执行：测试青茶。");
        AgentContext ctx = context("[CQ:at,qq=10000] 把你自己的群名片改为测试青茶，把我的群名片改为测试月");
        AiToolBridge bridge = bridge(registry, ctx);

        String result = bridge.executeKanriAction("card", "10000 测试青茶", "true");

        Assertions.assertEquals("设置群名片已执行：测试青茶。", result);
        Mockito.verify(registry).execute(Mockito.any(), Mockito.any(), Mockito.eq("execute_kanri"), Mockito.argThat(map ->
                "card".equals(map.get("action"))
                        && "10000 测试青茶".equals(map.get("args"))
                        && "true".equals(map.get("confirm"))));
    }

    @Test
    void shouldBlockBotGroupCardChangeFromWakeMentionWhenNotExplicitlyBotCard() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        AgentContext ctx = context("[CQ:at,qq=10000] 把群名片改成辉小月试试");
        AiToolBridge bridge = bridge(registry, ctx);

        String result = bridge.setGroupCard("[CQ:at,qq=10000]", "辉小月");

        Assertions.assertTrue(result.contains("缺少要修改群名片的目标"));
        Mockito.verifyNoInteractions(registry);
    }

    @Test
    void shouldBlockGroupCardChangeWithoutCurrentMentionTarget() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        AgentContext ctx = context("[CQ:at,qq=10000] 你随便想一个正常一点的就行");
        AiToolBridge bridge = bridge(registry, ctx);

        String result = bridge.setGroupCard("123456", "洛裘");

        Assertions.assertTrue(result.contains("当前最新消息"));
        Mockito.verifyNoInteractions(registry);
    }

    @Test
    void shouldReturnRecentHistoryWithStableQqIdentity() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        IMessageRecordManager recordManager = Mockito.mock(IMessageRecordManager.class);
        Mockito.when(recordManager.getMessageRecordList(Mockito.any()))
                .thenReturn(java.util.List.of(new MessageRecord()
                        .setMsgId("88")
                        .setGroupId(20000L)
                        .setSenderId(30000L)
                        .setMsgRecode("刚刚那句话")));
        AgentContext ctx = context("刚刚谁说了那句话");
        AiToolBridge bridge = new AiToolBridge(
                ctx,
                new ChatConfig(),
                registry,
                Mockito.mock(MemoryRetriever.class),
                Mockito.mock(IAiMemoryManager.class),
                Mockito.mock(ChatQuotaService.class),
                3,
                new java.util.ArrayList<>(),
                null,
                recordManager
        );

        String result = bridge.getRecentChatHistory("10");

        Assertions.assertTrue(result.contains("msgId=88"));
        Assertions.assertTrue(result.contains("qq=30000"));
        Assertions.assertTrue(result.contains("是否你自己发送=否"));
        Assertions.assertTrue(result.contains("刚刚那句话"));
    }

    @Test
    void shouldReadOwnOldMessageWhenOnlyRawTextWasRecorded() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        IMessageRecordManager recordManager = Mockito.mock(IMessageRecordManager.class);
        Mockito.when(recordManager.getGroupMessageRecord(20000L, 1622503723))
                .thenReturn(new MessageRecord()
                        .setMsgId("1622503723")
                        .setGroupId(20000L)
                        .setSenderId(10000L)
                        .setMsgRaw("确实不知道，我搜了也没搜到"));
        AgentContext ctx = context("[CQ:reply,id=1622503723][CQ:at,qq=10000] 你再搜搜看");
        AiToolBridge bridge = new AiToolBridge(
                ctx,
                new ChatConfig(),
                registry,
                Mockito.mock(MemoryRetriever.class),
                Mockito.mock(IAiMemoryManager.class),
                Mockito.mock(ChatQuotaService.class),
                3,
                new java.util.ArrayList<>(),
                null,
                recordManager
        );

        String result = bridge.getMessageById("1622503723");

        Assertions.assertTrue(result.contains("msgId=1622503723"));
        Assertions.assertTrue(result.contains("是否你自己发送=是"));
        Assertions.assertTrue(result.contains("确实不知道，我搜了也没搜到"));
    }

    @Test
    void shouldLoadMessageImagesOnDemand() throws Exception {
        Path image = Files.createTempFile("banira-tool-image", ".png");
        Files.write(image, ONE_PIXEL_PNG);
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        IMessageRecordManager recordManager = Mockito.mock(IMessageRecordManager.class);
        Mockito.when(recordManager.getGroupMessageRecord(20000L, 77))
                .thenReturn(new MessageRecord()
                        .setMsgId("77")
                        .setGroupId(20000L)
                        .setSenderId(40000L)
                        .setMsgRecode("[CQ:image,file=test.png,url=" + image.toUri() + "]"));
        AgentContext ctx = context("[CQ:at,qq=10000] 看看前面那张图");
        ChatConfig config = new ChatConfig();
        config.model().imageInputEnabled(true);
        AiToolBridge bridge = new AiToolBridge(
                ctx,
                config,
                registry,
                Mockito.mock(MemoryRetriever.class),
                Mockito.mock(IAiMemoryManager.class),
                Mockito.mock(ChatQuotaService.class),
                3,
                new java.util.ArrayList<>(),
                null,
                recordManager
        );

        String result = bridge.loadMessageImages("77");

        Assertions.assertTrue(result.contains("图片内容已加载"));
        java.util.List<dev.langchain4j.data.message.ChatMessage> media = bridge.drainPendingMediaMessages();
        Assertions.assertEquals(1, media.size());
        Assertions.assertTrue(media.getFirst() instanceof UserMessage);
        UserMessage message = (UserMessage) media.getFirst();
        Assertions.assertTrue(message.contents().stream().anyMatch(ImageContent.class::isInstance));
        Assertions.assertTrue(bridge.drainPendingMediaMessages().isEmpty());
    }

    @Test
    void shouldResolveFollowUpSearchTargetBeforeWebSearch() {
        AiCapabilityRegistry registry = Mockito.mock(AiCapabilityRegistry.class);
        Mockito.when(registry.execute(Mockito.any(), Mockito.any(), Mockito.eq("web_search"), Mockito.anyMap()))
                .thenReturn("网页搜索资料：韬定律相关内容");
        IMessageRecordManager recordManager = Mockito.mock(IMessageRecordManager.class);
        Mockito.when(recordManager.getGroupMessageRecord(20000L, 1622503723))
                .thenReturn(new MessageRecord()
                        .setId(100L)
                        .setMsgId("1622503723")
                        .setGroupId(20000L)
                        .setSenderId(10000L)
                        .setMsgRaw("确实不知道，我搜了也没搜到"));
        Mockito.when(recordManager.getMessageRecordList(Mockito.any()))
                .thenReturn(java.util.List.of(
                        new MessageRecord()
                                .setId(99L)
                                .setMsgId("99")
                                .setGroupId(20000L)
                                .setSenderId(30000L)
                                .setMsgRecode("韬定律"),
                        new MessageRecord()
                                .setId(98L)
                                .setMsgId("98")
                                .setGroupId(20000L)
                                .setSenderId(40000L)
                                .setMsgRecode("高性能是这样的")
                ));
        AgentContext ctx = context("[CQ:reply,id=1622503723][CQ:at,qq=10000] 你再搜搜看");
        AiToolBridge bridge = new AiToolBridge(
                ctx,
                new ChatConfig(),
                registry,
                Mockito.mock(MemoryRetriever.class),
                Mockito.mock(IAiMemoryManager.class),
                Mockito.mock(ChatQuotaService.class),
                3,
                new java.util.ArrayList<>(),
                null,
                recordManager
        );

        String result = bridge.webSearch("确实不知道，我搜了也没搜到");

        Assertions.assertEquals("网页搜索资料：韬定律相关内容", result);
        Mockito.verify(registry).execute(Mockito.any(), Mockito.any(), Mockito.eq("web_search"), Mockito.argThat(map ->
                "韬定律".equals(map.get("query"))));
    }

    private static AiToolBridge bridge(AiCapabilityRegistry registry, String currentMessage) {
        return bridge(registry, new AgentContext().userMessage(currentMessage));
    }

    private static AiToolBridge bridge(AiCapabilityRegistry registry, AgentContext ctx) {
        return new AiToolBridge(
                ctx,
                new ChatConfig(),
                registry,
                Mockito.mock(MemoryRetriever.class),
                Mockito.mock(IAiMemoryManager.class),
                Mockito.mock(ChatQuotaService.class),
                3
        );
    }

    private static AgentContext context(String currentMessage) {
        xin.vanilla.banira.plugin.common.BaniraBot bot = Mockito.mock(xin.vanilla.banira.plugin.common.BaniraBot.class);
        Mockito.when(bot.getSelfId()).thenReturn(10000L);
        return new AgentContext()
                .bot(bot)
                .groupId(20000L)
                .senderId(30000L)
                .msgType(EnumMessageType.GROUP)
                .msgId("1")
                .userMessage(currentMessage);
    }
}
