package xin.vanilla.banira.plugin.chat;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import xin.vanilla.banira.config.entity.extended.ChatReplySettings;

import java.util.List;

class StructuredReplyPipelineTest {

    @Test
    void shouldMoveLongCodeBlockIntoForwardReference() {
        ChatReplySettings settings = new ChatReplySettings()
                .maxForwardLength(220)
                .maxCharsPerPart(80);
        String raw = """
                来了，DFS 标准模板：
                ```java
                import java.util.*;

                public class DfsTemplate {
                    static List<Integer>[] graph;
                    static boolean[] visited;

                    static void dfs(int u) {
                        visited[u] = true;
                        for (int v : graph[u]) {
                            if (!visited[v]) {
                                dfs(v);
                            }
                        }
                    }
                }
                ```
                """;

        StructuredReply reply = StructuredReplyPipeline.parseAndProcess(raw, List.of(), settings);

        Assertions.assertEquals("来了，DFS 标准模板：", reply.speech());
        Assertions.assertEquals(1, reply.references().size());
        Assertions.assertTrue(reply.references().getFirst().contains("public class DfsTemplate"));
        Assertions.assertFalse(reply.references().getFirst().contains("```"));
    }

    @Test
    void shouldMoveVeryLongPlainTextIntoForwardReference() {
        ChatReplySettings settings = new ChatReplySettings().maxForwardLength(120);
        String raw = "整理好了：\n" + "第一点内容\n".repeat(30);

        StructuredReply reply = StructuredReplyPipeline.parseAndProcess(raw, List.of(), settings);

        Assertions.assertEquals("整理好了：", reply.speech());
        Assertions.assertEquals(1, reply.references().size());
        Assertions.assertTrue(reply.references().getFirst().contains("第一点内容"));
    }

    @Test
    void shouldCleanMarkdownFenceFromExplicitReference() {
        String cleaned = StructuredReplyPipeline.cleanReferenceText("""
                ```go
                package main
                ```
                """);

        Assertions.assertEquals("package main", cleaned);
    }

    @Test
    void shouldStripGenericForwardReferencePrefix() {
        Assertions.assertEquals("韬定律可能是网络梗", StructuredReplyPipeline.cleanReferenceText("总结：韬定律可能是网络梗"));
        Assertions.assertEquals("第一条资料", StructuredReplyPipeline.cleanReferenceText("搜索结果：\n第一条资料"));
        Assertions.assertEquals("正文", StructuredReplyPipeline.cleanReferenceText("完整内容：正文"));
    }

    @Test
    void shouldParseBareReplyAndAtDirectives() {
        StructuredReply reply = StructuredReplyPipeline.parseAndProcess(
                "REPLY:834772792 AT:123456789 来了",
                List.of(),
                new ChatReplySettings()
        );

        Assertions.assertEquals("来了", reply.speech());
        Assertions.assertEquals(834772792, reply.replyToMessageId());
        Assertions.assertEquals(List.of(123456789L), reply.atTargets());
    }

    @Test
    void shouldConvertCqAtIntoStructuredMention() {
        StructuredReply reply = StructuredReplyPipeline.parseAndProcess(
                "[CQ:at,qq=123456789] 你看这个",
                List.of(),
                new ChatReplySettings()
        );

        Assertions.assertEquals("你看这个", reply.speech());
        Assertions.assertEquals(List.of(123456789L), reply.atTargets());
    }

    @Test
    void shouldStripAddresseeStageDirection() {
        StructuredReply reply = StructuredReplyPipeline.parseAndProcess(
                "（对高性能萝卜子）这个说法不太对\n(对月酱) 我看到了",
                List.of(),
                new ChatReplySettings()
        );

        Assertions.assertEquals("这个说法不太对\n我看到了", reply.speech());
    }

    @Test
    void shouldDropInternalPolicyReferenceBlocks() {
        StructuredReply reply = StructuredReplyPipeline.parseAndProcess(
                "这个我不太确定\n[REF]当前最新消息没有明确要求网页搜索，已阻止沿用旧搜索目标。[/REF]",
                List.of("工具执行失败：timeout"),
                new ChatReplySettings()
        );

        Assertions.assertEquals("这个我不太确定", reply.speech());
        Assertions.assertTrue(reply.references().isEmpty());
    }

    @Test
    void shouldDropUploadedCodeFileNoticeReferences() {
        StructuredReply reply = StructuredReplyPipeline.parseAndProcess(
                "done",
                List.of("代码文件\n\n已上传：TeleportMod.java\n内容较长，正文不要重复贴代码"),
                new ChatReplySettings()
        );

        Assertions.assertEquals("done", reply.speech());
        Assertions.assertTrue(reply.references().isEmpty());
    }

    @Test
    void shouldDropRawSearchReferenceBlocks() {
        StructuredReply reply = StructuredReplyPipeline.parseAndProcess(
                "我大概知道了\n[REF]查询：夏生先生 高性能萝卜子\n1. 亚托莉\n摘要：角色资料\n链接：https://example.com[/REF]",
                List.of(),
                new ChatReplySettings()
        );

        Assertions.assertEquals("我大概知道了", reply.speech());
        Assertions.assertTrue(reply.references().isEmpty());
    }

    @Test
    void shouldDropRawSearchReferenceAfterTitlePrefixCleanup() {
        StructuredReply reply = StructuredReplyPipeline.parseAndProcess(
                "看了下\n[REF]搜索结果：\n1. 韬定律\n摘要：某个说法\n链接：https://example.com[/REF]",
                List.of(),
                new ChatReplySettings()
        );

        Assertions.assertEquals("看了下", reply.speech());
        Assertions.assertTrue(reply.references().isEmpty());
    }

    @Test
    void shouldDropMessageHistoryReferenceBlocks() {
        StructuredReply reply = StructuredReplyPipeline.parseAndProcess(
                "没搜到明确来源\n[REF]消息记录只供理解上下文，不要原样转发：\nmsgId=1；发送者=稳定身份 qq=1；显示名可能被修改或冒用；内容=测试[/REF]",
                List.of(),
                new ChatReplySettings()
        );

        Assertions.assertEquals("没搜到明确来源", reply.speech());
        Assertions.assertTrue(reply.references().isEmpty());
    }
}
