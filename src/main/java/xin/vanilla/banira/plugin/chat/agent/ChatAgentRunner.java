package xin.vanilla.banira.plugin.chat.agent;

import dev.langchain4j.agent.tool.ToolExecutionRequest;
import dev.langchain4j.agent.tool.ToolSpecification;
import dev.langchain4j.data.message.AiMessage;
import dev.langchain4j.data.message.ChatMessage;
import dev.langchain4j.data.message.ToolExecutionResultMessage;
import dev.langchain4j.data.message.UserMessage;
import dev.langchain4j.model.chat.request.ChatRequest;
import dev.langchain4j.model.chat.response.ChatResponse;
import dev.langchain4j.service.tool.AiServiceTool;
import dev.langchain4j.service.tool.ToolExecutor;
import dev.langchain4j.service.tool.ToolService;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import lombok.extern.slf4j.Slf4j;
import xin.vanilla.banira.plugin.chat.AiTextLimits;
import xin.vanilla.banira.plugin.chat.ChatSafetyRejectionTracker;
import xin.vanilla.banira.plugin.chat.StructuredReplyPipeline;
import xin.vanilla.banira.plugin.chat.capability.AiDirectResult;
import xin.vanilla.banira.plugin.chat.capability.AiToolBridge;
import xin.vanilla.banira.plugin.chat.model.ChatModelRouter;
import xin.vanilla.banira.util.StringUtils;

import java.util.*;

/**
 * LangChain4j Agent 工具调用循环
 */
@Slf4j
public class ChatAgentRunner {

    private static final int MIN_REFERENCE_LENGTH = 180;
    private static final int MAX_HISTORY_TOOL_CALLS = 3;
    private static final int MAX_PROVIDER_REJECTION_RECOVERY_ATTEMPTS = 2;
    private static final Set<String> ALWAYS_REFERENCE_TOOLS = Set.of();
    private static final Set<String> HISTORY_TOOLS = Set.of(
            "getMessageById",
            "getRecentChatHistory",
            "searchChatHistory",
            "loadMessageImages"
    );
    private static final Set<String> NON_REFERENCE_TOOLS = Set.of(
            "listCapabilities",
            "listKanriActions",
            "addForwardReference",
            "uploadCodeTextFile",
            "resolveFollowUpSearchTarget",
            "listModelEndpoints",
            "checkModelEndpointQuota",
            "getGroupOwner",
            "getGroupSummary",
            "getWeather",
            "getMessageById",
            "getRecentChatHistory",
            "searchChatHistory",
            "loadMessageImages",
            "saveMemory",
            "recallLastAiReply",
            "muteSelf",
            "muteAllGroup",
            "unmuteAllGroup",
            "muteGroupMember",
            "muteGroupMembers",
            "searchGroupMember",
            "unmuteGroupMember",
            "executeKanriAction",
            "executeRcon",
            "voteMcmod",
            "pushMcmod",
            "commentMcmod",
            "replyMcmodComment",
            "deleteMcmodComment"
    );

    private ChatAgentRunner() {
    }

    @Nonnull
    public static AgentRunResult run(@Nonnull ChatModelRouter router
            , @Nonnull List<ChatMessage> messages
            , @Nonnull Object toolSource
            , int maxIterations
            , @Nonnull List<String> toolReferences
    ) {
        List<AiServiceTool> aiTools = ToolService.findTools(toolSource);
        List<ToolSpecification> toolSpecifications = aiTools.stream()
                .map(AiServiceTool::toolSpecification)
                .toList();
        Map<String, ToolExecutor> executors = new HashMap<>();
        for (AiServiceTool tool : aiTools) {
            executors.put(tool.toolSpecification().name(), tool.toolExecutor());
        }

        List<ChatMessage> conversation = new ArrayList<>(messages);
        int iterations = Math.max(1, maxIterations);
        Map<String, String> toolResultCache = new HashMap<>();
        int historyToolCalls = 0;
        int providerRejectionRecoveries = 0;
        for (int i = 0; i < iterations; i++) {
            LOGGER.debug("AI agent iteration {}/{} messages={} tools={}",
                    i + 1, iterations, conversation.size(), toolSpecifications.size());
            ChatResponse response = router.chat(ChatRequest.builder()
                    .messages(conversation)
                    .toolSpecifications(toolSpecifications)
                    .build());
            AiMessage aiMessage = response.aiMessage();
            conversation.add(aiMessage);
            if (ChatSafetyRejectionTracker.looksLikeProviderSafetyText(aiMessage.text())) {
                if (providerRejectionRecoveries++ < MAX_PROVIDER_REJECTION_RECOVERY_ATTEMPTS) {
                    LOGGER.debug("AI agent provider rejection text detected, requesting recovery attempt={}",
                            providerRejectionRecoveries);
                    conversation.add(UserMessage.from(providerRejectionRecoveryInstruction()));
                    continue;
                }
                LOGGER.warn("AI agent provider rejection recovery exhausted");
                return new AgentRunResult("", List.copyOf(toolReferences));
            }
            if (!aiMessage.hasToolExecutionRequests()) {
                LOGGER.debug("AI agent final text chars={}", safeText(aiMessage.text()).length());
                return new AgentRunResult(safeText(aiMessage.text()), List.copyOf(toolReferences));
            }
            for (ToolExecutionRequest request : aiMessage.toolExecutionRequests()) {
                LOGGER.debug("AI agent tool request name={} argsChars={}",
                        request.name(), request.arguments() != null ? request.arguments().length() : 0);
                ToolGateResult gateResult = gateToolRequest(request, toolResultCache, historyToolCalls);
                String result;
                if (gateResult.blocked()) {
                    LOGGER.debug("AI agent tool request blocked name={} reason={}", request.name(), gateResult.result());
                    result = gateResult.result();
                } else {
                    if (HISTORY_TOOLS.contains(request.name())) {
                        historyToolCalls++;
                    }
                    result = executeTool(executors, request, toolReferences);
                    toolResultCache.put(toolSignature(request), result);
                }
                if (AiDirectResult.isDirect(result)) {
                    LOGGER.debug("AI agent tool handled directly name={}", request.name());
                    return new AgentRunResult("", List.copyOf(toolReferences), true);
                }
                conversation.add(ToolExecutionResultMessage.toolExecutionResultMessage(request, result));
                if (toolSource instanceof AiToolBridge bridge) {
                    List<ChatMessage> mediaMessages = bridge.drainPendingMediaMessages();
                    if (!mediaMessages.isEmpty()) {
                        conversation.addAll(mediaMessages);
                        LOGGER.debug("AI agent appended media messages from tool name={} count={}",
                                request.name(), mediaMessages.size());
                    }
                }
            }
        }
        ChatMessage last = conversation.getLast();
        if (last instanceof AiMessage aiMessage) {
            String text = safeText(aiMessage.text());
            if (StringUtils.isNotNullOrEmpty(text)) {
                return new AgentRunResult(text, List.copyOf(toolReferences));
            }
        }
        String finalText = finalTextWithoutTools(router, conversation);
        if (StringUtils.isNotNullOrEmpty(finalText)) {
            if (ChatSafetyRejectionTracker.looksLikeProviderSafetyText(finalText)) {
                LOGGER.warn("AI agent final text without tools is provider rejection, suppressing");
                return new AgentRunResult("", List.copyOf(toolReferences));
            }
            return new AgentRunResult(finalText, List.copyOf(toolReferences));
        }
        return new AgentRunResult("", List.copyOf(toolReferences));
    }

    @Nonnull
    private static ToolGateResult gateToolRequest(@Nonnull ToolExecutionRequest request
            , @Nonnull Map<String, String> toolResultCache
            , int historyToolCalls
    ) {
        String signature = toolSignature(request);
        if (toolResultCache.containsKey(signature)) {
            return ToolGateResult.block("这组工具参数本轮已经查过了，直接根据刚才的结果继续，不要重复调用同一个工具。");
        }
        if (HISTORY_TOOLS.contains(request.name()) && historyToolCalls >= MAX_HISTORY_TOOL_CALLS) {
            return ToolGateResult.block("消息记录本轮已经查够了，直接根据已有上下文继续；如果需要外部资料，调用 webSearch；如果信息不足，就简短说明没查到。");
        }
        return ToolGateResult.allow();
    }

    @Nonnull
    private static String toolSignature(@Nonnull ToolExecutionRequest request) {
        return StringUtils.nullToEmpty(request.name()).trim()
                + '\n'
                + StringUtils.nullToEmpty(request.arguments()).trim();
    }

    @Nonnull
    private static String finalTextWithoutTools(@Nonnull ChatModelRouter router, @Nonnull List<ChatMessage> conversation) {
        try {
            LOGGER.debug("AI agent exhausted tool iterations, requesting final text without tools messages={}", conversation.size());
            ChatResponse response = router.chat(ChatRequest.builder()
                    .messages(conversation)
                    .build());
            return safeText(response.aiMessage().text());
        } catch (Exception e) {
            LOGGER.warn("AI agent final text request failed after tool exhaustion: {}", e.getMessage());
            return "";
        }
    }

    @Nonnull
    private static String executeTool(@Nonnull Map<String, ToolExecutor> executors
            , @Nonnull ToolExecutionRequest request
            , @Nonnull List<String> toolReferences
    ) {
        ToolExecutor executor = executors.get(request.name());
        if (executor == null) {
            return "未知工具：" + request.name();
        }
        try {
            String result = AiTextLimits.truncate(executor.execute(request, null), AiTextLimits.MAX_TOOL_RESULT);
            if (ChatSafetyRejectionTracker.looksLikeProviderSafetyText(result)) {
                LOGGER.warn("AI agent tool returned provider rejection name={}", request.name());
                return toolSafetyRecoveryResult();
            }
            maybeCollectReference(request.name(), result, toolReferences);
            LOGGER.debug("AI agent tool result name={} chars={}",
                    request.name(), result != null ? result.length() : 0);
            return result;
        } catch (Exception e) {
            LOGGER.warn("Tool execution failed: {}", request.name(), e);
            if (ChatSafetyRejectionTracker.looksLikeProviderSafetyRejection(e)) {
                return toolSafetyRecoveryResult();
            }
            return "工具执行失败：" + e.getMessage();
        }
    }

    @Nonnull
    private static String providerRejectionRecoveryInstruction() {
        return """
                上一轮输出是模型服务的拒绝或风险提示，不能作为群聊回复。
                请重新生成最终群聊台词：
                - 不复述错误原文
                - 不提模型、平台、审核、安全策略、高风险、系统
                - 不展开敏感话题内容
                - 根据当前语境，用一句简短自然的话拒绝、打住或换话题
                - 如果需要参与度元数据，照常在最后保留 [ENGAGE reply=yes|interest=...]
                """;
    }

    @Nonnull
    private static String toolSafetyRecoveryResult() {
        return """
                工具或模型服务拒绝了这部分内容。
                这只是内部状态，不能原样告诉群友。
                请停止展开敏感内容，改为生成一句自然、简短的拒绝或换话题回复。
                """;
    }

    private static void maybeCollectReference(@Nonnull String toolName, @Nullable String result, @Nonnull List<String> toolReferences) {
        if (StringUtils.isNullOrEmptyEx(result)) {
            return;
        }
        if (StructuredReplyPipeline.isUnsuitableForwardReferenceText(result)) {
            LOGGER.debug("AI agent skipped unsuitable reference from tool name={}", toolName);
            return;
        }
        if (NON_REFERENCE_TOOLS.contains(toolName)) {
            return;
        }
        if (!ALWAYS_REFERENCE_TOOLS.contains(toolName) && result.length() < MIN_REFERENCE_LENGTH) {
            return;
        }
        toolReferences.add(result.trim());
    }

    @Nonnull
    private static String safeText(String text) {
        return StringUtils.isNotNullOrEmpty(text) ? text.trim() : "";
    }

    private record ToolGateResult(boolean blocked, String result) {
        private static ToolGateResult allow() {
            return new ToolGateResult(false, "");
        }

        private static ToolGateResult block(String result) {
            return new ToolGateResult(true, result);
        }
    }

}
