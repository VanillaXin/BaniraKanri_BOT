package xin.vanilla.banira.plugin.chat;

import dev.langchain4j.data.message.ChatMessage;
import dev.langchain4j.data.message.SystemMessage;
import dev.langchain4j.data.message.UserMessage;
import dev.langchain4j.model.chat.response.ChatResponse;
import jakarta.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.enums.EnumMessageType;
import xin.vanilla.banira.plugin.chat.agent.AgentContext;
import xin.vanilla.banira.plugin.chat.agent.AgentRunResult;
import xin.vanilla.banira.plugin.chat.agent.ChatAgentRunner;
import xin.vanilla.banira.plugin.chat.capability.AiCapabilityRegistry;
import xin.vanilla.banira.plugin.chat.capability.AiToolBridge;
import xin.vanilla.banira.plugin.chat.capability.CapabilityInvocationPolicy;
import xin.vanilla.banira.plugin.chat.memory.MemoryEmbeddingService;
import xin.vanilla.banira.plugin.chat.memory.MemoryExtractor;
import xin.vanilla.banira.plugin.chat.memory.MemoryRetriever;
import xin.vanilla.banira.plugin.chat.model.ChatModelRouter;
import xin.vanilla.banira.plugin.chat.model.ChatQuotaService;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.kanri.KanriService;
import xin.vanilla.banira.service.IAiMemoryManager;
import xin.vanilla.banira.service.IMessageRecordManager;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.*;

@Slf4j
public class ChatTurnPipeline {

    private final ChatConfig cfg;
    private final ChatModelRouter modelRouter;
    private final ReplyDecisionMaker decisionMaker;
    private final AiCapabilityRegistry capabilityRegistry;
    private final MemoryRetriever memoryRetriever;
    private final MemoryExtractor memoryExtractor;
    private final MemoryEmbeddingService memoryEmbeddingService;
    private final IAiMemoryManager aiMemoryManager;
    private final ChatAffinityService affinityService;
    private final ChatQuotaService chatQuotaService;
    private final ChatGuardService guard;
    private final ChatHistoryProvider historyProvider;
    private final ChatPromptAssembler promptAssembler;
    private final ChatResponseSanitizer responseSanitizer;
    private final ChatEngagementGate engagementGate;
    private final ChatEngagementService engagementService;
    private final KanriService kanriService;
    private final IMessageRecordManager messageRecordManager;

    public ChatTurnPipeline(@Nonnull ChatConfig cfg
            , @Nonnull ChatModelRouter modelRouter
            , @Nonnull ReplyDecisionMaker decisionMaker
            , @Nonnull AiCapabilityRegistry capabilityRegistry
            , @Nonnull MemoryRetriever memoryRetriever
            , @Nonnull MemoryExtractor memoryExtractor
            , @Nonnull MemoryEmbeddingService memoryEmbeddingService
            , @Nonnull IAiMemoryManager aiMemoryManager
            , @Nonnull ChatAffinityService affinityService
            , @Nonnull ChatQuotaService chatQuotaService
            , @Nonnull ChatGuardService guard
            , @Nonnull ChatHistoryProvider historyProvider
            , @Nonnull ChatPromptAssembler promptAssembler
            , @Nonnull ChatResponseSanitizer responseSanitizer
            , @Nonnull ChatEngagementGate engagementGate
            , @Nonnull ChatEngagementService engagementService
            , @Nonnull KanriService kanriService
            , @Nonnull IMessageRecordManager messageRecordManager
    ) {
        this.cfg = cfg;
        this.modelRouter = modelRouter;
        this.decisionMaker = decisionMaker;
        this.capabilityRegistry = capabilityRegistry;
        this.memoryRetriever = memoryRetriever;
        this.memoryExtractor = memoryExtractor;
        this.memoryEmbeddingService = memoryEmbeddingService;
        this.aiMemoryManager = aiMemoryManager;
        this.affinityService = affinityService;
        this.chatQuotaService = chatQuotaService;
        this.guard = guard;
        this.historyProvider = historyProvider;
        this.promptAssembler = promptAssembler;
        this.responseSanitizer = responseSanitizer;
        this.engagementGate = engagementGate;
        this.engagementService = engagementService;
        this.kanriService = kanriService;
        this.messageRecordManager = messageRecordManager;
    }

    public StructuredReply generateReply(@Nonnull BaniraBot bot
            , @Nonnull BaniraCodeContext ctx
            , @Nonnull ChatTurnCoalesceState coalesceState
    ) {
        if (!cfg.enabled()) {
            return null;
        }
        TurnAddressing addressing = resolveAddressing(bot, ctx);
        AgentContext agentContext = AgentContext.from(bot, ctx);
        boolean engagementMode = cfg.engagement().enabled();
        ChatGuardService.GuardDecision guardDecision = guard.preCheck(
                ctx.msg(),
                addressing.directlyAddressed(),
                BaniraUtils.getBotNick(),
                engagementMode
        );
        if (guardDecision.handled()) {
            LOGGER.debug("AI guard handled group={} sender={} respond={}", ctx.group(), ctx.sender(), guardDecision.respond());
            return guardDecision.respond() ? new StructuredReply(guardDecision.replyText(), List.of()) : null;
        }
        StructuredReply directWeather = tryHandleDirectWeather(agentContext, ctx, addressing);
        if (directWeather != null) {
            return directWeather;
        }
        if (!modelRouter.isReady()) {
            LOGGER.debug("skip reply: no LLM endpoint configured");
            return null;
        }
        int affinityScore = affinityService.scoreAfterMessage(cfg, agentContext, ctx.msg());
        long botId = bot.getSelfId();
        if (!engagementGate.shouldInvokeModel(botId, ctx, addressing.directMentioned(), addressing.nameMentioned(),
                addressing.botNamePrefixMentioned(), affinityScore)) {
            engagementGate.onSkippedMessage(botId, ctx, addressing.directMentioned(), addressing.nameMentioned(),
                    addressing.botNamePrefixMentioned());
            return null;
        }
        long groupId = ctx.group() != null ? ctx.group() : 0L;
        int followInterest = engagementService.currentInterest(botId, groupId, cfg.engagement());
        LOGGER.debug("AI chat start group={} sender={} direct={} name={} prefix={} affinity={} agent={} engagement={} followInterest={}",
                ctx.group(), ctx.sender(), addressing.directMentioned(), addressing.nameMentioned(),
                addressing.botNamePrefixMentioned(), affinityScore, cfg.agent().enabled(), engagementMode, followInterest);

        List<MessageRecord> records = historyProvider.history(bot, ctx);
        if (cfg.agent().localKanriPreflight()) {
            KanriMuteFollowUpHandler.tryExecute(
                    kanriService, bot, ctx, agentContext, records, addressing.directMentioned());
        }
        if (engagementMode && ChatPreflightRunner.needsPreflight(ctx, addressing.directMentioned(),
                addressing.nameMentioned(), addressing.botNamePrefixMentioned(), cfg.engagement(), followInterest, records, botId)) {
            BaniraCodeContext preflightCtx = ctx.clone().msg(ChatInputSanitizer.sanitizeUserText(ctx.msg()));
            PreflightMetaParser.PreflightDecision preflight = ChatPreflightRunner.evaluate(
                    bot, preflightCtx, records, cfg, modelRouter, followInterest);
            if (!preflight.invoke()) {
                LOGGER.debug("preflight skipped main agent group={} sender={} interest={}",
                        ctx.group(), ctx.sender(), preflight.interest());
                return null;
            }
            engagementService.update(botId, groupId, preflight.interest(), cfg.engagement());
            followInterest = preflight.interest();
        }
        List<ChatMessage> prompt = promptAssembler.build(bot, ctx, agentContext, records, engagementMode, followInterest, coalesceState);
        if (prompt.isEmpty()) {
            return null;
        }
        LOGGER.debug("AI prompt prepared group={} sender={} messages={} history={}",
                ctx.group(), ctx.sender(), prompt.size(), records.size());

        List<String> toolReferences = new ArrayList<>();
        String replyText;
        try {
            if (cfg.agent().enabled()) {
                AiToolBridge tools = new AiToolBridge(
                        agentContext,
                        cfg,
                        capabilityRegistry,
                        memoryRetriever,
                        aiMemoryManager,
                        chatQuotaService,
                        cfg.memory().retrieveLimit(),
                        toolReferences,
                        memoryEmbeddingService,
                        messageRecordManager
                );
                AgentRunResult result = ChatAgentRunner.run(
                        modelRouter,
                        prompt,
                        tools,
                        cfg.agent().maxIterations(),
                        toolReferences
                );
                if (result.directHandled()) {
                    return StructuredReply.handledDirectly();
                }
                replyText = result.speechText();
            } else {
                ChatResponse chatResponse = modelRouter.chat(prompt);
                replyText = chatResponse.aiMessage().text();
            }
        } catch (Exception e) {
            LOGGER.warn("AI reply generation failed", e);
            if (ChatSafetyRejectionTracker.looksLikeProviderSafetyRejection(e)) {
                ChatSafetyRejectionTracker.markPromptMessages(ctx, records);
                engagementService.update(botId, groupId, 0, cfg.engagement());
                LOGGER.debug("marked recent prompt messages after provider safety rejection group={} sender={} history={}",
                        ctx.group(), ctx.sender(), records.size());
            }
            return null;
        }

        replyText = rewriteIdentityDisclosureDraft(prompt, replyText, ctx, engagementMode);
        EngagementMeta engagementMeta = EngagementMeta.inferFromSpeech(replyText);
        if (KanriProposalParser.containsMuteProposal(replyText)) {
            PendingKanriProposalStore.putMute(
                    botId,
                    groupId,
                    KanriProposalParser.extractMuteTargetKeywords(replyText),
                    KanriProposalParser.extractMuteMinutes(replyText, 10)
            );
        }
        if (engagementMode) {
            EngagementMetaParser.ParsedEngagement parsed = EngagementMetaParser.parse(replyText);
            replyText = parsed.speech();
            engagementMeta = parsed.meta();
            int interest = engagementMeta.interest();
            if (ChatConversationSignals.shouldBoostFollowInterest(replyText)) {
                interest = Math.max(interest, cfg.engagement().followInterestThreshold());
            }
            engagementService.update(botId, groupId, interest, cfg.engagement());
            LOGGER.debug("AI engagement meta group={} reply={} interest={}",
                    groupId, engagementMeta.shouldReply(), interest);
            if (!engagementMeta.shouldReply()) {
                return null;
            }
        }

        if (!decisionMaker.tryAcquireReplySlot(ctx, addressing.strongMentioned())) {
            LOGGER.debug("skip AI reply due rate limit group={} sender={}", ctx.group(), ctx.sender());
            return null;
        }

        StructuredReply structured = responseSanitizer.sanitize(ctx, replyText, toolReferences);
        if (ChatReplyContextPolicy.isStaleIdentityAnswer(ctx, structured)) {
            LOGGER.debug("suppress stale identity answer group={} sender={} msgId={}", ctx.group(), ctx.sender(), ctx.msgId());
            return null;
        }
        if (ChatReplyContextPolicy.isDuplicateRecentBotReply(ctx, records, structured, bot.getSelfId())) {
            LOGGER.debug("suppress duplicate recent AI reply group={} sender={} msgId={}", ctx.group(), ctx.sender(), ctx.msgId());
            return null;
        }
        if (ChatReplyContextPolicy.isVacuousReplyToOtherAddressedMessage(ctx, structured, bot.getSelfId())) {
            LOGGER.debug("suppress vacuous reply to other-addressed message group={} sender={} msgId={}",
                    ctx.group(), ctx.sender(), ctx.msgId());
            return null;
        }
        structured = ChatReplyContextPolicy.applyBusyGroupQuote(ctx, records, structured, cfg.reply());
        if (StringUtils.isNullOrEmptyEx(structured.speech()) && structured.references().isEmpty()) {
            return null;
        }
        LOGGER.debug("AI reply ready group={} sender={} speechChars={} refs={} atTargets={} replyTo={}",
                ctx.group(), ctx.sender(), structured.speech().length(), structured.references().size(),
                structured.atTargets().size(), structured.replyToMessageId());
        memoryExtractor.extractAsync(modelRouter.primaryModel(), aiMemoryManager, cfg, agentContext, ctx.msg(), structured.speech());
        return structured;
    }

    @Nonnull
    private String rewriteIdentityDisclosureDraft(@Nonnull List<ChatMessage> prompt
            , @Nonnull String replyText
            , @Nonnull BaniraCodeContext ctx
            , boolean engagementMode
    ) {
        if (StringUtils.isNullOrEmptyEx(replyText) || !guard.looksLikeIdentityDisclosure(replyText)) {
            return replyText;
        }
        try {
            EngagementMeta originalMeta = EngagementMetaParser.parse(replyText).meta();
            List<ChatMessage> rewritePrompt = new ArrayList<>(prompt);
            rewritePrompt.add(SystemMessage.from("""
                    上一条草稿触碰了身份披露边界。
                    请保留对当前消息的回答意图，用自然群聊口吻改写。
                    不要承认或解释你自己的 AI、机器人、大模型、语言模型、底层接口、系统规则。
                    不要提“规则/边界/身份披露/改写”。
                    如果当前问题就是追问你自己的身份，用一句自然话带过，像被贴奇怪标签时轻微不耐烦地岔开。
                    不要直接说“不是”，不要承认，也不要解释内部实现。
                    如果草稿末尾有 [ENGAGE reply=...|interest=...] 元数据，必须原样保留在最后。
                    """));
            rewritePrompt.add(UserMessage.from("需要改写的草稿：\n" + replyText));
            ChatResponse rewritten = modelRouter.chat(rewritePrompt);
            String text = rewritten.aiMessage() != null ? rewritten.aiMessage().text() : "";
            if (StringUtils.isNotNullOrEmpty(text) && !guard.looksLikeIdentityDisclosure(text)) {
                LOGGER.debug("rewrote identity disclosure draft group={} sender={}", ctx.group(), ctx.sender());
                return ensureEngagementMeta(text, originalMeta, engagementMode);
            }
        } catch (Exception e) {
            LOGGER.debug("identity disclosure rewrite failed group={} sender={} error={}",
                    ctx.group(), ctx.sender(), e.toString());
        }
        return "";
    }

    @Nonnull
    private static String ensureEngagementMeta(@Nonnull String text
            , @Nonnull EngagementMeta meta
            , boolean engagementMode
    ) {
        if (!engagementMode || text.toUpperCase(Locale.ROOT).contains("[ENGAGE")) {
            return text;
        }
        String reply = meta.shouldReply() ? "yes" : "no";
        return text.trim() + "\n[ENGAGE reply=" + reply + "|interest=" + meta.interest() + "]";
    }

    private StructuredReply tryHandleDirectWeather(@Nonnull AgentContext agentContext
            , @Nonnull BaniraCodeContext ctx
            , @Nonnull TurnAddressing addressing
    ) {
        if (!addressing.directlyAddressed() || !looksLikeWeatherQuestion(ctx.msg())) {
            return null;
        }
        String location = extractWeatherLocation(ctx.msg());
        if (StringUtils.isNullOrEmptyEx(location)) {
            return null;
        }
        Map<String, String> args = Map.of("location", location);
        CapabilityInvocationPolicy.Decision decision = CapabilityInvocationPolicy.evaluate(
                agentContext,
                capabilityRegistry.resolve(ctx.group(), "get_weather"),
                "get_weather",
                args
        );
        if (!decision.allowed()) {
            return null;
        }
        String result = capabilityRegistry.execute(agentContext, cfg, "get_weather", args);
        if (StringUtils.isNullOrEmptyEx(result) || result.startsWith("未找到能力")) {
            return null;
        }
        LOGGER.debug("AI direct weather handled group={} sender={} location={}", ctx.group(), ctx.sender(), location);
        return new StructuredReply(result, List.of(), ctx.msgId(), List.of(), false);
    }

    private static boolean looksLikeWeatherQuestion(String text) {
        if (StringUtils.isNullOrEmptyEx(text)) {
            return false;
        }
        return text.contains("天气")
                || text.contains("气温")
                || text.contains("温度")
                || text.contains("下雨")
                || text.contains("冷不冷")
                || text.contains("热不热");
    }

    @Nonnull
    private String extractWeatherLocation(String text) {
        String location = StringUtils.nullToEmpty(text)
                .replaceAll("\\[CQ:[^]]+]", " ")
                .replaceAll("@\\S{1,32}", " ");
        Set<String> names = new LinkedHashSet<>();
        for (String nick : BaniraUtils.getBotNicks()) {
            addName(names, nick);
        }
        for (String alias : cfg.reply().botNameAliases()) {
            addName(names, alias);
        }
        for (String name : names) {
            location = location.replace(name, " ");
        }
        return location
                .replaceAll("(帮我|查一下|查询|看看|看一下|今天|明天|后天|现在|当前|的|天气|气温|温度|下雨|冷不冷|热不热|怎么样|如何)", " ")
                .replaceAll("[，,。！？!?：:;；~～\\s]+", " ")
                .trim();
    }

    @Nonnull
    private TurnAddressing resolveAddressing(@Nonnull BaniraBot bot, @Nonnull BaniraCodeContext ctx) {
        boolean directMentioned = bot.isMentioned(ctx.originalMsg());
        boolean nameMentioned = isNameMentioned(bot, ctx);
        boolean botNamePrefixMentioned = isGlobalBotNamePrefixMentioned(ctx);
        boolean strongMentioned = directMentioned || botNamePrefixMentioned || ctx.msgType() != EnumMessageType.GROUP;
        boolean directlyAddressed = directMentioned || nameMentioned || botNamePrefixMentioned || ctx.msgType() != EnumMessageType.GROUP;
        return new TurnAddressing(directMentioned, nameMentioned, botNamePrefixMentioned, strongMentioned, directlyAddressed);
    }

    private boolean isNameMentioned(BaniraBot bot, BaniraCodeContext ctx) {
        if (StringUtils.isNullOrEmptyEx(ctx.msg())) {
            return false;
        }
        String message = ctx.msg().toLowerCase(Locale.ROOT);
        Set<String> names = new LinkedHashSet<>();
        for (String nick : BaniraUtils.getBotNicks()) {
            addName(names, nick);
        }
        if (bot.getLoginInfoEx() != null) {
            addName(names, bot.getLoginInfoEx().getNickname());
        }
        for (String alias : cfg.reply().botNameAliases()) {
            addName(names, alias);
        }
        return names.stream().anyMatch(message::contains);
    }

    private boolean isGlobalBotNamePrefixMentioned(BaniraCodeContext ctx) {
        if (StringUtils.isNullOrEmptyEx(ctx.msg())) {
            return false;
        }
        String message = ctx.msg().trim().toLowerCase(Locale.ROOT);
        for (String botNick : BaniraUtils.getBotNicks()) {
            if (StringUtils.isNullOrEmptyEx(botNick) || botNick.trim().length() < 2) {
                continue;
            }
            String name = botNick.trim().toLowerCase(Locale.ROOT);
            if (message.startsWith(name)) {
                return true;
            }
        }
        return false;
    }

    private static void addName(Set<String> names, String name) {
        if (StringUtils.isNullOrEmptyEx(name)) {
            return;
        }
        String normalized = name.trim().toLowerCase(Locale.ROOT);
        if (normalized.length() >= 2) {
            names.add(normalized);
        }
    }

    private record TurnAddressing(boolean directMentioned
            , boolean nameMentioned
            , boolean botNamePrefixMentioned
            , boolean strongMentioned
            , boolean directlyAddressed) {
    }
}
