package xin.vanilla.banira.plugin.chat;

import lombok.extern.slf4j.Slf4j;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.plugin.chat.capability.AiCapabilityRegistry;
import xin.vanilla.banira.plugin.chat.memory.MemoryEmbeddingService;
import xin.vanilla.banira.plugin.chat.memory.MemoryExtractor;
import xin.vanilla.banira.plugin.chat.memory.MemoryRetriever;
import xin.vanilla.banira.plugin.chat.model.ChatModelRouter;
import xin.vanilla.banira.plugin.chat.model.ChatQuotaService;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.kanri.KanriService;
import xin.vanilla.banira.service.IAiMemoryManager;
import xin.vanilla.banira.service.IMessageRecordManager;

import java.util.Objects;

@Slf4j
public class AIChatService {

    private final ChatConfig cfg;
    private final ChatTurnPipeline pipeline;
    private final ChatGroupTurnCoordinator turnCoordinator;
    private final ChatModelRouter modelRouter;

    public AIChatService(ChatConfig cfg
            , IMessageRecordManager messageRecordManager
            , AiCapabilityRegistry capabilityRegistry
            , MemoryRetriever memoryRetriever
            , MemoryExtractor memoryExtractor
            , MemoryEmbeddingService memoryEmbeddingService
            , IAiMemoryManager aiMemoryManager
            , ChatAffinityService affinityService
            , ChatQuotaService chatQuotaService
            , ChatEngagementService engagementService
            , ChatGroupTurnCoordinator turnCoordinator
            , KanriService kanriService
    ) {
        this.cfg = ChatConfigSupport.normalize(Objects.requireNonNull(cfg, "cfg"));
        this.turnCoordinator = Objects.requireNonNull(turnCoordinator, "turnCoordinator");
        Objects.requireNonNull(messageRecordManager, "messageRecordManager");
        Objects.requireNonNull(capabilityRegistry, "capabilityRegistry");
        Objects.requireNonNull(memoryRetriever, "memoryRetriever");
        Objects.requireNonNull(memoryExtractor, "memoryExtractor");
        Objects.requireNonNull(memoryEmbeddingService, "memoryEmbeddingService");
        Objects.requireNonNull(aiMemoryManager, "aiMemoryManager");
        Objects.requireNonNull(affinityService, "affinityService");
        Objects.requireNonNull(chatQuotaService, "chatQuotaService");
        Objects.requireNonNull(engagementService, "engagementService");
        Objects.requireNonNull(kanriService, "kanriService");

        this.modelRouter = new ChatModelRouter(this.cfg);
        ReplyDecisionMaker decisionMaker = new ReplyDecisionMaker(this.cfg.reply(), this.cfg.affinity());
        ChatEngagementGate engagementGate = new ChatEngagementGate(
                this.cfg.engagement(),
                this.cfg.reply(),
                engagementService,
                decisionMaker
        );
        ChatGuardService guard = ChatGuardService.from(this.cfg);
        ChatHistoryProvider historyProvider = new ChatHistoryProvider(this.cfg, messageRecordManager);
        ChatPromptAssembler promptAssembler = new ChatPromptAssembler(this.cfg, memoryRetriever, capabilityRegistry);
        ChatResponseSanitizer responseSanitizer = new ChatResponseSanitizer(this.cfg, guard);
        this.pipeline = new ChatTurnPipeline(
                this.cfg,
                this.modelRouter,
                decisionMaker,
                capabilityRegistry,
                memoryRetriever,
                memoryExtractor,
                memoryEmbeddingService,
                aiMemoryManager,
                affinityService,
                chatQuotaService,
                guard,
                historyProvider,
                promptAssembler,
                responseSanitizer,
                engagementGate,
                engagementService,
                kanriService,
                messageRecordManager
        );
    }

    public StructuredReply generateReply(BaniraBot bot, BaniraCodeContext ctx) {
        ChatTurnCoalesceOutcome outcome = turnCoordinator.run(bot, ctx, cfg, pipeline::generateReply);
        if (outcome instanceof ChatTurnCoalesceOutcome.Follower) {
            return null;
        }
        return ((ChatTurnCoalesceOutcome.Leader) outcome).reply();
    }

    public boolean generateAndSendReply(BaniraBot bot, BaniraCodeContext ctx) {
        ChatTurnCoalesceOutcome outcome;
        try (ChatThinkingFeedbackService.FeedbackHandle ignored =
                     ChatThinkingFeedbackService.arm(bot, ctx, cfg, modelRouter)) {
            outcome = turnCoordinator.run(bot, ctx, cfg, pipeline::generateReply);
        }
        if (outcome instanceof ChatTurnCoalesceOutcome.Follower) {
            return false;
        }
        ChatTurnCoalesceOutcome.Leader leader = (ChatTurnCoalesceOutcome.Leader) outcome;
        StructuredReply reply = leader.reply();
        if (reply == null) {
            return false;
        }
        if (reply.directHandled()) {
            return true;
        }
        return ReplyDeliveryService.deliver(bot, leader.replyContext(), reply, cfg.reply());
    }
}
