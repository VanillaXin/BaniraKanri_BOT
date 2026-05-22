package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Resource;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.plugin.chat.capability.AiCapabilityRegistry;
import xin.vanilla.banira.plugin.chat.memory.MemoryEmbeddingService;
import xin.vanilla.banira.plugin.chat.memory.MemoryExtractor;
import xin.vanilla.banira.plugin.chat.memory.MemoryRetriever;
import xin.vanilla.banira.plugin.chat.model.ChatQuotaService;
import xin.vanilla.banira.plugin.kanri.KanriService;
import xin.vanilla.banira.service.IAiMemoryManager;
import xin.vanilla.banira.service.IMessageRecordManager;

/**
 * 按群配置创建 AIChatService 实例
 */
@Component
public class AIChatServiceFactory {

    @Resource
    private IMessageRecordManager messageRecordManager;
    @Resource
    private AiCapabilityRegistry capabilityRegistry;
    @Resource
    private MemoryRetriever memoryRetriever;
    @Resource
    private MemoryExtractor memoryExtractor;
    @Resource
    private MemoryEmbeddingService memoryEmbeddingService;
    @Resource
    private IAiMemoryManager aiMemoryManager;
    @Resource
    private ChatAffinityService affinityService;
    @Resource
    private ChatQuotaService chatQuotaService;
    @Resource
    private ChatEngagementService engagementService;
    @Resource
    private ChatGroupTurnCoordinator turnCoordinator;
    @Resource
    private KanriService kanriService;

    public AIChatService create(ChatConfig cfg) {
        return new AIChatService(ChatConfigSupport.normalize(cfg), messageRecordManager, capabilityRegistry, memoryRetriever, memoryExtractor, memoryEmbeddingService, aiMemoryManager, affinityService, chatQuotaService, engagementService, turnCoordinator, kanriService);
    }

}
