package xin.vanilla.banira.plugin.chat.memory;

import dev.langchain4j.data.message.ChatMessage;
import dev.langchain4j.data.message.SystemMessage;
import dev.langchain4j.data.message.UserMessage;
import dev.langchain4j.model.chat.ChatModel;
import dev.langchain4j.model.chat.response.ChatResponse;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.domain.AiMemory;
import xin.vanilla.banira.plugin.chat.ChatGuardService;
import xin.vanilla.banira.plugin.chat.agent.AgentContext;
import xin.vanilla.banira.plugin.chat.agent.PromptTemplateLoader;
import xin.vanilla.banira.service.IAiMemoryManager;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.DateUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 对话结束后异步提取长期记忆
 */
@Slf4j
@Component
public class MemoryExtractor {

    private static final String MEMORY_EXTRACT_PROMPT = "prompt/aichat/memory-extract.txt";

    @Resource
    private MemoryEmbeddingService memoryEmbeddingService;

    private final ThreadPoolExecutor executor = new ThreadPoolExecutor(1, 1,
            0L, TimeUnit.MILLISECONDS,
            new ArrayBlockingQueue<>(1000),
            r -> {
                Thread thread = new Thread(r, "ai-memory-extractor");
                thread.setDaemon(true);
                return thread;
            }, new ThreadPoolExecutor.AbortPolicy());

    public void extractAsync(@Nonnull ChatModel chatModel
            , @Nonnull IAiMemoryManager aiMemoryManager
            , @Nonnull ChatConfig cfg
            , @Nonnull AgentContext ctx
            , @Nonnull String userMessage
            , @Nonnull String botReply
    ) {
        if (!cfg.memory().autoExtract()) {
            return;
        }
        if (StringUtils.isNullOrEmptyEx(userMessage) || StringUtils.isNullOrEmptyEx(botReply)) {
            return;
        }
        ChatGuardService guard = ChatGuardService.from(cfg);
        if (guard.isPromptExtractionAttempt(userMessage)
                || guard.looksLikePromptLeak(botReply)) {
            LOGGER.debug("skip memory extraction for prompt-security-sensitive turn group={} user={}",
                    ctx.groupId(), ctx.senderId());
            return;
        }
        saveLowImportanceReplyMemory(aiMemoryManager, cfg, ctx, userMessage, botReply);
        if (trySaveOwnerBehaviorInstruction(aiMemoryManager, cfg, ctx, userMessage)) {
            return;
        }
        int queueLimit = Math.max(1, cfg.memory().memoryExtractionQueueSize());
        if (executor.getQueue().size() >= queueLimit) {
            LOGGER.debug("skip memory extraction: queue full group={} user={} size={} limit={}",
                    ctx.groupId(), ctx.senderId(), executor.getQueue().size(), queueLimit);
            return;
        }
        try {
            executor.execute(() -> {
                try {
                    extract(chatModel, aiMemoryManager, cfg, ctx, userMessage, botReply);
                } catch (Exception e) {
                    LOGGER.warn("Failed to extract ai memory", e);
                }
            });
        } catch (Exception e) {
            LOGGER.debug("skip memory extraction: executor rejected group={} user={} error={}",
                    ctx.groupId(), ctx.senderId(), e.toString());
        }
    }

    private void extract(@Nonnull ChatModel chatModel
            , @Nonnull IAiMemoryManager aiMemoryManager
            , @Nonnull ChatConfig cfg
            , @Nonnull AgentContext ctx
            , @Nonnull String userMessage
            , @Nonnull String botReply
    ) {
        List<ChatMessage> prompt = new ArrayList<>();
        prompt.add(SystemMessage.from(PromptTemplateLoader.render(MEMORY_EXTRACT_PROMPT, Map.of())));
        prompt.add(UserMessage.from("当前发言者QQ：" + ctx.senderId()
                + "\n用户原话：" + userMessage
                + "\n你的回复：" + botReply));
        ChatResponse response = chatModel.chat(prompt);
        String text = response.aiMessage().text();
        if (StringUtils.isNullOrEmptyEx(text) || "NONE".equalsIgnoreCase(text.trim())) {
            return;
        }
        long now = DateUtils.getTimestamp(new java.util.Date());
        int saved = 0;
        int maxSaved = Math.max(1, cfg.memory().maxExtractedPerTurn());
        for (String line : text.split("\\R")) {
            if (saved >= maxSaved) {
                break;
            }
            String content = line.trim();
            if (content.isEmpty() || "NONE".equalsIgnoreCase(content)) {
                continue;
            }
            content = content.replaceFirst("^[-*\\d.、)\\]]+\\s*", "");
            if (!MemorySafety.isSafeToStore(content, cfg)) {
                continue;
            }
            content = MemorySafety.normalize(content, cfg);
            if (MemoryScopePolicy.isUnsafeOwnerTitleClaim(ctx.senderId(), content)) {
                continue;
            }
            if (ChatGuardService.from(cfg).looksLikePromptLeak(content)) {
                continue;
            }
            if (aiMemoryManager.existsSimilar(ctx.botId(), ctx.scopeGroupId(), ctx.senderId(), content)) {
                continue;
            }
            AiMemory memory = new AiMemory()
                    .setBotId(ctx.botId())
                    .setGroupId(ctx.scopeGroupId())
                    .setUserId(ctx.senderId())
                    .setContent(content)
                    .setTags("source:auto")
                    .setSourceMsgId(ctx.msgId())
                    .setCreatedAt(now)
                    .setLastUsedAt(now);
            aiMemoryManager.addMemory(memory);
            if (memoryEmbeddingService != null) {
                memoryEmbeddingService.indexMemory(cfg, memory);
            }
            saved++;
        }
        if (saved > 0) {
            LOGGER.debug("saved ai memories group={} user={} count={}", ctx.groupId(), ctx.senderId(), saved);
        }
    }

    private void saveLowImportanceReplyMemory(@Nonnull IAiMemoryManager aiMemoryManager
            , @Nonnull ChatConfig cfg
            , @Nonnull AgentContext ctx
            , @Nonnull String userMessage
            , @Nonnull String botReply
    ) {
        if (!cfg.memory().lowImportanceMemoryEnabled()) {
            return;
        }
        String reply = normalizeLowImportanceText(botReply, cfg.memory().maxLowImportanceMemoryChars());
        if (!isWorthLowImportanceMemory(reply)) {
            return;
        }
        if (ChatGuardService.from(cfg).looksLikePromptLeak(reply) || !MemorySafety.isSafeToStore(reply, cfg)) {
            return;
        }
        String user = normalizeLowImportanceText(userMessage, 80);
        String content = "你曾在本群";
        if (ctx.senderId() != null && ctx.senderId() > 0) {
            content += "对 qq=" + ctx.senderId();
        }
        if (StringUtils.isNotNullOrEmpty(user)) {
            content += "（对方当时说：「" + user + "」）";
        }
        content += "回复过：「" + reply + "」";
        content = MemorySafety.normalize(content, cfg);
        if (!MemorySafety.isSafeToStore(content, cfg)
                || aiMemoryManager.existsSimilar(ctx.botId(), ctx.scopeGroupId(), 0L, content)) {
            return;
        }
        long now = DateUtils.getTimestamp(new java.util.Date());
        AiMemory memory = new AiMemory()
                .setBotId(ctx.botId())
                .setGroupId(ctx.scopeGroupId())
                .setUserId(0L)
                .setContent(content)
                .setTags("type:episodic,importance:low,source:auto_reply,bot_said")
                .setSourceMsgId(ctx.msgId())
                .setCreatedAt(now)
                .setLastUsedAt(now);
        aiMemoryManager.addMemory(memory);
        if (memoryEmbeddingService != null) {
            memoryEmbeddingService.indexMemory(cfg, memory);
        }
        LOGGER.debug("saved low-importance reply memory group={} user={} chars={}",
                ctx.groupId(), ctx.senderId(), content.length());
    }

    @Nonnull
    private static String normalizeLowImportanceText(@Nonnull String text, int maxChars) {
        String cleaned = text.replaceAll("\\[ENGAGE\\s+reply=(?:yes|no)\\|interest=\\d+]\\s*", "")
                .replaceAll("\\[REPLY:\\d+]\\s*", "")
                .replaceAll("\\[AT:\\d+]\\s*", "")
                .replaceAll("\\[/?REF]", " ")
                .replaceAll("\\s+", " ")
                .trim();
        if (cleaned.length() > maxChars) {
            cleaned = cleaned.substring(0, Math.max(1, maxChars)).trim();
        }
        return cleaned;
    }

    static boolean isWorthLowImportanceMemory(@Nonnull String reply) {
        if (reply.length() < 6) {
            return false;
        }
        String compact = reply.replaceAll("[\\p{Punct}！？。。，、~～…·\\s]", "");
        if (compact.length() < 4) {
            return false;
        }
        if (compact.matches("^(好|行|可以|收到|知道了|嗯|干嘛|有事快说|不行|不是)$")) {
            return false;
        }
        return !reply.contains("工具执行失败")
                && !reply.contains("权限不足：")
                && !reply.contains("The request was rejected")
                && !reply.contains("处理步骤过多")
                && !isCapabilityOrPermissionFailureReply(reply);
    }

    static boolean isCapabilityOrPermissionFailureReply(@Nonnull String text) {
        String compact = text.replaceAll("\\[CQ:[^]]+]", " ")
                .replaceAll("\\s+", "")
                .toLowerCase(java.util.Locale.ROOT);
        if (StringUtils.isNullOrEmptyEx(compact)) {
            return false;
        }
        boolean capabilityFailure = containsAny(compact,
                "没权限", "没有权限", "无权限", "权限不够", "权限不足", "权限不允许",
                "没法", "无法", "不能", "做不了", "办不到", "执行不了",
                "改不了", "禁不了", "撤不了", "发不了", "查不了", "看不了", "调不了",
                "不支持", "失败", "报错", "被拦", "拦住", "卡住",
                "需要管理员", "需要群主", "不是群管", "没有群管", "缺群管");
        boolean capabilityContext = containsAny(compact,
                "工具", "接口", "权限", "群管", "管理员", "群主", "群名片", "禁言",
                "解禁", "撤回", "搜索", "查询", "发送", "上传", "文件", "配置", "表情包",
                "mc百科", "mcmod", "服务器", "rcon");
        return capabilityFailure && capabilityContext;
    }

    private static boolean containsAny(@Nonnull String text, @Nonnull String... needles) {
        for (String needle : needles) {
            if (text.contains(needle)) {
                return true;
            }
        }
        return false;
    }

    boolean trySaveOwnerBehaviorInstruction(@Nonnull IAiMemoryManager aiMemoryManager
            , @Nonnull ChatConfig cfg
            , @Nonnull AgentContext ctx
            , @Nonnull String userMessage
    ) {
        if (!BaniraUtils.isOwner(ctx.senderId()) || !looksLikeOwnerBehaviorInstruction(userMessage)) {
            return false;
        }
        String instruction = normalizeOwnerBehaviorInstruction(userMessage, cfg);
        if (!MemorySafety.isSafeToStore(instruction, cfg)) {
            return false;
        }
        long groupId = ctx.scopeGroupId();
        if (aiMemoryManager.existsSimilar(ctx.botId(), groupId, 0L, instruction)) {
            return true;
        }
        long now = DateUtils.getTimestamp(new java.util.Date());
        AiMemory memory = new AiMemory()
                .setBotId(ctx.botId())
                .setGroupId(groupId)
                .setUserId(0L)
                .setContent(instruction)
                .setTags("behavior,owner_instruction,source:owner")
                .setSourceMsgId(ctx.msgId())
                .setCreatedAt(now)
                .setLastUsedAt(now);
        aiMemoryManager.addMemory(memory);
        if (memoryEmbeddingService != null) {
            memoryEmbeddingService.indexMemory(cfg, memory);
        }
        LOGGER.debug("saved owner behavior instruction group={} chars={}", groupId, instruction.length());
        return true;
    }

    private static boolean looksLikeOwnerBehaviorInstruction(@Nonnull String text) {
        String normalized = text.replaceAll("\\[CQ:[^]]+]", " ")
                .replaceAll("\\s+", "")
                .trim();
        if (StringUtils.isNullOrEmptyEx(normalized)) {
            return false;
        }
        boolean instructionPrefix = normalized.contains("以后")
                || normalized.contains("之后你")
                || normalized.contains("从现在开始")
                || normalized.contains("记住")
                || normalized.contains("你要")
                || normalized.contains("你应该")
                || normalized.contains("你以后")
                || normalized.contains("慢慢学")
                || normalized.contains("学着");
        boolean behaviorTarget = normalized.contains("回复")
                || normalized.contains("说话")
                || normalized.contains("语气")
                || normalized.contains("风格")
                || normalized.contains("行为")
                || normalized.contains("称呼")
                || normalized.contains("不要")
                || normalized.contains("别")
                || normalized.contains("少")
                || normalized.contains("多");
        return instructionPrefix && behaviorTarget;
    }

    @Nonnull
    static String normalizeOwnerBehaviorInstruction(@Nonnull String text, @Nonnull ChatConfig cfg) {
        String cleaned = text.replaceAll("\\[CQ:[^]]+]", " ")
                .replaceAll("\\s+", " ")
                .trim();
        cleaned = cleaned.replaceFirst("^(记住|你记住|请记住)[，,：:\\s]*", "");
        String explicitNaming = normalizeOwnerNamingPreference(cleaned);
        if (StringUtils.isNotNullOrEmpty(explicitNaming)) {
            return MemorySafety.normalize(explicitNaming, cfg);
        }
        return MemorySafety.normalize("主人账号对你的长期行为要求：" + cleaned, cfg);
    }

    @Nonnull
    private static String normalizeOwnerNamingPreference(@Nonnull String cleaned) {
        String compact = cleaned.replaceAll("\\s+", "");
        Matcher callMatcher = Pattern.compile("(?:以后|之后|从现在开始)?(?:叫我|喊我|称呼我为)([^，,。.!！?？；;\\s]{1,16})").matcher(compact);
        if (!callMatcher.find()) {
            return "";
        }
        String preferred = callMatcher.group(1);
        String forbidden = "";
        Matcher forbidMatcher = Pattern.compile("(?:别|不要|不准)(?:再)?(?:叫我|喊我|称呼我为)?([^，,。.!！?？；;\\s]{1,16})").matcher(compact);
        if (forbidMatcher.find()) {
            forbidden = forbidMatcher.group(1);
        }
        Long owner = BaniraUtils.getOwner();
        String ownerText = owner != null && owner > 0 ? "qq=" + owner : "配置中的熟人账号";
        String result = "配置熟人账号(" + ownerText + ")希望你称呼配置熟人账号为「" + preferred + "」";
        if (StringUtils.isNotNullOrEmpty(forbidden)) {
            result += "，不要称呼配置熟人账号为「" + forbidden + "」";
        }
        return result;
    }

}
