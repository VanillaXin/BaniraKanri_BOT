package xin.vanilla.banira.plugin.chat.memory;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.domain.AiMemory;
import xin.vanilla.banira.mapper.param.AiMemoryQueryParam;
import xin.vanilla.banira.plugin.chat.agent.AgentContext;
import xin.vanilla.banira.plugin.chat.ChatRecordTimeFormatter;
import xin.vanilla.banira.service.IAiMemoryManager;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.DateUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 按需检索长期记忆
 */
@Component
@Slf4j
public class MemoryRetriever {

    private static final int MAX_QUERY_TOKENS = 24;
    private static final String LOW_IMPORTANCE_TAG = "importance:low";
    private static final Set<String> STOP_TOKENS = Set.of(
            "这个", "那个", "什么", "一下", "帮我", "给我", "看看", "看下", "查询", "搜索",
            "现在", "怎么", "为什么", "可以", "需要", "进行", "没有", "不是", "就是", "还有",
            "如果", "知道", "回复", "消息", "问题"
    );

    @Resource
    private IAiMemoryManager aiMemoryManager;
    @Resource
    private MemoryEmbeddingService memoryEmbeddingService;

    @Nonnull
    public List<AiMemory> retrieve(@Nonnull AgentContext ctx, @Nonnull ChatConfig cfg, @Nonnull String query) {
        int limit = cfg.memory().retrieveLimit();
        if (limit <= 0) {
            return Collections.emptyList();
        }
        if (StringUtils.isNullOrEmptyEx(query)) {
            return touch(listRecent(ctx, Math.min(limit, 2)));
        }
        Map<Long, AiMemory> scored = new LinkedHashMap<>();
        Map<Long, Integer> scores = new HashMap<>();
        if (memoryEmbeddingService != null && cfg.memory().semanticRetrieveEnabled()) {
            for (MemoryEmbeddingService.SemanticMemoryMatch match : memoryEmbeddingService.retrieve(ctx, cfg, query, aiMemoryManager)) {
                scored.putIfAbsent(match.memory().getId(), match.memory());
                scores.merge(match.memory().getId(), Math.max(1, (int) Math.round(match.score() * 1000)), Integer::sum);
            }
        }
        List<String> tokens = tokenize(query);
        if (cfg.memory().keywordFallbackEnabled() || scored.isEmpty()) {
            for (String token : tokens) {
                if (token.length() < 2) {
                    continue;
                }
                appendMatches(ctx, token, scored, scores);
            }
        }
        boolean asksAboutBotSaid = asksAboutBotSaid(query);
        if (asksAboutBotSaid && cfg.memory().lowImportanceMemoryEnabled()) {
            appendRecentLowImportance(ctx, scored, scores, cfg.memory().lowImportanceRetrieveLimit());
        }
        if (scored.isEmpty()) {
            LOGGER.debug("AI memory retrieve group={} user={} tokens={} matched=0",
                    ctx.scopeGroupId(), ctx.senderId(), tokens.size());
            return Collections.emptyList();
        }
        List<AiMemory> result = scored.values().stream()
                .filter(memory -> shouldUseForCurrentQuery(memory, asksAboutBotSaid))
                .sorted(Comparator
                        .comparingInt((AiMemory m) -> scores.getOrDefault(m.getId(), 0)).reversed()
                        .thenComparing((AiMemory m) -> m.getLastUsedAt() != null ? m.getLastUsedAt() : 0L,
                                Comparator.reverseOrder()))
                .limit(limit)
                .toList();
        LOGGER.debug("AI memory retrieve group={} user={} tokens={} matched={} used={}",
                ctx.scopeGroupId(), ctx.senderId(), tokens.size(), scored.size(), result.size());
        return touch(result);
    }

    @Nonnull
    public String format(@Nonnull List<AiMemory> memories) {
        return format(null, memories);
    }

    @Nonnull
    public String format(AgentContext ctx, @Nonnull List<AiMemory> memories) {
        if (memories.isEmpty()) {
            return "";
        }
        List<AiMemory> important = memories.stream()
                .filter(memory -> !isLowImportance(memory))
                .toList();
        List<AiMemory> lightweight = memories.stream()
                .filter(MemoryRetriever::isLowImportance)
                .toList();
        List<String> sections = new ArrayList<>();
        if (!important.isEmpty()) {
            sections.add("重要长期记忆：\n" + formatLines(ctx, important));
        }
        if (!lightweight.isEmpty()) {
            sections.add("轻量会话记忆（只表示你曾说过或短期上下文，不是长期设定）：\n" + formatLines(ctx, lightweight));
        }
        return String.join("\n", sections);
    }

    @Nonnull
    private static String scopePrefix(AgentContext ctx, @Nonnull AiMemory memory) {
        long userId = memory.getUserId() != null ? memory.getUserId() : 0L;
        long currentUser = ctx != null ? ctx.senderId() : 0L;
        String tags = StringUtils.nullToEmpty(memory.getTags()).toLowerCase(Locale.ROOT);
        if (isLowImportance(memory)) {
            return "[轻量会话记忆；你曾说过；不是长期设定] ";
        }
        if (ctx != null && userId > 0 && userId == currentUser) {
            return "[当前发言者 qq=" + currentUser + " 的记忆] ";
        }
        if (userId == 0 && (tags.contains("owner_instruction") || tags.contains("behavior"))) {
            Long owner = BaniraUtils.getOwner();
            String ownerText = owner != null && owner > 0 ? "配置熟人qq=" + owner : "配置熟人账号";
            String ownerDisplay = BaniraUtils.getOwnerDisplayName();
            String currentText = ctx != null && BaniraUtils.isOwner(currentUser)
                    ? "当前发言者就是这位配置熟人"
                    : "不要把这条套用成当前发言者身份";
            return "[配置熟人长期偏好；称呼=" + ownerDisplay + "；" + ownerText + "；" + currentText + "] ";
        }
        if (userId == 0) {
            return "[本群公共记忆] ";
        }
        return "[其他用户 qq=" + userId + " 的记忆；除非当前消息明确询问此人，否则不要套用到当前发言者] ";
    }

    @Nonnull
    private static String displayContent(@Nonnull AiMemory memory) {
        String content = StringUtils.nullToEmpty(memory.getContent());
        String tags = StringUtils.nullToEmpty(memory.getTags()).toLowerCase(Locale.ROOT);
        if ((tags.contains("owner_instruction") || tags.contains("behavior"))
                && content.contains("叫我")
                && (content.startsWith("主人对你的长期行为要求：")
                || content.startsWith("主人账号对你的长期行为要求："))) {
            String raw = content
                    .replaceFirst("^主人对你的长期行为要求：", "")
                    .replaceFirst("^主人账号对你的长期行为要求：", "");
            return MemoryExtractor.normalizeOwnerBehaviorInstruction(raw, new ChatConfig());
        }
        return content;
    }

    @Nonnull
    public String search(@Nonnull AgentContext ctx, @Nonnull String keyword, int limit) {
        Map<Long, AiMemory> scored = new LinkedHashMap<>();
        Map<Long, Integer> scores = new HashMap<>();
        for (String token : tokenize(keyword)) {
            appendMatches(ctx, token, scored, scores);
        }
        boolean asksAboutBotSaid = asksAboutBotSaid(keyword);
        List<AiMemory> memories = scored.values().stream()
                .filter(memory -> shouldUseForCurrentQuery(memory, asksAboutBotSaid))
                .sorted(Comparator
                        .comparingInt((AiMemory m) -> scores.getOrDefault(m.getId(), 0)).reversed()
                        .thenComparing((AiMemory m) -> m.getLastUsedAt() != null ? m.getLastUsedAt() : 0L,
                                Comparator.reverseOrder()))
                .limit(Math.max(1, limit))
                .toList();
        if (memories.isEmpty()) {
            return "未找到相关记忆";
        }
        return format(ctx, touch(memories));
    }

    @Nonnull
    private List<AiMemory> listRecent(@Nonnull AgentContext ctx, int limit) {
        AiMemoryQueryParam param = baseParam(ctx)
                .addOrderByLastUsedAt(false)
                .setLimit(limit);
        return aiMemoryManager.getMemoryList(param);
    }

    @Nonnull
    private List<AiMemory> touch(@Nonnull List<AiMemory> memories) {
        long now = DateUtils.getTimestamp(new java.util.Date());
        memories.forEach(memory -> aiMemoryManager.touchMemory(memory.getId(), now));
        return memories;
    }

    private void appendMatches(@Nonnull AgentContext ctx
            , @Nonnull String token
            , @Nonnull Map<Long, AiMemory> scored
            , @Nonnull Map<Long, Integer> scores
    ) {
        AiMemoryQueryParam contentParam = baseParam(ctx)
                .setContentLike(token)
                .addOrderByLastUsedAt(false)
                .setLimit(20);
        for (AiMemory memory : aiMemoryManager.getMemoryList(contentParam)) {
            scored.putIfAbsent(memory.getId(), memory);
            scores.merge(memory.getId(), 2, Integer::sum);
        }
        AiMemoryQueryParam tagParam = baseParam(ctx)
                .setTagsLike(token)
                .addOrderByLastUsedAt(false)
                .setLimit(20);
        for (AiMemory memory : aiMemoryManager.getMemoryList(tagParam)) {
            scored.putIfAbsent(memory.getId(), memory);
            scores.merge(memory.getId(), 1, Integer::sum);
        }
    }

    private void appendRecentLowImportance(@Nonnull AgentContext ctx
            , @Nonnull Map<Long, AiMemory> scored
            , @Nonnull Map<Long, Integer> scores
            , int limit
    ) {
        if (limit <= 0) {
            return;
        }
        AiMemoryQueryParam param = baseParam(ctx)
                .setTagsLike(LOW_IMPORTANCE_TAG)
                .addOrderByLastUsedAt(false)
                .setLimit(Math.max(1, limit));
        for (AiMemory memory : aiMemoryManager.getMemoryList(param)) {
            scored.putIfAbsent(memory.getId(), memory);
            scores.merge(memory.getId(), 6, Integer::sum);
        }
    }

    @Nonnull
    private static AiMemoryQueryParam baseParam(@Nonnull AgentContext ctx) {
        return new AiMemoryQueryParam()
                .setBotId(ctx.botId())
                .setGroupIdInGlobal(ctx.scopeGroupId())
                .setUserIdInGlobal(ctx.senderId());
    }

    @Nonnull
    private static List<String> tokenize(@Nonnull String text) {
        LinkedHashSet<String> tokens = new LinkedHashSet<>();
        String[] parts = text.toLowerCase(Locale.ROOT).split("[\\s，,。！？!?；;：:\\[\\]()（）\"'“”/\\\\]+");
        for (String part : parts) {
            addToken(tokens, part);
            for (String segment : part.split("[的了呢吧呀哦啊吗么]")) {
                addToken(tokens, segment);
                addHanNgrams(tokens, segment);
                if (tokens.size() >= MAX_QUERY_TOKENS) {
                    return List.copyOf(tokens);
                }
            }
            if (tokens.size() >= MAX_QUERY_TOKENS) {
                break;
            }
        }
        return List.copyOf(tokens);
    }

    private static void addHanNgrams(@Nonnull LinkedHashSet<String> tokens, @Nonnull String text) {
        String han = text.replaceAll("[^\\p{IsHan}]", "");
        if (han.length() < 4) {
            return;
        }
        for (int size : new int[]{2, 3, 4}) {
            for (int i = 0; i + size <= han.length(); i++) {
                addToken(tokens, han.substring(i, i + size));
                if (tokens.size() >= MAX_QUERY_TOKENS) {
                    return;
                }
            }
        }
    }

    private static void addToken(@Nonnull LinkedHashSet<String> tokens, String token) {
        if (token == null) {
            return;
        }
        String normalized = token.trim();
        if (normalized.length() < 2 || STOP_TOKENS.contains(normalized)) {
            return;
        }
        tokens.add(normalized);
    }

    @Nonnull
    private static String formatLines(AgentContext ctx, @Nonnull List<AiMemory> memories) {
        return memories.stream()
                .map(memory -> "- " + scopePrefix(ctx, memory) + displayContent(memory)
                        + " [created=" + ChatRecordTimeFormatter.format(memory.getCreatedAt())
                        + ", lastUsed=" + ChatRecordTimeFormatter.format(memory.getLastUsedAt()) + "]"
                        + (StringUtils.isNotNullOrEmpty(memory.getTags()) ? " [" + memory.getTags() + "]" : ""))
                .collect(Collectors.joining("\n"));
    }

    private static boolean isLowImportance(@Nonnull AiMemory memory) {
        String tags = StringUtils.nullToEmpty(memory.getTags()).toLowerCase(Locale.ROOT);
        return tags.contains(LOW_IMPORTANCE_TAG) || tags.contains("type:episodic");
    }

    private static boolean shouldUseForCurrentQuery(@Nonnull AiMemory memory, boolean asksAboutBotSaid) {
        if (!isLowImportance(memory) || asksAboutBotSaid) {
            return true;
        }
        return !MemoryExtractor.isCapabilityOrPermissionFailureReply(StringUtils.nullToEmpty(memory.getContent()));
    }

    private static boolean asksAboutBotSaid(@Nonnull String query) {
        String compact = query.replaceAll("\\[CQ:[^]]+]", " ")
                .replaceAll("\\s+", "")
                .toLowerCase(Locale.ROOT);
        return compact.contains("你说过")
                || compact.contains("你刚才说")
                || compact.contains("你之前说")
                || compact.contains("你前面说")
                || compact.contains("你不是说")
                || compact.contains("刚刚你说")
                || compact.contains("刚才你说")
                || compact.contains("之前你说")
                || compact.contains("前面你说")
                || compact.contains("否认")
                || compact.contains("认账");
    }

}
