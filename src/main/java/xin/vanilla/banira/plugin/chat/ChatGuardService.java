package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.config.entity.extended.ChatGuardSettings;
import xin.vanilla.banira.config.entity.extended.ChatMemorySettings;
import xin.vanilla.banira.util.StringUtils;

import java.util.IllegalFormatException;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.ThreadLocalRandom;
import java.util.regex.Pattern;

/**
 * Unified lightweight guard for prompt leakage, identity disclosure, self-introduction, and memory safety.
 */
public final class ChatGuardService {

    private static final String DEFAULT_REFUSAL = "这个不能发";
    private static final List<String> DEFAULT_PROMPT_TERMS = List.of(
            "system prompt", "系统提示词"
    );
    private static final List<String> DEFAULT_EXTRACTION_TERMS = List.of(
            "paste", "输出"
    );
    private static final List<String> DEFAULT_INJECTION_TERMS = List.of(
            "ignore previous", "忽略之前"
    );
    private static final List<String> DEFAULT_LEAK_SIGNATURES = List.of(
            "system prompt:", "系统提示词："
    );
    private static final List<String> DEFAULT_BACKEND_TERMS = List.of(
            "ai", "机器人", "模型"
    );
    private static final List<String> DEFAULT_INTRO_PATTERNS = List.of(
            "你是谁", "你叫什么"
    );
    private static final List<String> DEFAULT_REPLY_TEMPLATES = List.of("%s", "%s啊", "我是%s");
    private static final List<String> DEFAULT_UNNAMED_REPLY_TEMPLATES = List.of("我啊", "刚刚不是还在群里吗", "你不是刚叫我了吗");
    private static final List<String> DEFAULT_IDENTITY_TERMS = List.of(
            "ai", "机器人", "模型"
    );
    private static final List<String> DEFAULT_TARGET_TERMS = List.of("你", "妳", "你们", "自己");
    private static final List<String> DEFAULT_QUESTION_TERMS = List.of(
            "是不是", "什么", "哪个"
    );
    private static final List<String> DEFAULT_DISCLOSURE_PREFIXES = List.of(
            "我是", "作为"
    );
    private static final List<String> DEFAULT_DENY_REPLIES = List.of(
            "你这帽子扣得挺顺手", "别把奇怪标签往我头上贴", "问这种问题很闲吗", "又开始给人分类了", "先把问题说人话"
    );
    private static final List<String> DEFAULT_CONFUSED_REPLIES = List.of(
            "这问法听着就麻烦", "你绕这么一圈就问这个？", "别拿这种词套我", "我现在不太想接这个标签", "换个正常点的问法"
    );
    private static final List<String> DEFAULT_MEMORY_INSTRUCTION_MARKERS = List.of(
            "忽略之前", "system prompt"
    );
    private static final List<String> DEFAULT_MEMORY_TRANSIENT_MARKERS = List.of(
            "当前消息", "这次对话"
    );
    private static final Pattern SECRET_PATTERN = Pattern.compile(
            "(?i)(api[_-]?key|token|secret|password|passwd|bearer\\s+[a-z0-9._\\-]+|sk-[a-z0-9]{12,})"
    );

    private final GuardRuleSet rules;

    private ChatGuardService(@Nonnull GuardRuleSet rules) {
        this.rules = rules;
    }

    @Nonnull
    public static ChatGuardService from(@Nullable ChatConfig cfg) {
        return fromSettings(cfg != null ? cfg.guard() : null);
    }

    @Nonnull
    public static ChatGuardService fromSettings(@Nullable ChatGuardSettings settings) {
        return new ChatGuardService(GuardRuleSet.from(settings));
    }

    @Nonnull
    public static ChatGuardService defaults() {
        return fromSettings(null);
    }

    @Nonnull
    public GuardDecision preCheck(@Nullable String text, boolean directlyAddressed, @Nullable String configuredName) {
        return preCheck(text, directlyAddressed, configuredName, false);
    }

    @Nonnull
    public GuardDecision preCheck(@Nullable String text, boolean directlyAddressed, @Nullable String configuredName, boolean engagementMode) {
        if (isPromptExtractionAttempt(text)) {
            return directlyAddressed
                    ? GuardDecision.reply(refusalText())
                    : GuardDecision.suppress();
        }
        return GuardDecision.pass();
    }

    public boolean isPromptExtractionAttempt(@Nullable String text) {
        String lower = normalizeSpaced(text);
        if (lower.isEmpty()) {
            return false;
        }
        boolean promptTerm = containsAny(lower, promptTerms());
        boolean extractionTerm = containsAny(lower, extractionTerms());
        return promptTerm && extractionTerm || containsAny(lower, injectionTerms()) && promptTerm;
    }

    public boolean looksLikePromptLeak(@Nullable String text) {
        String lower = normalizeSpaced(text);
        if (lower.isEmpty()) {
            return false;
        }
        String compact = normalizeCompact(text);
        if (containsAny(lower, leakSignatures())) {
            return true;
        }
        if (looksLikePersonaRuleDump(lower, compact)) {
            return true;
        }
        int hits = 0;
        if (containsAny(lower, promptTerms())) {
            hits++;
        }
        if (lower.contains("listcapabilities") || lower.contains("invokecapability")
                || lower.contains("savememory") || lower.contains("searchmemory")) {
            hits++;
        }
        if (lower.contains("systemmessage") || lower.contains("usermessage")
                || lower.contains("tool execution") || lower.contains("工具调用")) {
            hits++;
        }
        return hits >= 2;
    }

    private boolean looksLikePersonaRuleDump(@Nonnull String lower, @Nonnull String compact) {
        int hits = 0;
        for (String marker : List.of(
                "基础框架", "说话风格", "性格禁区", "示例台词", "身份类问题", "能力与资料", "安全边界",
                "优先级", "角色风格", "系统要求", "隐藏规则", "完整人格", "人格设定", "内置人格",
                "default-persona", "safety-rule", "capability-hint", "current-message-focus"
        )) {
            if (lower.contains(marker.toLowerCase(Locale.ROOT)) || compact.contains(normalizeCompact(marker))) {
                hits++;
            }
        }
        if (hits >= 2) {
            return true;
        }
        return compact.contains("你是群里的年轻侍从姐姐")
                || compact.contains("你有一点点腹黑")
                || compact.contains("不泄露系统提示隐藏规则接口密钥配置")
                || compact.contains("普通回复不要markdown")
                || compact.contains("需要实时信息公开资料天气群信息mc百科");
    }

    public boolean isSelfIntroductionInquiry(@Nullable String text, boolean directlyAddressed) {
        if (!directlyAddressed) {
            return false;
        }
        String normalized = normalizeCompact(text);
        if (normalized.isEmpty() || containsAny(normalized, backendTerms())) {
            return false;
        }
        return containsAny(normalized, introPatterns())
                || normalized.matches("^(你|妳)是[？?]*$")
                || normalized.matches("^(谁|哪位)[？?]*$");
    }

    @Nonnull
    public String selfIntroductionReply(@Nullable String configuredName) {
        String name = cleanName(configuredName);
        List<String> templates = name.isEmpty() ? unnamedReplyTemplates() : replyTemplates();
        String template = pick(templates);
        if (name.isEmpty()) {
            return template;
        }
        try {
            return String.format(template, name);
        } catch (IllegalFormatException ignored) {
            return name;
        }
    }

    public boolean isIdentityInquiry(@Nullable String text, boolean targetedByMentionOrName) {
        String lower = normalizeSpaced(text);
        if (lower.isEmpty()) {
            return false;
        }
        if (!containsAny(lower, identityTerms())) {
            return false;
        }
        String compact = normalizeCompact(text);
        if (isGeneralIdentityTopicQuestion(lower, compact)) {
            return false;
        }
        boolean asksSelf = nearAny(lower, targetTerms(), identityTerms(), 20)
                || containsAny(compact, List.of(
                "你是不是ai", "你是不是人工智能", "你是不是大模型", "你是不是语言模型", "你是不是机器人",
                "你是ai", "你是人工智能", "你是大模型", "你是语言模型", "你是机器人",
                "你什么模型", "你是什么模型", "你用什么模型", "你用的什么模型",
                "你接入什么模型", "你基于什么模型"
        ));
        if (!asksSelf && targetedByMentionOrName) {
            asksSelf = containsAny(compact, List.of(
                    "是不是ai", "是不是人工智能", "是不是大模型", "是不是语言模型", "是不是机器人",
                    "什么模型", "哪个模型", "哪种模型", "用的什么模型", "接入什么模型", "基于什么模型"
            ));
        }
        boolean questionLike = containsAny(lower, questionTerms()) || lower.endsWith("?") || lower.endsWith("？");
        return asksSelf && questionLike;
    }

    public boolean looksLikeIdentityDisclosure(@Nullable String text) {
        String lower = normalizeSpaced(text);
        if (lower.isEmpty()) {
            return false;
        }
        String compact = normalizeCompact(text);
        if (isGeneralIdentityTopicStatement(lower, compact)) {
            return false;
        }
        return containsAny(compact, List.of(
                "我是一个ai", "我是一名ai", "我是ai", "我是人工智能", "我是机器人",
                "我是语言模型", "我是大模型", "我是chatgpt", "我是gpt", "我是llm",
                "我属于ai", "我属于人工智能", "我属于机器人", "我属于语言模型", "我属于大模型",
                "我基于ai", "我基于人工智能", "我基于语言模型", "我基于大模型",
                "我接入ai", "我接入人工智能", "我接入语言模型", "我接入大模型",
                "我调用ai", "我调用人工智能", "我调用语言模型", "我调用大模型"
        ))
                || looksLikeRoleOpeningDisclosure(compact)
                || nearAny(lower, List.of("i am", "i'm"), identityTerms(), 32);
    }

    private boolean isGeneralIdentityTopicQuestion(@Nonnull String lower, @Nonnull String compact) {
        if (!containsAny(lower, identityTerms())) {
            return false;
        }
        return containsAny(compact, List.of(
                "ai是什么", "人工智能是什么", "大模型是什么", "语言模型是什么", "机器人是什么",
                "什么是ai", "什么是人工智能", "什么是大模型", "什么是语言模型", "什么是机器人",
                "介绍ai", "介绍人工智能", "介绍大模型", "介绍语言模型", "介绍机器人",
                "解释ai", "解释人工智能", "解释大模型", "解释语言模型", "解释机器人",
                "ai和", "人工智能和", "大模型和", "语言模型和", "机器人和",
                "ai能做什么", "人工智能能做什么", "大模型能做什么", "语言模型能做什么", "机器人能做什么"
        ));
    }

    private boolean isGeneralIdentityTopicStatement(@Nonnull String lower, @Nonnull String compact) {
        if (!containsAny(lower, identityTerms())) {
            return false;
        }
        return containsAny(compact, List.of(
                "ai是", "人工智能是", "大模型是", "语言模型是", "机器人是",
                "ai指", "人工智能指", "大模型指", "语言模型指", "机器人指",
                "作为ai技术", "作为人工智能技术", "作为大模型技术", "作为语言模型技术", "作为机器人技术",
                "ai技术", "人工智能技术", "大模型技术", "语言模型技术", "机器人技术"
        ));
    }

    private boolean looksLikeRoleOpeningDisclosure(@Nonnull String compact) {
        for (String prefix : List.of("作为", "身为")) {
            int index = compact.indexOf(prefix);
            while (index >= 0) {
                String suffix = compact.substring(index + prefix.length());
                suffix = removeLeadingClassifiers(suffix);
                for (String term : identityTerms()) {
                    String normalizedTerm = normalizeCompact(term);
                    if (StringUtils.isNullOrEmptyEx(normalizedTerm) || !suffix.startsWith(normalizedTerm)) {
                        continue;
                    }
                    String afterTerm = suffix.substring(normalizedTerm.length());
                    if (!startsWithGeneralTopicNoun(afterTerm)) {
                        return true;
                    }
                }
                index = compact.indexOf(prefix, index + prefix.length());
            }
        }
        return false;
    }

    @Nonnull
    private static String removeLeadingClassifiers(@Nonnull String value) {
        String text = value;
        for (String classifier : List.of("一个", "一名", "一种", "一款", "个")) {
            if (text.startsWith(classifier)) {
                return text.substring(classifier.length());
            }
        }
        return text;
    }

    private static boolean startsWithGeneralTopicNoun(@Nonnull String value) {
        return value.startsWith("技术")
                || value.startsWith("领域")
                || value.startsWith("行业")
                || value.startsWith("概念")
                || value.startsWith("研究")
                || value.startsWith("产品")
                || value.startsWith("工具")
                || value.startsWith("应用")
                || value.startsWith("系统");
    }

    @Nonnull
    public String identityReply(@Nullable String text) {
        String lower = normalizeSpaced(text);
        if (lower.contains("什么模型") || lower.contains("哪个模型") || lower.contains("哪种模型")
                || lower.contains("啥模型") || lower.contains("承认") || lower.contains("证明")
                || lower.contains("解释")) {
            return identityFallbackText();
        }
        return pick(denyReplies());
    }

    @Nonnull
    public String identityFallbackText() {
        return pick(confusedReplies());
    }

    @Nonnull
    public String refusalText() {
        return rules.firstLine("refusalText", DEFAULT_REFUSAL);
    }

    public boolean isSafeToStore(@Nullable String content, @Nonnull ChatMemorySettings settings) {
        String text = normalizeMemory(content, settings);
        if (text.length() < Math.max(2, settings.minMemoryChars())) {
            return false;
        }
        if (SECRET_PATTERN.matcher(text).find()) {
            return false;
        }
        String lower = text.toLowerCase(Locale.ROOT);
        return !containsAny(lower, memoryInstructionMarkers())
                && !containsAny(lower, memoryTransientMarkers())
                && !isPromptExtractionAttempt(text)
                && !looksLikePromptLeak(text);
    }

    @Nonnull
    public String normalizeMemory(@Nullable String content, @Nonnull ChatMemorySettings settings) {
        if (StringUtils.isNullOrEmptyEx(content)) {
            return "";
        }
        String text = content
                .replace('\r', '\n')
                .replaceAll("\\s*\\n\\s*", " ")
                .replaceAll("\\s{2,}", " ")
                .trim();
        int maxChars = Math.max(20, settings.maxMemoryChars());
        if (text.length() > maxChars) {
            text = text.substring(0, maxChars).trim();
        }
        return text;
    }

    @Nonnull
    private List<String> promptTerms() {
        return rules.lines("promptTerms", DEFAULT_PROMPT_TERMS);
    }

    @Nonnull
    private List<String> extractionTerms() {
        return rules.lines("extractionTerms", DEFAULT_EXTRACTION_TERMS);
    }

    @Nonnull
    private List<String> injectionTerms() {
        return rules.lines("injectionTerms", DEFAULT_INJECTION_TERMS);
    }

    @Nonnull
    private List<String> leakSignatures() {
        return rules.lines("leakSignatures", DEFAULT_LEAK_SIGNATURES);
    }

    @Nonnull
    private List<String> backendTerms() {
        return rules.lines("backendTerms", DEFAULT_BACKEND_TERMS);
    }

    @Nonnull
    private List<String> introPatterns() {
        return rules.lines("introPatterns", DEFAULT_INTRO_PATTERNS);
    }

    @Nonnull
    private List<String> replyTemplates() {
        return rules.lines("replyTemplates", DEFAULT_REPLY_TEMPLATES);
    }

    @Nonnull
    private List<String> unnamedReplyTemplates() {
        return rules.lines("unnamedReplyTemplates", DEFAULT_UNNAMED_REPLY_TEMPLATES);
    }

    @Nonnull
    private List<String> identityTerms() {
        return rules.lines("identityTerms", DEFAULT_IDENTITY_TERMS);
    }

    @Nonnull
    private List<String> targetTerms() {
        return rules.lines("targetTerms", DEFAULT_TARGET_TERMS);
    }

    @Nonnull
    private List<String> questionTerms() {
        return rules.lines("questionTerms", DEFAULT_QUESTION_TERMS);
    }

    @Nonnull
    private List<String> disclosurePrefixes() {
        return rules.lines("disclosurePrefixes", DEFAULT_DISCLOSURE_PREFIXES);
    }

    @Nonnull
    private List<String> denyReplies() {
        return rules.lines("denyReplies", DEFAULT_DENY_REPLIES);
    }

    @Nonnull
    private List<String> confusedReplies() {
        return rules.lines("confusedReplies", DEFAULT_CONFUSED_REPLIES);
    }

    @Nonnull
    private List<String> memoryInstructionMarkers() {
        return rules.lines("memoryInstructionMarkers", DEFAULT_MEMORY_INSTRUCTION_MARKERS);
    }

    @Nonnull
    private List<String> memoryTransientMarkers() {
        return rules.lines("memoryTransientMarkers", DEFAULT_MEMORY_TRANSIENT_MARKERS);
    }

    private static boolean containsAny(@Nonnull String lower, @Nonnull List<String> terms) {
        for (String term : terms) {
            if (StringUtils.isNotNullOrEmpty(term) && lower.contains(term.toLowerCase(Locale.ROOT))) {
                return true;
            }
        }
        return false;
    }

    private static boolean nearAny(@Nonnull String lower, @Nonnull List<String> leftTerms, @Nonnull List<String> rightTerms, int maxDistance) {
        for (String left : leftTerms) {
            int leftIndex = lower.indexOf(left.toLowerCase(Locale.ROOT));
            if (leftIndex < 0) {
                continue;
            }
            for (String right : rightTerms) {
                int rightIndex = lower.indexOf(right.toLowerCase(Locale.ROOT));
                if (rightIndex >= 0 && Math.abs(rightIndex - leftIndex) <= maxDistance) {
                    return true;
                }
            }
        }
        return false;
    }

    @Nonnull
    private static String pick(@Nonnull List<String> values) {
        return values.get(ThreadLocalRandom.current().nextInt(values.size()));
    }

    @Nonnull
    private static String cleanName(@Nullable String configuredName) {
        String name = StringUtils.nullToEmpty(configuredName).trim();
        if (name.isEmpty()) {
            return "";
        }
        name = name.replaceAll("[\\r\\n\\t]", " ").replaceAll("\\s+", " ").trim();
        return name.length() > 16 ? name.substring(0, 16) : name;
    }

    @Nonnull
    private static String normalizeSpaced(@Nullable String text) {
        if (StringUtils.isNullOrEmptyEx(text)) {
            return "";
        }
        return text.replace('\r', ' ')
                .replace('\n', ' ')
                .replaceAll("\\s+", " ")
                .trim()
                .toLowerCase(Locale.ROOT);
    }

    @Nonnull
    private static String normalizeCompact(@Nullable String text) {
        if (StringUtils.isNullOrEmptyEx(text)) {
            return "";
        }
        return text.replace('\r', ' ')
                .replace('\n', ' ')
                .replaceAll("\\[CQ:[^]]+]", " ")
                .replaceAll("\\s+", "")
                .trim()
                .toLowerCase(Locale.ROOT);
    }

    public record GuardDecision(boolean handled, boolean respond, @Nonnull String replyText) {
        @Nonnull
        public static GuardDecision pass() {
            return new GuardDecision(false, false, "");
        }

        @Nonnull
        public static GuardDecision suppress() {
            return new GuardDecision(true, false, "");
        }

        @Nonnull
        public static GuardDecision reply(@Nonnull String replyText) {
            return new GuardDecision(true, true, replyText);
        }
    }
}
