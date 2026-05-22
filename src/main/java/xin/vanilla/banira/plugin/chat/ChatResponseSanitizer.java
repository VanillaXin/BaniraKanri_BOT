package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

@Slf4j
public class ChatResponseSanitizer {

    private final ChatConfig cfg;
    private final ChatGuardService guard;

    public ChatResponseSanitizer(@Nonnull ChatConfig cfg, @Nonnull ChatGuardService guard) {
        this.cfg = cfg;
        this.guard = guard;
    }

    @Nonnull
    public StructuredReply sanitize(@Nonnull BaniraCodeContext ctx, String replyText, @Nonnull List<String> toolReferences) {
        String text = StringUtils.isNotNullOrEmpty(replyText) ? replyText : "";
        if (containsPromptLeakReference(toolReferences)) {
            LOGGER.warn("blocked prompt-leak forward reference group={} sender={}", ctx.group(), ctx.sender());
            return new StructuredReply(guard.refusalText(), List.of());
        }
        List<String> references = filterReferencesForCurrentMessage(ctx, toolReferences);
        if (looksLikeProviderRejection(text)) {
            LOGGER.warn("suppressed provider rejection response group={} sender={}", ctx.group(), ctx.sender());
            return new StructuredReply("", List.of());
        }
        if (StructuredReplyPipeline.isInternalReferenceText(text)) {
            LOGGER.debug("suppressed internal tool/policy response group={} sender={}", ctx.group(), ctx.sender());
            return new StructuredReply("", List.of());
        }
        if (looksLikeInternalContextSpeech(text)) {
            LOGGER.debug("suppressed internal context speech group={} sender={}", ctx.group(), ctx.sender());
            return new StructuredReply("", List.of());
        }
        if (guard.looksLikePromptLeak(text)) {
            LOGGER.warn("blocked suspicious prompt-leak response group={} sender={}", ctx.group(), ctx.sender());
            return new StructuredReply(guard.refusalText(), List.of());
        }
        if (guard.looksLikeIdentityDisclosure(text)) {
            LOGGER.debug("suppressed identity disclosure response group={} sender={}", ctx.group(), ctx.sender());
            return new StructuredReply("", List.of());
        }
        if (looksLikeMechanicalFailure(text)) {
            LOGGER.debug("suppressed mechanical failure response group={} sender={}", ctx.group(), ctx.sender());
            return new StructuredReply("", List.of());
        }
        if (looksLikeToolInstructionParaphrase(text)) {
            LOGGER.debug("suppressed tool-instruction paraphrase group={} sender={}", ctx.group(), ctx.sender());
            return new StructuredReply("", List.of());
        }
        if (looksLikeKanriSurrender(text)) {
            LOGGER.debug("suppressed kanri surrender speech group={} sender={}", ctx.group(), ctx.sender());
            return new StructuredReply("", List.of());
        }
        if (looksLikeRawWebSearchDump(text)) {
            LOGGER.debug("blocked raw web-search dump group={} sender={}", ctx.group(), ctx.sender());
            references.clear();
            return new StructuredReply("", List.of());
        }

        StructuredReply structured = StructuredReplyPipeline.parseAndProcess(text, references, cfg.reply());
        structured = filterStructuredReferences(ctx, structured);
        if (guard.looksLikePromptLeak(structured.speech())) {
            LOGGER.warn("blocked suspicious prompt-leak speech group={} sender={}", ctx.group(), ctx.sender());
            return new StructuredReply(guard.refusalText(), List.of());
        }
        if (looksLikeInternalContextSpeech(structured.speech())) {
            LOGGER.debug("suppressed internal context speech group={} sender={}", ctx.group(), ctx.sender());
            return new StructuredReply("", List.of());
        }
        if (guard.looksLikeIdentityDisclosure(structured.speech())) {
            LOGGER.debug("suppressed identity disclosure speech group={} sender={}", ctx.group(), ctx.sender());
            return new StructuredReply("", List.of());
        }
        return structured;
    }

    private boolean containsPromptLeakReference(@Nonnull List<String> references) {
        for (String reference : references) {
            if (StringUtils.isNotNullOrEmpty(reference)
                    && guard.looksLikePromptLeak(StructuredReplyPipeline.cleanReferenceText(reference))) {
                return true;
            }
        }
        return false;
    }

    @Nonnull
    private List<String> filterReferencesForCurrentMessage(@Nonnull BaniraCodeContext ctx, @Nonnull List<String> references) {
        List<String> cleanedReferences = new ArrayList<>();
        for (String reference : references) {
            if (StringUtils.isNullOrEmptyEx(reference)) {
                continue;
            }
            String cleaned = StructuredReplyPipeline.cleanReferenceText(reference);
            if (guard.looksLikePromptLeak(cleaned)) {
                LOGGER.warn("suppressed prompt-leak forward reference group={} sender={}", ctx.group(), ctx.sender());
                continue;
            }
            if (StructuredReplyPipeline.isUnsuitableForwardReferenceText(cleaned)) {
                LOGGER.debug("suppressed unsuitable forward reference group={} sender={}", ctx.group(), ctx.sender());
                continue;
            }
            cleanedReferences.add(cleaned);
        }
        if (cleanedReferences.isEmpty() || !shouldSuppressImplicitWebSearchReferences(ctx.msg(), cleanedReferences)) {
            return cleanedReferences;
        }
        List<String> filtered = new ArrayList<>();
        for (String reference : cleanedReferences) {
            if (!looksLikeWebSearchReference(reference) || looksLikeStrongWebSearchReference(ctx.msg(), reference)) {
                filtered.add(reference);
            }
        }
        if (filtered.size() != cleanedReferences.size()) {
            LOGGER.debug("suppressed weak implicit web-search references group={} sender={} before={} after={}",
                    ctx.group(), ctx.sender(), cleanedReferences.size(), filtered.size());
        }
        return filtered;
    }

    @Nonnull
    private StructuredReply filterStructuredReferences(@Nonnull BaniraCodeContext ctx, @Nonnull StructuredReply structured) {
        List<String> filtered = filterReferencesForCurrentMessage(ctx, structured.references());
        return new StructuredReply(
                structured.speech(),
                filtered,
                structured.replyToMessageId(),
                structured.atTargets(),
                structured.directHandled()
        );
    }

    private static boolean shouldSuppressImplicitWebSearchReferences(String message, @Nonnull List<String> references) {
        if (StringUtils.isNullOrEmptyEx(message) || references.isEmpty()) {
            return false;
        }
        if (hasExplicitSearchIntent(message) || !looksLikeExplanationQuestion(message)) {
            return false;
        }
        for (String reference : references) {
            if (looksLikeWebSearchReference(reference) && !looksLikeStrongWebSearchReference(message, reference)) {
                return true;
            }
        }
        return false;
    }

    private static boolean hasExplicitSearchIntent(String message) {
        String text = normalizeForIntent(message);
        String lower = text.toLowerCase(java.util.Locale.ROOT);
        return text.contains("搜")
                || text.contains("查")
                || text.contains("查询")
                || text.contains("查找")
                || text.contains("检索")
                || text.contains("资料")
                || text.contains("网页")
                || lower.contains("search")
                || lower.contains("google")
                || lower.contains("bing");
    }

    private static boolean looksLikeExplanationQuestion(String message) {
        String text = normalizeForIntent(message);
        return text.contains("是什么")
                || text.contains("什么是")
                || text.contains("是啥")
                || text.contains("啥是")
                || text.contains("怎么样")
                || text.contains("怎么回事")
                || text.contains("什么意思")
                || text.contains("啥意思")
                || text.contains("介绍一下")
                || text.contains("讲讲")
                || text.contains("说说");
    }

    private static boolean looksLikeWebSearchReference(String text) {
        if (StringUtils.isNullOrEmptyEx(text)) {
            return false;
        }
        String trimmed = text.trim();
        return trimmed.startsWith("查询：")
                || trimmed.startsWith("网页搜索")
                || trimmed.startsWith("搜索结果")
                || (Pattern.compile("(?m)^\\s*1[.、]").matcher(trimmed).find()
                && Pattern.compile("(?m)^\\s*(摘要|链接)：").matcher(trimmed).find());
    }

    private static boolean looksLikeStrongWebSearchReference(String message, String reference) {
        String subject = coreSearchSubject(message);
        if (subject.length() < 2) {
            return false;
        }
        String haystack = normalizeForCompare(firstSearchTitle(reference) + " " + firstSnippet(reference));
        String normalizedSubject = normalizeForCompare(subject);
        if (normalizedSubject.length() >= 2 && haystack.contains(normalizedSubject)) {
            return true;
        }
        List<String> tokens = importantTokens(normalizedSubject);
        if (tokens.isEmpty()) {
            return false;
        }
        long matched = tokens.stream().filter(haystack::contains).count();
        return matched >= Math.min(tokens.size(), 2);
    }

    @Nonnull
    private static String firstSearchTitle(String reference) {
        if (StringUtils.isNullOrEmptyEx(reference)) {
            return "";
        }
        java.util.regex.Matcher matcher = Pattern.compile("(?m)^\\s*\\d+[.、]\\s*(.+)$").matcher(reference);
        return matcher.find() ? matcher.group(1).trim() : "";
    }

    @Nonnull
    private static String firstSnippet(String reference) {
        if (StringUtils.isNullOrEmptyEx(reference)) {
            return "";
        }
        java.util.regex.Matcher matcher = Pattern.compile("(?m)^\\s*摘要：\\s*(.+)$").matcher(reference);
        return matcher.find() ? matcher.group(1).trim() : "";
    }

    @Nonnull
    private static String coreSearchSubject(String message) {
        String text = normalizeForCompare(message);
        text = text.replaceAll("\\[CQ:[^]]+]", " ")
                .replaceAll("@\\S+", " ")
                .replace("是什么", " ")
                .replace("什么是", " ")
                .replace("是啥", " ")
                .replace("啥是", " ")
                .replace("怎么样", " ")
                .replace("怎么回事", " ")
                .replace("什么意思", " ")
                .replace("啥意思", " ")
                .replace("介绍一下", " ")
                .replace("讲讲", " ")
                .replace("说说", " ")
                .replace("是", " ")
                .replace("的", " ")
                .replace("吗", " ")
                .replace("呢", " ")
                .replace("啊", " ");
        return text.replaceAll("\\s+", " ").trim();
    }

    @Nonnull
    private static List<String> importantTokens(String text) {
        if (StringUtils.isNullOrEmptyEx(text)) {
            return List.of();
        }
        List<String> tokens = new ArrayList<>();
        for (String token : text.split("\\s+")) {
            String trimmed = token.trim();
            if (trimmed.length() >= 2 && !isGenericSearchWord(trimmed)) {
                tokens.add(trimmed);
            }
        }
        if (tokens.isEmpty() && text.length() >= 2 && text.length() <= 8) {
            tokens.add(text);
        }
        return tokens;
    }

    private static boolean isGenericSearchWord(String token) {
        return "模组".equals(token)
                || "整合包".equals(token)
                || "游戏".equals(token)
                || "东西".equals(token)
                || "内容".equals(token)
                || "信息".equals(token)
                || "资料".equals(token)
                || "介绍".equals(token);
    }

    @Nonnull
    private static String normalizeForIntent(String message) {
        return StringUtils.isNotNullOrEmpty(message) ? message.replaceAll("\\s+", "") : "";
    }

    @Nonnull
    private static String normalizeForCompare(String message) {
        if (StringUtils.isNullOrEmptyEx(message)) {
            return "";
        }
        return message.toLowerCase(java.util.Locale.ROOT)
                .replaceAll("[\\p{Punct}\\u3000-\\u303F\\uFF00-\\uFFEF]+", " ")
                .replaceAll("\\s+", " ")
                .trim();
    }

    private static boolean looksLikeRawWebSearchDump(String text) {
        if (StringUtils.isNullOrEmptyEx(text)) {
            return false;
        }
        String trimmed = text.trim();
        boolean searchHeader = trimmed.startsWith("网页搜索结果")
                || trimmed.startsWith("网页搜索资料")
                || trimmed.startsWith("搜索结果")
                || trimmed.startsWith("查询：");
        boolean listLike = Pattern.compile("(?m)^\\s*1[.、]").matcher(trimmed).find();
        boolean searchFields = Pattern.compile("(?m)^\\s*(摘要|链接)：").matcher(trimmed).find();
        return searchHeader && (listLike || searchFields);
    }

    private static boolean looksLikeInternalContextSpeech(String text) {
        if (StringUtils.isNullOrEmptyEx(text)) {
            return false;
        }
        String compact = text.replaceAll("\\s+", "");
        return compact.startsWith("引用消息[msgId=")
                || compact.startsWith("引用消息：消息ID=")
                || compact.startsWith("发送者：稳定身份qq=")
                || compact.startsWith("轻量会话记忆")
                || compact.startsWith("重要长期记忆")
                || compact.startsWith("相关长期记忆")
                || compact.contains("稳定身份qq=") && compact.contains("显示名可能被修改或冒用")
                || compact.contains("[轻量会话记忆；你曾说过；不是长期设定]")
                || compact.contains("type:episodic")
                || compact.contains("source:auto_reply")
                || compact.contains("bot_said")
                || compact.contains("[ENGAGEreply=")
                || compact.contains("[PREFLIGHTinvoke=");
    }

    private static boolean looksLikeMechanicalFailure(String text) {
        if (StringUtils.isNullOrEmptyEx(text)) {
            return false;
        }
        String trimmed = text.trim();
        return trimmed.contains("处理步骤过多")
                || trimmed.contains("未能完成回复，请简化问题")
                || trimmed.contains("未能完成回复")
                || trimmed.contains("请简化问题或稍后再试");
    }

    private static boolean looksLikeToolInstructionParaphrase(String text) {
        if (StringUtils.isNullOrEmptyEx(text)) {
            return false;
        }
        String trimmed = text.trim();
        return trimmed.contains("工具需要")
                || trimmed.contains("明确指令")
                || trimmed.contains("类似的话")
                || trimmed.contains("固定指令")
                || trimmed.contains("内部规则")
                || (trimmed.contains("请直接说") && trimmed.contains("禁言"))
                || (trimmed.contains("直接说") && trimmed.contains("禁言") && trimmed.contains("分钟"));
    }

    private static boolean looksLikeKanriSurrender(String text) {
        if (StringUtils.isNullOrEmptyEx(text)) {
            return false;
        }
        String trimmed = text.trim();
        return (trimmed.contains("手动") && trimmed.contains("禁"))
                || trimmed.contains("群设置里")
                || trimmed.contains("系统那边卡住")
                || trimmed.contains("一直没成功")
                || trimmed.contains("没有禁言权限")
                || trimmed.contains("没有禁言的权限")
                || trimmed.contains("需要管理员或群主操作")
                || trimmed.contains("操作被拦住")
                || (trimmed.contains("三个号") && trimmed.contains("："));
    }

    private static boolean looksLikeProviderRejection(String text) {
        return ChatSafetyRejectionTracker.looksLikeProviderSafetyText(text);
    }
}
