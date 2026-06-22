package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;
import xin.vanilla.banira.config.entity.extended.ChatReplySettings;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class StructuredReplyPipeline {

    private static final Pattern FENCED_CODE_BLOCK = Pattern.compile("```[\\s\\S]*?```");

    private StructuredReplyPipeline() {
    }

    @Nonnull
    public static StructuredReply parseAndProcess(@Nonnull String raw
            , @Nonnull List<String> references
            , @Nonnull ChatReplySettings settings
    ) {
        StructuredReply parsed = ReplyContentParser.parse(raw, references);
        StructuredReply normalized = moveLongSpeechToReferences(parsed, settings);
        return new StructuredReply(
                ReplyPostProcessor.process(normalized.speech(), settings),
                normalized.references(),
                normalized.replyToMessageId(),
                normalized.atTargets(),
                normalized.directHandled()
        );
    }

    @Nonnull
    private static StructuredReply moveLongSpeechToReferences(@Nonnull StructuredReply parsed, @Nonnull ChatReplySettings settings) {
        String speech = parsed.speech();
        if (StringUtils.isNullOrEmptyEx(speech)) {
            return parsed;
        }
        int maxForwardLength = dynamicForwardLength(settings);
        boolean hasLongCode = hasLongCodeBlock(speech, Math.max(80, settings.maxCharsPerPart()));
        boolean tooLong = speech.length() > maxForwardLength;
        if (!hasLongCode && !tooLong) {
            return parsed;
        }

        List<String> refs = new ArrayList<>(parsed.references());
        refs.add(cleanReferenceText(speech));
        String summary = summarizeForwardedSpeech(speech, hasLongCode);
        return new StructuredReply(
                summary,
                refs,
                parsed.replyToMessageId(),
                parsed.atTargets(),
                parsed.directHandled()
        );
    }

    private static int dynamicForwardLength(@Nonnull ChatReplySettings settings) {
        int baseLimit = Math.max(100, settings.maxForwardLength());
        int dynamicBudget = MessageSplitter.dynamicSpeechCharBudget(settings.maxCharsPerPart(), settings.maxSplitParts());
        if (dynamicBudget <= 0) {
            return baseLimit;
        }
        int relaxedLimit = Math.min(dynamicBudget, baseLimit * 2);
        return Math.min(dynamicBudget, Math.max(baseLimit, relaxedLimit));
    }

    private static boolean hasLongCodeBlock(@Nonnull String text, int minCodeLength) {
        Matcher matcher = FENCED_CODE_BLOCK.matcher(text);
        while (matcher.find()) {
            if (matcher.group().length() >= minCodeLength) {
                return true;
            }
        }
        return false;
    }

    @Nonnull
    private static String summarizeForwardedSpeech(@Nonnull String speech, boolean codeLike) {
        String withoutCode = FENCED_CODE_BLOCK.matcher(speech).replaceAll("\n").trim();
        for (String line : withoutCode.split("\\R")) {
            String trimmed = line.trim();
            if (StringUtils.isNullOrEmptyEx(trimmed)) {
                continue;
            }
            if (trimmed.length() <= 90) {
                return trimmed;
            }
            return trimmed.substring(0, 90).trim();
        }
        return codeLike ? "代码放合并转发里了" : "内容有点长，我放合并转发里了";
    }

    @Nonnull
    public static String cleanReferenceText(@Nonnull String text) {
        String cleaned = text.trim();
        cleaned = cleaned.replaceAll("(?m)^```[a-zA-Z0-9_+\\-.#]*\\s*$", "");
        cleaned = cleaned.replaceAll("(?m)^```\\s*$", "");
        cleaned = stripReferenceTitlePrefix(cleaned);
        cleaned = cleaned.replaceAll("\\n{3,}", "\n\n").trim();
        return cleaned;
    }

    @Nonnull
    private static String stripReferenceTitlePrefix(@Nonnull String text) {
        String cleaned = text.stripLeading();
        return cleaned.replaceFirst("(?is)^(?:完整内容|详细内容|总结|摘要|搜索结果|网页搜索结果|网页搜索资料|资料整理)\\s*[:：]\\s*", "").stripLeading();
    }

    public static boolean isInternalReferenceText(@Nonnull String text) {
        String cleaned = cleanReferenceText(text);
        if (StringUtils.isNullOrEmptyEx(cleaned)) {
            return true;
        }
        String lower = cleaned.toLowerCase(java.util.Locale.ROOT);
        if (lower.contains("the request was rejected")
                || lower.contains("considered high risk")
                || lower.contains("content policy")
                || lower.contains("safety policy")) {
            return true;
        }
        if (cleaned.contains("当前最新消息") && (cleaned.contains("已阻止") || cleaned.contains("不允许") || cleaned.contains("没有明确"))) {
            return true;
        }
        if (cleaned.length() <= 500) {
            return cleaned.contains("工具执行失败")
                    || cleaned.contains("未知工具：")
                    || cleaned.contains("合并转发内容为空")
                    || cleaned.contains("内容较长，正文不要重复贴代码")
                    || cleaned.contains("最终回复只用一句短话")
                    || cleaned.startsWith("代码文件\n\n已上传：")
                    || cleaned.contains("消息记录只供理解上下文")
                    || cleaned.contains("网页搜索关键词为空")
                    || cleaned.contains("搜索关键词不能为空")
                    || cleaned.contains("没搜到可用结果")
                    || cleaned.contains("当前配置不允许 AI 主动写入记忆")
                    || cleaned.contains("这条内容不适合写入长期记忆")
                    || cleaned.contains("这条称呼记忆容易串人")
                    || cleaned.contains("处理步骤过多")
                    || cleaned.contains("未能完成回复");
        }
        if (cleaned.contains("消息记录只供理解上下文")
                || cleaned.contains("相关长期记忆")
                || cleaned.contains("重要长期记忆")
                || cleaned.contains("轻量会话记忆")
                || cleaned.contains("不是长期设定")
                || cleaned.contains("type:episodic")
                || cleaned.contains("bot_said")
                || cleaned.contains("记忆按 qq 绑定")
                || cleaned.contains("仅作事实参考")
                || cleaned.contains("是否你自己发送=")
                || cleaned.contains("显示名可能被修改或冒用")
                || cleaned.contains("AI 可调用的群管动作")
                || cleaned.contains("listCapabilities")
                || cleaned.contains("能力列表")
                || cleaned.contains("参数：action=")) {
            return true;
        }
        return false;
    }

    public static boolean isUnsuitableForwardReferenceText(@Nonnull String text) {
        String cleaned = cleanReferenceText(text);
        return isInternalReferenceText(cleaned) || looksLikeRawWebSearchReference(cleaned);
    }

    public static boolean looksLikeRawWebSearchReference(@Nonnull String text) {
        String cleaned = cleanReferenceText(text);
        if (StringUtils.isNullOrEmptyEx(cleaned)) {
            return false;
        }
        boolean searchHeader = cleaned.startsWith("查询：")
                || cleaned.startsWith("网页搜索")
                || cleaned.startsWith("搜索结果");
        boolean numberedList = Pattern.compile("(?m)^\\s*\\d+[.、]\\s+").matcher(cleaned).find();
        boolean searchFields = Pattern.compile("(?m)^\\s*(摘要|链接)：").matcher(cleaned).find();
        return searchHeader && (numberedList || searchFields)
                || numberedList && searchFields;
    }
}
