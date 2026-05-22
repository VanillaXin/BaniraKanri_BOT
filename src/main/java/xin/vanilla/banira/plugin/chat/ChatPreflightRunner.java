package xin.vanilla.banira.plugin.chat;

import dev.langchain4j.data.message.ChatMessage;
import dev.langchain4j.data.message.SystemMessage;
import dev.langchain4j.model.chat.response.ChatResponse;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import lombok.extern.slf4j.Slf4j;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.config.entity.extended.ChatEngagementSettings;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.enums.EnumMessageType;
import xin.vanilla.banira.plugin.chat.agent.PromptTemplateLoader;
import xin.vanilla.banira.plugin.chat.capability.PendingAiActionStore;
import xin.vanilla.banira.plugin.chat.model.ChatModelRouter;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * 轻量 LLM 预检：决定是否进入完整 Agent。
 */
@Slf4j
public class ChatPreflightRunner {

    private static final String PREFLIGHT_GATE = "prompt/aichat/preflight-gate.txt";

    private ChatPreflightRunner() {
    }

    public static boolean needsPreflight(@Nonnull BaniraCodeContext ctx
            , boolean directMentioned
            , boolean nameMentioned
            , boolean botNamePrefixMentioned
            , @Nonnull ChatEngagementSettings settings
            , int followInterest
            , @Nonnull List<MessageRecord> records
            , long botId
    ) {
        if (!settings.enabled() || !settings.preflightEnabled()) {
            return false;
        }
        if (ctx.msgType() != EnumMessageType.GROUP) {
            return false;
        }
        if (directMentioned || botNamePrefixMentioned) {
            return false;
        }
        if (nameMentioned && looksLikeNameAddressedRequest(ctx.msg())) {
            return false;
        }
        if (followInterest >= settings.followInterestThreshold()) {
            return false;
        }
        if (PendingAiActionStore.isConfirmationText(ctx.msg())) {
            return false;
        }
        if (PendingAiActionStore.isKanriProceedIntent(ctx.msg())) {
            return false;
        }
        long groupId = ctx.group() != null ? ctx.group() : 0L;
        if (directMentioned && PendingKanriProposalStore.hasMute(botId, groupId)) {
            return false;
        }
        if (isProceedingMuteConfirmation(records, botId, ctx.msg())) {
            return false;
        }
        if (ChatConversationSignals.isShortReplyToBotQuestion(records, botId, ctx.msg())) {
            return false;
        }
        return true;
    }

    private static boolean looksLikeNameAddressedRequest(@Nullable String message) {
        if (StringUtils.isNullOrEmptyEx(message)) {
            return false;
        }
        String text = message.replaceAll("\\[CQ:[^]]+]", " ").replaceAll("\\s+", "");
        if (text.length() <= 2) {
            return true;
        }
        return text.contains("帮")
                || text.contains("看看")
                || text.contains("看下")
                || text.contains("锐评")
                || text.contains("评价")
                || text.contains("分析")
                || text.contains("解释")
                || text.contains("搜")
                || text.contains("查")
                || text.contains("来")
                || text.contains("在吗")
                || text.contains("干嘛")
                || text.contains("为什么")
                || text.contains("怎么")
                || text.contains("能不能")
                || text.contains("可以")
                || text.endsWith("?")
                || text.endsWith("？");
    }

    private static boolean isProceedingMuteConfirmation(@Nonnull List<MessageRecord> records
            , long botId
            , @Nullable String currentMessage
    ) {
        if (!PendingAiActionStore.isKanriProceedIntent(currentMessage)) {
            return false;
        }
        for (int i = records.size() - 1; i >= 0; i--) {
            MessageRecord record = records.get(i);
            if (record == null || record.getSenderId() == null || record.getSenderId() != botId) {
                continue;
            }
            String speech = record.getMsgRecode();
            return ChatConversationSignals.awaitsUserReply(speech)
                    || KanriProposalParser.containsMuteProposal(speech);
        }
        return false;
    }

    @Nonnull
    public static PreflightMetaParser.PreflightDecision evaluate(@Nonnull BaniraBot bot
            , @Nonnull BaniraCodeContext ctx
            , @Nonnull List<MessageRecord> records
            , @Nonnull ChatConfig cfg
            , @Nonnull ChatModelRouter router
            , int followInterest
    ) {
        ChatEngagementSettings settings = cfg.engagement();
        List<ChatMessage> prompt = buildPrompt(bot, ctx, records, cfg, followInterest);
        try {
            ChatResponse response = router.chatViaEndpoint(
                    prompt,
                    settings.preflightEndpointName(),
                    settings.preflightTemperature()
            );
            String text = response.aiMessage() != null ? response.aiMessage().text() : "";
            PreflightMetaParser.PreflightDecision decision = PreflightMetaParser.parse(StringUtils.nullToEmpty(text));
            LOGGER.debug("preflight decision group={} sender={} invoke={} interest={}",
                    ctx.group(), ctx.sender(), decision.invoke(), decision.interest());
            return decision;
        } catch (Exception e) {
            if (ChatSafetyRejectionTracker.looksLikeProviderSafetyRejection(e)) {
                ChatSafetyRejectionTracker.markPromptMessages(ctx, records);
            }
            LOGGER.warn("preflight failed group={} sender={}, fallback invoke", ctx.group(), ctx.sender(), e);
            return PreflightMetaParser.PreflightDecision.invoke(followInterest);
        }
    }

    @Nonnull
    private static List<ChatMessage> buildPrompt(@Nonnull BaniraBot bot
            , @Nonnull BaniraCodeContext ctx
            , @Nonnull List<MessageRecord> records
            , @Nonnull ChatConfig cfg
            , int followInterest
    ) {
        List<ChatMessage> prompt = new ArrayList<>();
        PromptTemplateLoader.loadSections(PREFLIGHT_GATE).forEach(text -> prompt.add(SystemMessage.from(text)));
        if (followInterest > 0) {
            prompt.add(SystemMessage.from("你当前对本群对话的关注度约 " + followInterest + "/100"));
        }
        ChatMessageContextFormatter.UserInfoCache cache = new ChatMessageContextFormatter.UserInfoCache();
        int limit = Math.max(3, cfg.engagement().preflightHistoryLimit());
        String history = buildCompactHistory(bot, records, cache, limit);
        if (StringUtils.isNotNullOrEmpty(history)) {
            prompt.add(SystemMessage.from(history));
        }
        prompt.add(SystemMessage.from("当前消息：\n" + StringUtils.nullToEmpty(ctx.msg())));
        return prompt;
    }

    @Nonnull
    private static String buildCompactHistory(@Nonnull BaniraBot bot
            , @Nonnull List<MessageRecord> records
            , @Nonnull ChatMessageContextFormatter.UserInfoCache cache
            , int limit
    ) {
        if (records.isEmpty()) {
            return "";
        }
        int start = Math.max(0, records.size() - limit);
        StringBuilder builder = new StringBuilder("最近群聊摘要：\n");
        for (int i = start; i < records.size(); i++) {
            MessageRecord record = records.get(i);
            if (record == null || StringUtils.isNullOrEmptyEx(record.getMsgRecode())) {
                continue;
            }
            String body = ChatPromptAssembler.buildHistoryLog(bot, List.of(record), cache);
            if (StringUtils.isNotNullOrEmpty(body)) {
                builder.append(body).append('\n');
            }
        }
        return builder.toString().trim();
    }
}
