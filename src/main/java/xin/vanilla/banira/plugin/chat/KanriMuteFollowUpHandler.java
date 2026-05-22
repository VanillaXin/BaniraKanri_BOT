package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import lombok.extern.slf4j.Slf4j;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.plugin.chat.agent.AgentContext;
import xin.vanilla.banira.plugin.chat.capability.PendingAiActionStore;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.kanri.KanriExecuteReport;
import xin.vanilla.banira.plugin.kanri.KanriService;
import xin.vanilla.banira.util.StringUtils;

import java.util.List;

/**
 * 禁言跟进：提议确认、补时长、再试试等场景下先执行群管，结果写入 {@link AgentContext#kanriFeedback()} 供 LLM 口语化回复。
 * 解析失败时不编造用户可见话术，交回 Agent 主链路处理。
 */
@Slf4j
public final class KanriMuteFollowUpHandler {

    private KanriMuteFollowUpHandler() {
    }

    /**
     * @return 是否已执行群管并写入 feedback
     */
    public static boolean tryExecute(@Nonnull KanriService kanriService
            , @Nonnull BaniraBot bot
            , @Nonnull BaniraCodeContext ctx
            , @Nonnull AgentContext agentContext
            , @Nonnull List<MessageRecord> records
            , boolean directMentioned
    ) {
        long groupId = ctx.group() != null ? ctx.group() : 0L;
        long botId = bot.getSelfId();
        long operatorId = ctx.sender() != null ? ctx.sender() : 0L;
        String message = ctx.msg();
        if (groupId <= 0 || operatorId <= 0 || StringUtils.isNullOrEmptyEx(message)) {
            return false;
        }
        if (!shouldHandle(message, botId, groupId, records, directMentioned)) {
            return false;
        }

        PendingKanriProposalStore.KanriProposal proposal = PendingKanriProposalStore.peek(botId, groupId);
        if (proposal == null) {
            proposal = proposalFromHistory(records, botId);
        }

        KanriMuteTargetResolver.ResolvedMute resolved = KanriMuteTargetResolver.resolve(
                bot, groupId, botId, operatorId, records, message, proposal);
        if (resolved == null) {
            LOGGER.debug("kanri mute follow-up unresolved group={} sender={}", groupId, operatorId);
            return false;
        }

        KanriExecuteReport report = kanriService.executeMuteReport(agentContext, resolved.targets(), resolved.minutes());
        if (report.hasSucceeded()) {
            PendingKanriProposalStore.consume(botId, groupId);
            agentContext.kanriMuteSucceeded(true);
        }
        agentContext.kanriFeedback(report.toAgentMessage());
        LOGGER.info("kanri mute follow-up executed group={} operator={} targets={} minutes={} result={} success={} partial={} feedback={}",
                groupId, operatorId, resolved.targets().size(), resolved.minutes(),
                report.resultCode(), report.hasSucceeded(), report.isPartialSuccess(), report.toAgentMessage());
        return true;
    }

    private static boolean shouldHandle(@Nullable String message
            , long botId
            , long groupId
            , @Nonnull List<MessageRecord> records
            , boolean directMentioned
    ) {
        if (StringUtils.isNullOrEmptyEx(message)) {
            return false;
        }
        if (PendingAiActionStore.isKanriProceedIntent(message)) {
            return PendingKanriProposalStore.hasMute(botId, groupId)
                    || proposalFromHistory(records, botId) != null;
        }
        if (PendingAiActionStore.isKanriRetryIntent(message) && directMentioned) {
            return true;
        }
        if (directMentioned && PendingKanriProposalStore.hasMute(botId, groupId)) {
            return true;
        }
        if (ChatConversationSignals.botAskedMuteDuration(records, botId)
                && KanriMuteIntent.hasMuteDurationOnlyReply(message)) {
            return directMentioned || PendingKanriProposalStore.hasMute(botId, groupId) || proposalFromHistory(records, botId) != null;
        }
        return false;
    }

    @Nullable
    private static PendingKanriProposalStore.KanriProposal proposalFromHistory(@Nonnull List<MessageRecord> records
            , long botId
    ) {
        for (int i = records.size() - 1; i >= 0; i--) {
            MessageRecord record = records.get(i);
            if (record == null || record.getSenderId() == null || record.getSenderId() != botId) {
                continue;
            }
            String speech = record.getMsgRecode();
            if (!KanriProposalParser.containsMuteProposal(speech)) {
                continue;
            }
            List<String> keywords = KanriProposalParser.extractMuteTargetKeywords(speech);
            if (keywords.isEmpty()) {
                return null;
            }
            return new PendingKanriProposalStore.KanriProposal(
                    keywords,
                    KanriProposalParser.extractMuteMinutes(speech, 10),
                    java.time.Instant.now()
            );
        }
        return null;
    }
}
