package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 从会话上下文解析禁言目标 QQ 与时长。
 */
public final class KanriMuteTargetResolver {

    private static final Pattern CQ_AT = Pattern.compile("\\[CQ:at,qq=(\\d+)]");

    private KanriMuteTargetResolver() {
    }

    @Nullable
    public static ResolvedMute resolve(@Nonnull BaniraBot bot
            , long groupId
            , long botId
            , long operatorId
            , @Nonnull List<MessageRecord> records
            , @Nullable String currentMessage
            , @Nullable PendingKanriProposalStore.KanriProposal proposal
    ) {
        if (groupId <= 0 || StringUtils.isNullOrEmptyEx(currentMessage)) {
            return null;
        }

        Set<Long> targets = new LinkedHashSet<>();
        if (proposal != null && !proposal.targetKeywords().isEmpty()) {
            for (String keyword : proposal.targetKeywords()) {
                targets.addAll(GroupMemberKeywordResolver.findMemberQq(bot, groupId, keyword));
            }
        }
        if (targets.isEmpty()) {
            targets.addAll(atTargets(currentMessage, botId));
        }
        if (targets.isEmpty()) {
            return null;
        }

        int minutes = resolveMinutes(records, botId, currentMessage, proposal);
        return new ResolvedMute(new ArrayList<>(targets), minutes);
    }

    @Nonnull
    private static Set<Long> atTargets(@Nonnull String message, long botId) {
        Set<Long> targets = new LinkedHashSet<>();
        Matcher matcher = CQ_AT.matcher(message);
        while (matcher.find()) {
            long qq = Long.parseLong(matcher.group(1));
            if (qq > 0 && qq != botId) {
                targets.add(qq);
            }
        }
        return targets;
    }

    private static int resolveMinutes(@Nonnull List<MessageRecord> records
            , long botId
            , @Nonnull String currentMessage
            , @Nullable PendingKanriProposalStore.KanriProposal proposal
    ) {
        int fromCurrent = KanriMuteIntent.extractMinutes(currentMessage, 0);
        if (fromCurrent > 0) {
            return fromCurrent;
        }
        if (proposal != null) {
            return proposal.minutes();
        }
        for (int i = records.size() - 1; i >= 0; i--) {
            MessageRecord record = records.get(i);
            if (record == null || record.getSenderId() == null || record.getSenderId() == botId) {
                continue;
            }
            String speech = StringUtils.nullToEmpty(record.getMsgRecode());
            if (!KanriMuteIntent.hasMuteDurationOnlyReply(speech)) {
                continue;
            }
            int minutes = KanriMuteIntent.extractMinutes(speech, 0);
            if (minutes > 0) {
                return minutes;
            }
        }
        return 10;
    }

    public record ResolvedMute(@Nonnull List<Long> targets, int minutes) {
    }
}
