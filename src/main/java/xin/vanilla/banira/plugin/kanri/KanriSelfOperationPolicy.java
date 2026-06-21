package xin.vanilla.banira.plugin.kanri;

import jakarta.annotation.Nonnull;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.Set;

/**
 * 群友对自己发起群管请求（如禁言自己、修改自己的群名片）时的放行规则。
 */
public final class KanriSelfOperationPolicy {

    private KanriSelfOperationPolicy() {
    }

    /**
     * 允许普通群友请求机器人为自己禁言/解禁。
     * 禁止：机器人不是群管；请求者是 QQ 群主；请求者与机器人同为 QQ 群管。
     */
    public static boolean canAllowSelfMuteOrLoud(@Nonnull BaniraBot bot, long groupId, long memberQq) {
        if (!bot.isGroupOwnerOrAdmin(groupId)) {
            return false;
        }
        if (BaniraUtils.isGroupOwner(bot, groupId, memberQq)) {
            return false;
        }
        return !BaniraUtils.isGroupAdmin(bot, groupId, memberQq);
    }

    public static boolean allowsMute(@Nonnull KanriContext context, @Nonnull String[] args, @Nonnull KanriHandler handler) {
        if (!(handler instanceof MuteCommand)) {
            return false;
        }
        return allowsSingleSelfTarget(context, handler, args);
    }

    public static boolean allowsLoud(@Nonnull KanriContext context, @Nonnull String[] args, @Nonnull KanriHandler handler) {
        if (!(handler instanceof LoudCommand)) {
            return false;
        }
        return allowsSingleSelfTarget(context, handler, args);
    }

    public static boolean allowsCard(@Nonnull KanriContext context, @Nonnull String[] args, @Nonnull KanriHandler handler) {
        if (!(handler instanceof CardCommand)) {
            return false;
        }
        if (!allowsSingleSelfTargetByArgs(context, args)) {
            return false;
        }
        return canAllowSelfCard(context.bot(), context.group(), context.sender());
    }

    public static boolean canAllowSelfCard(@Nonnull BaniraBot bot, long groupId, long memberQq) {
        if (memberQq <= 0) {
            return false;
        }
        if (memberQq == bot.getSelfId()) {
            return true;
        }
        return bot.isGroupOwnerOrAdmin(groupId);
    }

    private static boolean allowsSingleSelfTarget(@Nonnull KanriContext context
            , @Nonnull KanriHandler handler
            , @Nonnull String[] args
    ) {
        if (!allowsSingleSelfTargetByArgs(context, args)) {
            return false;
        }
        return canAllowSelfMuteOrLoud(context.bot(), context.group(), context.sender());
    }

    private static boolean allowsSingleSelfTargetByArgs(@Nonnull KanriContext context, @Nonnull String[] args) {
        // 仅用 args 中的 QQ，避免把当前消息里 @机器人 算进目标导致 size!=1
        Set<Long> targets = BaniraUtils.getUserIds(args);
        if (targets.size() != 1 || targets.contains(233L)) {
            return false;
        }
        long target = targets.iterator().next();
        return target == context.sender();
    }
}
