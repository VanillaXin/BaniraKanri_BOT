package xin.vanilla.banira.plugin.chat;

import com.mikuac.shiro.dto.action.common.ActionList;
import com.mikuac.shiro.dto.action.response.GroupMemberInfoResp;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.util.StringUtils;

import java.util.*;

/**
 * 按群名片/昵称关键词在成员列表中查 QQ。
 */
public final class GroupMemberKeywordResolver {

    private GroupMemberKeywordResolver() {
    }

    @Nonnull
    public static List<Long> findMemberQq(@Nonnull BaniraBot bot, long groupId, @Nonnull String keyword) {
        if (StringUtils.isNullOrEmptyEx(keyword) || groupId <= 0) {
            return List.of();
        }
        ActionList<GroupMemberInfoResp> members = bot.getGroupMemberList(groupId);
        if (!bot.isActionDataNotEmpty(members)) {
            return List.of();
        }
        String needle = normalizeKeyword(keyword);
        Set<Long> matched = new LinkedHashSet<>();
        for (GroupMemberInfoResp member : members.getData()) {
            if (member == null) {
                continue;
            }
            if (memberMatches(member, needle)) {
                matched.add(member.getUserId());
            }
        }
        return new ArrayList<>(matched);
    }

    private static boolean memberMatches(@Nonnull GroupMemberInfoResp member, @Nonnull String needle) {
        return normalizeKeyword(StringUtils.nullToEmpty(member.getCard())).contains(needle)
                || normalizeKeyword(StringUtils.nullToEmpty(member.getNickname())).contains(needle)
                || String.valueOf(member.getUserId()).contains(needle);
    }

    @Nonnull
    private static String normalizeKeyword(@Nullable String value) {
        if (value == null) {
            return "";
        }
        return value.replace("·", "")
                .replace("•", "")
                .replace(" ", "")
                .trim()
                .toLowerCase(Locale.ROOT);
    }
}
