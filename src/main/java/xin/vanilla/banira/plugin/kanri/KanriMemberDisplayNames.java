package xin.vanilla.banira.plugin.kanri;

import com.mikuac.shiro.dto.action.common.ActionData;
import com.mikuac.shiro.dto.action.response.GroupMemberInfoResp;
import jakarta.annotation.Nonnull;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * 群管 AI 结果里使用的成员展示名（群名片优先，不暴露 QQ）。
 */
public final class KanriMemberDisplayNames {

    private KanriMemberDisplayNames() {
    }

    @Nonnull
    public static String format(@Nonnull BaniraBot bot, long groupId, long qq) {
        if (qq <= 0) {
            return "";
        }
        ActionData<GroupMemberInfoResp> info = bot.getGroupMemberInfo(groupId, qq, false);
        if (bot.isActionDataNotEmpty(info) && info.getData() != null) {
            String card = StringUtils.nullToEmpty(info.getData().getCard()).trim();
            if (!card.isEmpty()) {
                return card;
            }
            String nickname = StringUtils.nullToEmpty(info.getData().getNickname()).trim();
            if (!nickname.isEmpty()) {
                return nickname;
            }
        }
        return "该成员";
    }

    @Nonnull
    public static List<String> formatAll(@Nonnull BaniraBot bot, long groupId, @Nonnull Collection<Long> qqList) {
        List<String> names = new ArrayList<>();
        for (Long qq : qqList) {
            if (qq == null || qq <= 0) {
                continue;
            }
            names.add(format(bot, groupId, qq));
        }
        return names;
    }
}
