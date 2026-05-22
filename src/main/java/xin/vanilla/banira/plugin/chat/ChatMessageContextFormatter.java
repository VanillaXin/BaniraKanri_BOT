package xin.vanilla.banira.plugin.chat;

import com.mikuac.shiro.dto.action.common.ActionData;
import com.mikuac.shiro.dto.action.response.GroupMemberInfoResp;
import com.mikuac.shiro.dto.action.response.StrangerInfoResp;
import com.mikuac.shiro.model.ArrayMsg;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public final class ChatMessageContextFormatter {

    private ChatMessageContextFormatter() {
    }

    @Nonnull
    public static String senderPrefix(@Nonnull BaniraBot bot, @Nullable Long groupId, @Nullable Long senderId) {
        return senderPrefix(bot, groupId, senderId, null);
    }

    @Nonnull
    public static String senderPrefix(@Nonnull BaniraBot bot, @Nullable Long groupId, @Nullable Long senderId, @Nullable String msgId) {
        return senderPrefix(bot, groupId, senderId, msgId, null);
    }

    @Nonnull
    public static String senderPrefix(@Nonnull BaniraBot bot, @Nullable Long groupId, @Nullable Long senderId, @Nullable String msgId, @Nullable UserInfoCache cache) {
        if (!BaniraUtils.isUserIdValid(senderId)) {
            return "";
        }
        String messageIdText = StringUtils.isNotNullOrEmpty(msgId) ? "，消息ID=" + msgId : "";
        return "发送者：" + describeUser(bot, groupId, senderId, cache) + messageIdText + "\n";
    }

    @Nonnull
    public static String atText(@Nonnull BaniraBot bot, @Nullable Long groupId, long qq) {
        return atText(bot, groupId, qq, null);
    }

    @Nonnull
    public static String atText(@Nonnull BaniraBot bot, @Nullable Long groupId, long qq, @Nullable UserInfoCache cache) {
        if (qq == 0 || qq == 233) {
            return "[被@用户: qq=0, 类型=全体成员] ";
        }
        UserNames names = resolveNames(bot, groupId, qq, cache);
        return "[被@用户: qq=" + qq
                + ", 群昵称=\"" + safeInlineName(names.groupCard()) + "\""
                + ", QQ昵称=\"" + safeInlineName(names.qqNickname()) + "\"] ";
    }

    @Nonnull
    static List<String> atSearchNames(@Nonnull BaniraBot bot, @Nullable Long groupId, @Nullable List<ArrayMsg> arrayMsg, long selfId) {
        if (CollectionUtils.isNullOrEmpty(arrayMsg)) {
            return List.of();
        }
        List<String> result = new ArrayList<>();
        for (ArrayMsg msg : arrayMsg) {
            if (msg == null || msg.getType() == null || msg.getType() != com.mikuac.shiro.enums.MsgTypeEnum.at) {
                continue;
            }
            long qq = msg.getLongData("qq");
            if (qq <= 0 || qq == 233 || qq == selfId) {
                continue;
            }
            UserNames names = resolveNames(bot, groupId, qq, null);
            if (StringUtils.isNotNullOrEmpty(names.groupCard())) {
                result.add(names.groupCard());
            }
            if (StringUtils.isNotNullOrEmpty(names.qqNickname()) && !result.contains(names.qqNickname())) {
                result.add(names.qqNickname());
            }
            if (result.isEmpty()) {
                result.add(String.valueOf(qq));
            }
        }
        return result;
    }

    @Nonnull
    public static String describeUser(@Nonnull BaniraBot bot, @Nullable Long groupId, @Nullable Long userId) {
        return describeUser(bot, groupId, userId, null);
    }

    @Nonnull
    public static String describeUser(@Nonnull BaniraBot bot, @Nullable Long groupId, @Nullable Long userId, @Nullable UserInfoCache cache) {
        if (!BaniraUtils.isUserIdValid(userId)) {
            return "未知用户";
        }
        UserNames names = resolveNames(bot, groupId, userId, cache);
        String display = firstNonBlank(names.groupCard(), names.qqNickname(), String.valueOf(userId));
        return "稳定身份 qq=" + userId
                + "；显示名=" + display
                + "（群昵称=" + valueOrUnknown(names.groupCard())
                + "，QQ昵称=" + valueOrUnknown(names.qqNickname())
                + "；显示名可能被修改或冒用）";
    }

    @Nonnull
    private static UserNames resolveNames(@Nonnull BaniraBot bot, @Nullable Long groupId, @Nonnull Long userId) {
        return resolveNames(bot, groupId, userId, null);
    }

    @Nonnull
    private static UserNames resolveNames(@Nonnull BaniraBot bot, @Nullable Long groupId, @Nonnull Long userId, @Nullable UserInfoCache cache) {
        if (cache != null) {
            return cache.resolve(bot, groupId, userId);
        }
        return fetchNames(bot, groupId, userId);
    }

    @Nonnull
    private static UserNames fetchNames(@Nonnull BaniraBot bot, @Nullable Long groupId, @Nonnull Long userId) {
        String groupCard = "";
        String qqNickname = "";
        if (BaniraUtils.isGroupIdValid(groupId)) {
            try {
                ActionData<GroupMemberInfoResp> info = bot.getGroupMemberInfo(groupId, userId, false);
                if (bot.isActionDataNotEmpty(info)) {
                    groupCard = StringUtils.nullToEmpty(info.getData().getCard());
                    qqNickname = StringUtils.nullToEmpty(info.getData().getNickname());
                }
            } catch (Exception ignored) {
            }
        }
        if (StringUtils.isNullOrEmpty(qqNickname)) {
            try {
                ActionData<StrangerInfoResp> info = bot.getStrangerInfo(userId, false);
                if (bot.isActionDataNotEmpty(info)) {
                    qqNickname = StringUtils.nullToEmpty(info.getData().getNickname());
                }
            } catch (Exception ignored) {
            }
        }
        if (StringUtils.isNullOrEmpty(groupCard) && StringUtils.isNotNullOrEmpty(qqNickname)) {
            groupCard = qqNickname;
        }
        return new UserNames(groupCard, qqNickname);
    }

    @Nonnull
    private static String valueOrUnknown(@Nullable String value) {
        return StringUtils.isNotNullOrEmpty(value) ? value : "未知";
    }

    @Nonnull
    private static String safeInlineName(@Nullable String value) {
        if (StringUtils.isNullOrEmpty(value)) {
            return "未知";
        }
        String cleaned = ChatInputSanitizer.sanitizeInlineName(value)
                .replaceAll("\\s+", " ")
                .trim();
        if (cleaned.length() > 24) {
            return cleaned.substring(0, 24) + "...";
        }
        return StringUtils.isNotNullOrEmpty(cleaned) ? cleaned : "未知";
    }

    @Nonnull
    private static String firstNonBlank(String... values) {
        for (String value : values) {
            if (StringUtils.isNotNullOrEmpty(value)) {
                return value;
            }
        }
        return "";
    }

    private record UserNames(String groupCard, String qqNickname) {
    }

    public static final class UserInfoCache {
        private final Map<String, UserNames> cache = new HashMap<>();

        @Nonnull
        UserNames resolve(@Nonnull BaniraBot bot, @Nullable Long groupId, @Nonnull Long userId) {
            String key = StringUtils.nullToEmpty(groupId != null ? String.valueOf(groupId) : "") + ":" + userId;
            return cache.computeIfAbsent(key, ignored -> fetchNames(bot, groupId, userId));
        }
    }
}
