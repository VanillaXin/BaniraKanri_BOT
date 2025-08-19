package xin.vanilla.banira.util;

import com.fasterxml.jackson.core.type.TypeReference;
import com.mikuac.shiro.common.utils.JsonUtils;
import com.mikuac.shiro.common.utils.MessageConverser;
import com.mikuac.shiro.common.utils.ShiroUtils;
import com.mikuac.shiro.dto.action.common.ActionData;
import com.mikuac.shiro.dto.action.response.GroupMemberInfoResp;
import com.mikuac.shiro.dto.action.response.MsgResp;
import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import com.mikuac.shiro.dto.event.message.GuildMessageEvent;
import com.mikuac.shiro.dto.event.message.MessageEvent;
import com.mikuac.shiro.dto.event.message.PrivateMessageEvent;
import com.mikuac.shiro.enums.MsgTypeEnum;
import com.mikuac.shiro.model.ArrayMsg;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.ResolvableType;
import xin.vanilla.banira.config.YamlConfigManager;
import xin.vanilla.banira.config.entity.GlobalConfig;
import xin.vanilla.banira.config.entity.GroupConfig;
import xin.vanilla.banira.config.entity.basic.BaseConfig;
import xin.vanilla.banira.config.entity.basic.OtherConfig;
import xin.vanilla.banira.config.entity.basic.PermissionConfig;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.enums.EnumPermission;
import xin.vanilla.banira.mapper.param.MessageRecordQueryParam;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.service.IMessageRecordManager;
import xin.vanilla.banira.start.SpringContextHolder;

import java.util.*;
import java.util.function.Supplier;
import java.util.regex.Pattern;

@Slf4j
public class BaniraUtils {

    // region private

    // endregion private

    // region 配置管理

    private static GlobalConfig getGlobalConfig() {
        Supplier<GlobalConfig> globalConfig = SpringContextHolder.getBean(
                ResolvableType.forClassWithGenerics(Supplier.class, GlobalConfig.class)
        );
        return globalConfig.get();
    }

    private static GroupConfig getGroupConfig() {
        Supplier<GroupConfig> groupConfig = SpringContextHolder.getBean(
                ResolvableType.forClassWithGenerics(Supplier.class, GroupConfig.class)
        );
        return groupConfig.get();
    }

    private static YamlConfigManager<GlobalConfig> getGlobalConfigManager() {
        return SpringContextHolder.getBean(
                ResolvableType.forClassWithGenerics(YamlConfigManager.class, GlobalConfig.class)
        );
    }

    private static YamlConfigManager<GroupConfig> getGroupConfigManager() {
        return SpringContextHolder.getBean(
                ResolvableType.forClassWithGenerics(YamlConfigManager.class, GroupConfig.class)
        );
    }

    public static boolean saveGlobalConfig() {
        try {
            getGlobalConfigManager().save();
        } catch (Exception e) {
            LOGGER.error("Failed to save global config", e);
            return false;
        }
        return true;
    }

    public static boolean saveGroupConfig() {
        try {
            getGroupConfigManager().save();
        } catch (Exception e) {
            LOGGER.error("Failed to save group config", e);
            return false;
        }
        return true;
    }

    public static boolean saveConfig() {
        return saveGlobalConfig() && saveGroupConfig();
    }

    public static String getBotNick() {
        String nick = getGlobalConfig().botNick();
        return StringUtils.isNotNullOrEmpty(nick) ? nick : "香草酱";
    }

    public static BaseConfig getBaseConfig() {
        return getBaseConfig(null);
    }

    public static BaseConfig getBaseConfig(Long groupId) {
        BaseConfig baseConfig = null;
        if (isGroupIdValid(groupId)) {
            // 群聊配置
            GroupConfig groupConfig = getGroupConfig();
            if (groupConfig.baseConfig().containsKey(groupId)) {
                if (groupConfig.baseConfig().get(groupId) != null) {
                    baseConfig = groupConfig.baseConfig().get(groupId);
                } else {
                    groupConfig.baseConfig().put(groupId, BaseConfig.empty());
                    saveGroupConfig();
                }
            }
        }
        // 全局配置
        if (baseConfig == null) {
            baseConfig = getGlobalConfig().baseConfig();
        }
        return baseConfig;
    }

    public static OtherConfig getOthersConfig() {
        return getOthersConfig(null);
    }

    public static OtherConfig getOthersConfig(Long groupId) {
        OtherConfig otherConfig = null;
        if (isGroupIdValid(groupId)) {
            // 群聊配置
            GroupConfig groupConfig = getGroupConfig();
            if (groupConfig.otherConfig().containsKey(groupId)) {
                if (groupConfig.otherConfig().get(groupId) != null) {
                    otherConfig = groupConfig.otherConfig().get(groupId);
                } else {
                    groupConfig.otherConfig().put(groupId, OtherConfig.empty());
                    saveGroupConfig();
                }
            }
        }
        // 全局配置
        if (otherConfig == null) {
            otherConfig = getGlobalConfig().otherConfig();
        }
        return otherConfig;
    }

    // endregion 配置管理

    // region mutableSetOf

    public static <E> Set<E> mutableSetOf() {
        return new HashSet<>();
    }

    public static <E> Set<E> mutableSetOf(E e1) {
        return new HashSet<>(Collections.singletonList(e1));
    }

    public static <E> Set<E> mutableSetOf(E e1, E e2) {
        return new HashSet<>(Arrays.asList(e1, e2));
    }

    public static <E> Set<E> mutableSetOf(E e1, E e2, E e3) {
        return new HashSet<>(Arrays.asList(e1, e2, e3));
    }

    public static <E> Set<E> mutableSetOf(E e1, E e2, E e3, E e4) {
        return new HashSet<>(Arrays.asList(e1, e2, e3, e4));
    }

    public static <E> Set<E> mutableSetOf(E e1, E e2, E e3, E e4, E e5) {
        return new HashSet<>(Arrays.asList(e1, e2, e3, e4, e5));
    }

    public static <E> Set<E> mutableSetOf(E e1, E e2, E e3, E e4, E e5, E e6) {
        return new HashSet<>(Arrays.asList(e1, e2, e3, e4, e5, e6));
    }

    public static <E> Set<E> mutableSetOf(E e1, E e2, E e3, E e4, E e5, E e6, E e7) {
        return new HashSet<>(Arrays.asList(e1, e2, e3, e4, e5, e6, e7));
    }

    public static <E> Set<E> mutableSetOf(E e1, E e2, E e3, E e4, E e5, E e6, E e7, E e8) {
        return new HashSet<>(Arrays.asList(e1, e2, e3, e4, e5, e6, e7, e8));
    }

    public static <E> Set<E> mutableSetOf(E e1, E e2, E e3, E e4, E e5, E e6, E e7, E e8, E e9) {
        return new HashSet<>(Arrays.asList(e1, e2, e3, e4, e5, e6, e7, e8, e9));
    }

    public static <E> Set<E> mutableSetOf(E e1, E e2, E e3, E e4, E e5, E e6, E e7, E e8, E e9, E e10) {
        return new HashSet<>(Arrays.asList(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10));
    }

    @SafeVarargs
    public static <E> Set<E> mutableSetOf(E... elements) {
        return new HashSet<>(Arrays.asList(elements));
    }

    // endregion mutableSetOf

    // region mutableMapOf

    public static <K, V> Map<K, V> mutableMapOf() {
        return new HashMap<>();
    }

    public static <K, V> Map<K, V> mutableMapOf(K k1, V v1) {
        return new HashMap<>(Collections.singletonMap(k1, v1));
    }

    public static <K, V> Map<K, V> mutableMapOf(K k1, V v1, K k2, V v2) {
        return new HashMap<>(Map.of(k1, v1, k2, v2));
    }

    public static <K, V> Map<K, V> mutableMapOf(K k1, V v1, K k2, V v2, K k3, V v3) {
        return new HashMap<>(Map.of(k1, v1, k2, v2, k3, v3));
    }

    public static <K, V> Map<K, V> mutableMapOf(K k1, V v1, K k2, V v2, K k3, V v3, K k4, V v4) {
        return new HashMap<>(Map.of(k1, v1, k2, v2, k3, v3, k4, v4));
    }

    public static <K, V> Map<K, V> mutableMapOf(K k1, V v1, K k2, V v2, K k3, V v3, K k4, V v4, K k5, V v5) {
        return new HashMap<>(Map.of(k1, v1, k2, v2, k3, v3, k4, v4, k5, v5));
    }

    public static <K, V> Map<K, V> mutableMapOf(K k1, V v1, K k2, V v2, K k3, V v3, K k4, V v4, K k5, V v5, K k6, V v6) {
        return new HashMap<>(Map.of(k1, v1, k2, v2, k3, v3, k4, v4, k5, v5, k6, v6));
    }

    public static <K, V> Map<K, V> mutableMapOf(K k1, V v1, K k2, V v2, K k3, V v3, K k4, V v4, K k5, V v5, K k6, V v6, K k7, V v7) {
        return new HashMap<>(Map.of(k1, v1, k2, v2, k3, v3, k4, v4, k5, v5, k6, v6, k7, v7));
    }

    public static <K, V> Map<K, V> mutableMapOf(K k1, V v1, K k2, V v2, K k3, V v3, K k4, V v4, K k5, V v5, K k6, V v6, K k7, V v7, K k8, V v8) {
        return new HashMap<>(Map.of(k1, v1, k2, v2, k3, v3, k4, v4, k5, v5, k6, v6, k7, v7, k8, v8));
    }

    public static <K, V> Map<K, V> mutableMapOf(K k1, V v1, K k2, V v2, K k3, V v3, K k4, V v4, K k5, V v5, K k6, V v6, K k7, V v7, K k8, V v8, K k9, V v9) {
        return new HashMap<>(Map.of(k1, v1, k2, v2, k3, v3, k4, v4, k5, v5, k6, v6, k7, v7, k8, v8, k9, v9));
    }

    public static <K, V> Map<K, V> mutableMapOf(K k1, V v1, K k2, V v2, K k3, V v3, K k4, V v4, K k5, V v5, K k6, V v6, K k7, V v7, K k8, V v8, K k9, V v9, K k10, V v10) {
        return new HashMap<>(Map.of(k1, v1, k2, v2, k3, v3, k4, v4, k5, v5, k6, v6, k7, v7, k8, v8, k9, v9, k10, v10));
    }

    @SafeVarargs
    public static <K, V> Map<K, V> mutableMapOf(Map.Entry<K, V>... entries) {
        return new HashMap<>(Map.ofEntries(entries));
    }

    // endregion mutableMapOf

    // region 消息处理

    public static final Pattern QQ_PATTERN = Pattern.compile(",\\s*qq=(?<qq>\\d+)\\s*[,\\]]");
    public static final Pattern ID_PATTERN = Pattern.compile(",\\s*id=(?<id>\\d+)\\s*[,\\]]");

    // region 回复

    public static boolean hasReply(List<ArrayMsg> arrayMsg) {
        return arrayMsg.stream().anyMatch(e -> e.getType() == MsgTypeEnum.reply);
    }

    public static Long getReplyId(List<ArrayMsg> arrayMsg) {
        return arrayMsg.stream()
                .filter(e -> e.getType() == MsgTypeEnum.reply)
                .findFirst()
                .map(e -> e.getLongData("id"))
                .orElse(null);
    }

    public static List<ArrayMsg> getReplayContentById(BaniraBot bot, Long replayId) {
        List<ArrayMsg> result = null;
        if (replayId != null) {
            ActionData<MsgResp> msgData = bot.getMsg(replayId.intValue());
            if (bot.isActionDataNotEmpty(msgData)) {
                result = msgData.getData().getArrayMsg();
            }
        }
        return result;
    }

    public static List<ArrayMsg> getReplayContent(BaniraBot bot, List<ArrayMsg> arrayMsg) {
        return getReplayContentById(bot, getReplyId(arrayMsg));
    }

    public static String getReplyContentString(BaniraBot bot, List<ArrayMsg> arrayMsg) {
        return MessageConverser.arraysToString(getReplayContent(bot, arrayMsg));
    }

    public static long getReplyQQ(BaniraBot bot, Long groupId, List<ArrayMsg> arrayMsg) {
        long qq = arrayMsg.stream()
                .filter(e -> e.getType() == MsgTypeEnum.reply)
                .findFirst()
                .map(e -> e.getLongData("qq"))
                .orElse(0L);
        if (qq == 0) {
            IMessageRecordManager messageRecordManager = SpringContextHolder.getBean(IMessageRecordManager.class);
            List<MessageRecord> records = messageRecordManager.getMessageRecordList(new MessageRecordQueryParam()
                    .setMsgId(String.valueOf(getReplyId(arrayMsg)))
                    .setBotId(bot.getSelfId())
                    .setTargetId(groupId)
            );
            qq = records.stream()
                    .findFirst()
                    .map(MessageRecord::getSenderId)
                    .orElse(0L);
        }
        return qq;
    }

    public static boolean hasReply(String msg) {
        return StringUtils.isNotNullOrEmpty(msg) && msg.contains("[CQ:reply,");
    }

    public static Long getReplyId(String msg) {
        return hasReply(msg) ?
                StringUtils.toLong(ID_PATTERN.matcher(msg).results()
                        .map(m -> m.group("id"))
                        .findFirst().orElse(null)) :
                null;
    }

    public static List<ArrayMsg> getReplyContent(BaniraBot bot, String msg) {
        Long replayId = getReplyId(msg);
        return getReplayContentById(bot, replayId);
    }

    public static String getReplyContentString(BaniraBot bot, String msg) {
        Long replayId = getReplyId(msg);
        return MessageConverser.arraysToString(getReplayContentById(bot, replayId));
    }

    public static long getReplyQQ(BaniraBot bot, Long groupId, String msg) {
        long qq = 0;
        if (hasReply(msg)) {
            qq = QQ_PATTERN.matcher(msg).results()
                    .map(m -> m.group("qq"))
                    .map(Long::parseLong)
                    .findFirst().orElse(0L);
            if (qq == 0) {
                IMessageRecordManager messageRecordManager = SpringContextHolder.getBean(IMessageRecordManager.class);
                List<MessageRecord> records = messageRecordManager.getMessageRecordList(new MessageRecordQueryParam()
                        .setMsgId(String.valueOf(getReplyId(msg)))
                        .setBotId(bot.getSelfId())
                        .setTargetId(groupId)
                );
                return records.stream()
                        .findFirst()
                        .map(MessageRecord::getSenderId)
                        .orElse(0L);
            }
        }
        return qq;
    }

    public static String replaceReply(String msg) {
        String result = msg;
        if (hasReply(msg)) {
            result = MessageConverser.arraysToString(
                    MessageConverser.stringToArray(msg).stream()
                            .filter(o -> o.getType() != MsgTypeEnum.reply)
                            .toList()
            );
        }
        return result;
    }

    // endregion 回复

    // region 艾特

    public static boolean hasAt(List<ArrayMsg> arrayMsg) {
        return arrayMsg.stream().anyMatch(e -> e.getType() == MsgTypeEnum.at);
    }

    public static long getAtQQ(List<ArrayMsg> arrayMsg) {
        return arrayMsg.stream()
                .filter(e -> e.getType() == MsgTypeEnum.at)
                .findFirst()
                .map(e -> e.getLongData("qq"))
                .orElse(0L);
    }

    public static boolean hasAt(String msg) {
        return StringUtils.isNotNullOrEmpty(msg) && msg.contains("[CQ:at,");
    }

    public static long getAtQQ(String msg) {
        return hasAt(msg) ?
                QQ_PATTERN.matcher(msg).results()
                        .map(m -> m.group("qq"))
                        .map(Long::parseLong)
                        .map(qq -> qq == 0 ? 233L : qq)
                        .findFirst().orElse(0L) :
                0L;
    }

    public static boolean hasAtAll(List<ArrayMsg> arrayMsg) {
        return ShiroUtils.isAtAll(arrayMsg) || hasAtAll(MessageConverser.arraysToString(arrayMsg));
    }

    public static boolean hasAtAll(String msg) {
        Supplier<GlobalConfig> globalConfig = SpringContextHolder.getBean(
                ResolvableType.forClassWithGenerics(Supplier.class, GlobalConfig.class)
        );
        return hasAt(msg)
                && (ShiroUtils.isAtAll(msg)
                || msg.contains("[CQ:at,qq=0]")
                || msg.contains("[CQ:at,qq=233]"))
                || globalConfig.get().instConfig().base().atAll().stream().anyMatch(msg::contains);
    }

    // endregion 艾特

    // region 消息ID

    public static int getMsgId(MessageEvent event) {
        switch (event) {
            case GroupMessageEvent groupMessageEvent -> {
                return groupMessageEvent.getMessageId();
            }
            case PrivateMessageEvent privateMessageEvent -> {
                return privateMessageEvent.getMessageId();
            }
            default -> {
                return 0;
            }
        }
    }

    public static String getGuildMsgId(MessageEvent event) {
        if (event instanceof GuildMessageEvent guildMessageEvent) {
            return guildMessageEvent.getMessageId();
        } else {
            return "";
        }
    }

    // endregion 消息ID

    // region 合并转发

    public static boolean hasForward(List<ArrayMsg> arrayMsg) {
        return arrayMsg.stream().anyMatch(e -> e.getType() == MsgTypeEnum.forward);
    }

    public static Long getForwardId(List<ArrayMsg> arrayMsg) {
        return arrayMsg.stream()
                .filter(e -> e.getType() == MsgTypeEnum.forward)
                .findFirst()
                .map(e -> e.getLongData("id"))
                .orElse(null);
    }

    public static List<MsgResp> getForwardContentFirst(ArrayMsg... arrayMsg) {
        List<MsgResp> result = new ArrayList<>();
        if (CollectionUtils.isNotNullOrEmpty(arrayMsg)) {
            if (arrayMsg[0].getType() == MsgTypeEnum.forward) {
                String content = arrayMsg[0].getStringData("content");
                if (JsonUtils.isValid(content)) {
                    result = JsonUtils.readValue(content, new TypeReference<>() {
                    });
                }
            }
        }
        return result;
    }

    public static List<List<MsgResp>> getForwardContent(Collection<ArrayMsg> arrayMsg) {
        return getForwardContent(arrayMsg.toArray(new ArrayMsg[]{}));
    }

    public static List<List<MsgResp>> getForwardContent(ArrayMsg... arrayMsg) {
        List<List<MsgResp>> result = new ArrayList<>();
        if (CollectionUtils.isNotNullOrEmpty(arrayMsg)) {
            for (ArrayMsg msg : arrayMsg) {
                result.add(getForwardContentFirst(msg));
            }
        }
        return result;
    }

    public static boolean hsaForward(String msg) {
        return StringUtils.isNotNullOrEmpty(msg) && msg.contains("[CQ:forward,");
    }

    public static Long getForwardId(String msg) {
        return hsaForward(msg) ?
                ID_PATTERN.matcher(msg).results()
                        .map(m -> m.group("id"))
                        .map(Long::parseLong)
                        .findFirst().orElse(null) :
                null;
    }

    public static List<MsgResp> getForwardContentFirst(String msg) {
        List<MsgResp> result = new ArrayList<>();
        if (hsaForward(msg)) {
            List<ArrayMsg> arrayMsg = decodeForwardMsg(msg);
            if (CollectionUtils.isNotNullOrEmpty(arrayMsg)) {
                result = getForwardContentFirst(arrayMsg.toArray(new ArrayMsg[]{}));
            }
        }
        return result;
    }

    public static List<List<MsgResp>> getForwardContent(String msg) {
        List<List<MsgResp>> result = new ArrayList<>();
        if (hsaForward(msg)) {
            List<ArrayMsg> arrayMsg = decodeForwardMsg(msg);
            if (CollectionUtils.isNotNullOrEmpty(arrayMsg)) {
                result = getForwardContent(arrayMsg);
            }
        }
        return result;
    }

    public static ArrayMsg packForwardMsg(Long forwardId, List<MsgResp> forwardMsg) {
        Map<String, Object> forwardData = BaniraUtils.mutableMapOf("id", forwardId, "content", forwardMsg);
        return new ArrayMsg().setType(MsgTypeEnum.forward).setData(forwardData);
    }

    public static String encodeForwardMsg(Long forwardId, List<MsgResp> forwardMsg) {
        return packForwardMsg(forwardId, forwardMsg).toCQCode();
    }

    public static List<ArrayMsg> decodeForwardMsg(String forwardMsg) {
        return MessageConverser.stringToArray(forwardMsg);
    }

    // endregion 合并转发

    // region 其他

    public static boolean isGroupIdValid(Long groupId) {
        return groupId != null && groupId > 10000;
    }

    public static boolean isFriendIdValid(Long friendId) {
        return friendId != null && friendId > 10000;
    }

    // endregion 其他

    // endregion 消息处理

    // region 权限判断

    /**
     * 判断是否主人
     */
    @SuppressWarnings("ConstantConditions")
    public static boolean isOwner(@Nonnull Long qq) {
        Long owner = getGlobalConfig().owner();
        return owner != null && owner > 0 && Objects.equals(owner, qq);
    }

    /**
     * 判断是否管家
     */
    public static boolean isButler(@Nonnull Long qq) {
        Set<PermissionConfig> butler = getGlobalConfig().butler();
        return CollectionUtils.isNotNullOrEmpty(butler) && butler.stream().anyMatch(p -> qq.equals(p.id()));
    }

    /**
     * 判断是否仆人
     */
    public static boolean isServant(@Nullable Long groupId, @Nonnull Long qq) {
        if (groupId == null || groupId <= 0L) return false;
        Set<PermissionConfig> servant = getGroupConfig().maid().get(groupId);
        return CollectionUtils.isNotNullOrEmpty(servant) && servant.stream().anyMatch(e -> qq.equals(e.id()));
    }

    /**
     * 判断是否仆人
     */
    public static boolean isMaid(@Nullable Long groupId, @Nonnull Long qq) {
        return isServant(groupId, qq);
    }

    /**
     * 判断是否群主
     */
    public static boolean isGroupOwner(@Nullable BaniraBot bot, @Nullable Long groupId, @Nonnull Long qq) {
        if (bot == null || groupId == null || groupId <= 0L) return false;
        ActionData<GroupMemberInfoResp> groupMemberInfo = bot.getGroupMemberInfo(groupId, qq, false);
        return groupMemberInfo != null && "owner".equalsIgnoreCase(groupMemberInfo.getData().getRole());
    }

    /**
     * 判断是否群管理
     */
    public static boolean isGroupAdmin(@Nullable BaniraBot bot, @Nullable Long groupId, @Nonnull Long qq) {
        if (bot == null || groupId == null || groupId <= 0L) return false;
        ActionData<GroupMemberInfoResp> groupMemberInfo = bot.getGroupMemberInfo(groupId, qq, false);
        return groupMemberInfo != null && "admin".equalsIgnoreCase(groupMemberInfo.getData().getRole());
    }

    /**
     * 判断a是否b的上属
     */
    public static boolean isUpper(@Nullable BaniraBot bot, @Nullable Long groupId, @Nonnull Long a, @Nonnull Long b) {
        if (isOwner(a))
            return !isGroupOwner(bot, groupId, b);
        if (isButler(a))
            return !isGroupOwner(bot, groupId, b)
                    && !isOwner(b)
                    && !isButler(b);
        if (isGroupOwner(bot, groupId, a))
            return !isOwner(b);
        if (isGroupAdmin(bot, groupId, a))
            return !isGroupOwner(bot, groupId, b)
                    && !isOwner(b)
                    && !isButler(b)
                    && !isGroupAdmin(bot, groupId, b);
        if (isServant(groupId, a))
            return !isGroupOwner(bot, groupId, b)
                    && !isOwner(b)
                    && !isButler(b)
                    && !isGroupAdmin(bot, groupId, b)
                    && !isServant(groupId, b);
        return false;
    }

    /**
     * 判断a是否b的上属
     */
    public static boolean isUpperInGroup(@Nullable BaniraBot bot, @Nullable Long groupId, @Nonnull Long a, @Nonnull Long b) {
        if (isGroupOwner(bot, groupId, a))
            return true;
        if (isGroupAdmin(bot, groupId, a))
            return !isGroupOwner(bot, groupId, b)
                    && !isGroupAdmin(bot, groupId, b);
        return false;
    }

    /**
     * 获取所有拥有的权限
     */
    @Nonnull
    public static Set<EnumPermission> getPermission(@Nullable BaniraBot bot, @Nullable Long groupId, @Nonnull Long qq) {
        if (isOwner(qq)) return EnumPermission.getAll();
        Set<EnumPermission> permissions = mutableSetOf();
        if (isGroupOwner(bot, groupId, qq))
            permissions.addAll(EnumPermission.getGroupOwner());
        if (isGroupAdmin(bot, groupId, qq))
            permissions.addAll(EnumPermission.getGroupAdmin());
        Set<PermissionConfig> butler = getGlobalConfig().butler();
        if (CollectionUtils.isNotNullOrEmpty(butler)) {
            butler.stream()
                    .filter(p -> qq.equals(p.id()))
                    .findFirst()
                    .ifPresent(p -> permissions.addAll(p.permissions()));
        }
        if (groupId != null && groupId > 0L) {
            Set<PermissionConfig> servant = getGroupConfig().maid().getOrDefault(groupId, new HashSet<>());
            servant.stream()
                    .filter(p -> qq.equals(p.id()))
                    .findFirst()
                    .ifPresent(p -> permissions.addAll(p.permissions()));
        }
        return permissions;
    }

    /**
     * 判断是否拥有某个权限
     */
    public static boolean hasPermission(@Nullable BaniraBot bot, @Nullable Long groupId, @Nonnull Long qq, @Nonnull EnumPermission permission) {
        return getPermission(bot, groupId, qq).contains(permission);
    }

    /**
     * 判断是否拥有全部权限
     */
    public static boolean hasAllPermissions(@Nullable BaniraBot bot, @Nullable Long groupId, @Nonnull Long qq, @Nonnull EnumPermission... permissions) {
        return hasAllPermissions(bot, groupId, qq, Arrays.asList(permissions));
    }

    /**
     * 判断是否拥有全部权限
     */
    public static boolean hasAllPermissions(@Nullable BaniraBot bot, @Nullable Long groupId, @Nonnull Long qq, @Nonnull Collection<EnumPermission> permissions) {
        return getPermission(bot, groupId, qq).containsAll(permissions);
    }

    /**
     * 判断是否拥有任意一个权限
     */
    public static boolean hasAnyPermissions(@Nullable BaniraBot bot, @Nullable Long groupId, @Nonnull Long qq, @Nonnull EnumPermission... permission) {
        return hasAnyPermissions(bot, groupId, qq, Arrays.asList(permission));
    }

    /**
     * 判断是否拥有任意一个权限
     */
    public static boolean hasAnyPermissions(@Nullable BaniraBot bot, @Nullable Long groupId, @Nonnull Long qq, @Nonnull Collection<EnumPermission> permissions) {
        return getPermission(bot, groupId, qq).stream().anyMatch(permissions::contains);
    }

    public static Set<String> getPermissionNames(EnumPermission permission) {
        GlobalConfig globalConfig = getGlobalConfig();
        switch (permission) {
            case APER, RPER -> {
                return globalConfig.instConfig().kanri().op();
            }
            case MUTE, MALL -> {
                return globalConfig.instConfig().kanri().mute();
            }
            case LOUD, LALL -> {
                return globalConfig.instConfig().kanri().loud();
            }
            case ATAL -> {
                return globalConfig.instConfig().base().atAll();
            }
            case CARD -> {
                return globalConfig.instConfig().kanri().card();
            }
            case TAG -> {
                return globalConfig.instConfig().kanri().tag();
            }
            case KICK -> {
                return globalConfig.instConfig().kanri().kick();
            }
            case RECA -> {
                return globalConfig.instConfig().kanri().withdraw();
            }
            case GARE -> {
                return globalConfig.instConfig().kanri().approve();
            }
            case AESS, RESS -> {
                return globalConfig.instConfig().kanri().essence();
            }
            case GNAM -> {
                return globalConfig.instConfig().kanri().groupName();
            }
            case AADM, RADM -> {
                return globalConfig.instConfig().kanri().admin();
            }
            case AMAI, RMAI -> {
                return globalConfig.instConfig().kanri().maid();
            }
            case ABUT, RBUT -> {
                return globalConfig.instConfig().kanri().butler();
            }
            default -> {
                return Set.of();
            }
        }
    }

    // endregion 权限判断

}
