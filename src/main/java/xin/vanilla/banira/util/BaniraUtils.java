package xin.vanilla.banira.util;

import cn.hutool.core.io.FileUtil;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.JsonNodeType;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;
import com.github.houbb.sensitive.word.core.SensitiveWordHelper;
import com.google.gson.JsonObject;
import com.mikuac.shiro.common.utils.JsonUtils;
import com.mikuac.shiro.common.utils.MessageConverser;
import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.common.utils.ShiroUtils;
import com.mikuac.shiro.core.BotContainer;
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
import xin.vanilla.banira.coder.message.FileCode;
import xin.vanilla.banira.coder.message.ImageCode;
import xin.vanilla.banira.coder.message.VideoCode;
import xin.vanilla.banira.config.YamlConfigManager;
import xin.vanilla.banira.config.entity.GlobalConfig;
import xin.vanilla.banira.config.entity.GroupConfig;
import xin.vanilla.banira.config.entity.InstructionsConfig;
import xin.vanilla.banira.config.entity.basic.*;
import xin.vanilla.banira.domain.KeyValue;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.enums.EnumCacheFileType;
import xin.vanilla.banira.enums.EnumPermission;
import xin.vanilla.banira.mapper.param.MessageRecordQueryParam;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.service.IMessageRecordManager;
import xin.vanilla.banira.start.SpringContextHolder;

import java.io.File;
import java.net.InetAddress;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.*;
import java.util.function.Supplier;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

@Slf4j
public final class BaniraUtils {

    // region private

    private BaniraUtils() {
    }

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

    private static InstructionsConfig getInsConfig() {
        Supplier<InstructionsConfig> insConfig = SpringContextHolder.getBean(
                ResolvableType.forClassWithGenerics(Supplier.class, InstructionsConfig.class)
        );
        return insConfig.get();
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

    private static YamlConfigManager<InstructionsConfig> getInsConfigManager() {
        return SpringContextHolder.getBean(
                ResolvableType.forClassWithGenerics(YamlConfigManager.class, InstructionsConfig.class)
        );
    }

    private static IMessageRecordManager getMessageRecordManager() {
        return SpringContextHolder.getBean(IMessageRecordManager.class);
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

    public static OtherConfig getOthersConfig() {
        return getOthersConfig(null);
    }

    public static OtherConfig getOthersConfig(Long groupId) {
        OtherConfig otherConfig = null;
        Map<Long, OtherConfig> otherConfigMap = getGroupConfig().otherConfig();
        if (isGroupIdValid(groupId) && otherConfigMap.containsKey(groupId)) {
            // 群聊配置
            if (otherConfigMap.get(groupId) != null) {
                otherConfig = otherConfigMap.get(groupId);
            } else {
                otherConfigMap.put(groupId, new OtherConfig());
                saveGroupConfig();
            }
        }
        // 全局配置
        else {
            otherConfig = otherConfigMap.computeIfAbsent(0L, k -> new OtherConfig());
        }
        return otherConfig;
    }

    public static BaseInstructionsConfig getBaseIns() {
        BaseInstructionsConfig base = getInsConfig().base();
        return base != null ? base : new BaseInstructionsConfig();
    }

    public static KeyInstructionsConfig getKeyIns() {
        KeyInstructionsConfig key = getInsConfig().key();
        return key != null ? key : new KeyInstructionsConfig();
    }

    public static TimerInstructionsConfig getTimerIns() {
        TimerInstructionsConfig timer = getInsConfig().timer();
        return timer != null ? timer : new TimerInstructionsConfig();
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

    public static List<ArrayMsg> getReplyContentById(BaniraBot bot, Long replyId) {
        List<ArrayMsg> result = null;
        if (replyId != null) {
            ActionData<MsgResp> msgData = bot.getMsg(replyId.intValue());
            if (bot.isActionDataNotEmpty(msgData)) {
                result = msgData.getData().getArrayMsg();
            }
        }
        return result;
    }

    public static List<ArrayMsg> getReplyContent(BaniraBot bot, List<ArrayMsg> arrayMsg) {
        return getReplyContentById(bot, getReplyId(arrayMsg));
    }

    public static String getReplyContentString(BaniraBot bot, List<ArrayMsg> arrayMsg) {
        return MessageConverser.arraysToString(getReplyContent(bot, arrayMsg));
    }

    public static long getReplyUserId(BaniraBot bot, Long groupId, List<ArrayMsg> arrayMsg) {
        long qq = arrayMsg.stream()
                .filter(e -> e.getType() == MsgTypeEnum.reply)
                .findFirst()
                .map(e -> e.getLongData("qq"))
                .orElse(0L);
        Long replyId = getReplyId(arrayMsg);
        if (qq == 0 && replyId != null) {
            IMessageRecordManager messageRecordManager = getMessageRecordManager();
            List<MessageRecord> records = messageRecordManager.getMessageRecordList(new MessageRecordQueryParam()
                    .setMsgId(String.valueOf(replyId))
                    .setBotId(bot.getSelfId())
                    .setTargetId(groupId)
            );
            qq = records.stream()
                    .findFirst()
                    .map(MessageRecord::getSenderId)
                    .orElse(0L);
        }
        if (qq == 0 && replyId != null) {
            ActionData<MsgResp> msgData = bot.getMsg(replyId.intValue());
            if (bot.isActionDataNotEmpty(msgData)) {
                qq = StringUtils.toLong(msgData.getData().getSender().getUserId());
            }
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
        Long replyId = getReplyId(msg);
        return getReplyContentById(bot, replyId);
    }

    public static String getReplyContentString(BaniraBot bot, String msg) {
        Long replyId = getReplyId(msg);
        return MessageConverser.arraysToString(getReplyContentById(bot, replyId));
    }

    public static long getReplyUserId(BaniraBot bot, Long groupId, String msg) {
        long qq = 0;
        if (hasReply(msg)) {
            qq = QQ_PATTERN.matcher(msg).results()
                    .map(m -> m.group("qq"))
                    .map(Long::parseLong)
                    .findFirst().orElse(0L);
            if (qq == 0) {
                IMessageRecordManager messageRecordManager = getMessageRecordManager();
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

    public static long getAtUserId(List<ArrayMsg> arrayMsg) {
        return arrayMsg.stream()
                .filter(e -> e.getType() == MsgTypeEnum.at)
                .findFirst()
                .map(e -> e.getLongData("qq"))
                .orElse(0L);
    }

    public static boolean hasAt(String msg) {
        return StringUtils.isNotNullOrEmpty(msg) && msg.contains("[CQ:at,");
    }

    public static long getAtUserId(String msg) {
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
        return hasAt(msg)
                && (ShiroUtils.isAtAll(msg)
                || msg.contains("[CQ:at,qq=0]")
                || msg.contains("[CQ:at,qq=233]"))
                || getInsConfig().base().atAll().stream().anyMatch(msg::contains);
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

    private static boolean hasForward(JsonNode node) {
        boolean result = false;
        if (node != null && node.has("data")) {
            JsonNode data = node.get("data");
            if (data.getNodeType() == JsonNodeType.STRING) {
                Optional<JsonNode> rootNode = JsonUtils.parseObject(data.asText());
                if (rootNode.isPresent() && rootNode.get().has("app")) {
                    JsonNode appNode = rootNode.get().get("app");
                    result = appNode.getNodeType() == JsonNodeType.STRING
                            && "com.tencent.multimsg".equalsIgnoreCase(appNode.asText());
                }
            }
        }
        return result;
    }

    public static boolean hasForward(List<ArrayMsg> arrayMsg) {
        return arrayMsg.stream()
                .anyMatch(e -> e.getType() == MsgTypeEnum.forward || hasForward(e.getData()));
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

    public static boolean hasForward(String msg) {
        return StringUtils.isNotNullOrEmpty(msg) && msg.contains("[CQ:forward,");
    }

    public static Long getForwardId(String msg) {
        return hasForward(msg) ?
                ID_PATTERN.matcher(msg).results()
                        .map(m -> m.group("id"))
                        .map(Long::parseLong)
                        .findFirst().orElse(null) :
                null;
    }

    public static List<MsgResp> getForwardContentFirst(String msg) {
        List<MsgResp> result = new ArrayList<>();
        if (hasForward(msg)) {
            List<ArrayMsg> arrayMsg = decodeForwardMsg(msg);
            if (CollectionUtils.isNotNullOrEmpty(arrayMsg)) {
                result = getForwardContentFirst(arrayMsg.toArray(new ArrayMsg[]{}));
            }
        }
        return result;
    }

    public static List<List<MsgResp>> getForwardContent(String msg) {
        List<List<MsgResp>> result = new ArrayList<>();
        if (hasForward(msg)) {
            List<ArrayMsg> arrayMsg = decodeForwardMsg(msg);
            if (CollectionUtils.isNotNullOrEmpty(arrayMsg)) {
                result = getForwardContent(arrayMsg);
            }
        }
        return result;
    }

    public static ArrayMsg packForwardMsg(Long forwardId, List<MsgResp> forwardMsg) {
        Map<String, Object> forwardData = BaniraUtils.mutableMapOf("content", forwardMsg);
        if (forwardId != null) forwardData.put("id", forwardId);
        return new ArrayMsg().setType(MsgTypeEnum.forward).setData(forwardData);
    }

    public static String encodeForwardMsg(Long forwardId, List<MsgResp> forwardMsg) {
        return packForwardMsg(forwardId, forwardMsg).toCQCode();
    }

    public static List<ArrayMsg> decodeForwardMsg(String forwardMsg) {
        return MessageConverser.stringToArray(forwardMsg);
    }

    public static List<MsgResp> encodeSendForwardMsg(@Nonnull List<Map<String, Object>> msg) {
        List<MsgResp> result = new ArrayList<>();
        for (Map<String, Object> node : msg) {
            JsonObject jsonObject = xin.vanilla.banira.util.JsonUtils.parseJsonObject(node);
            if (jsonObject != null && "node".equalsIgnoreCase(xin.vanilla.banira.util.JsonUtils.getString(jsonObject, "type", ""))) {
                JsonObject data = xin.vanilla.banira.util.JsonUtils.getJsonObject(jsonObject, "data");
                MsgResp msgResp = new MsgResp();
                msgResp.setPostType("message");
                if (data.has("id")) {
                    msgResp.setMessage(MsgUtils.builder().forward(xin.vanilla.banira.util.JsonUtils.getString(data, "id", "")).build());
                    msgResp.setRawMessage(msgResp.getMessage());
                    msgResp.setArrayMsg(MessageConverser.stringToArray(msgResp.getMessage()));
                } else {
                    MsgResp.Sender sender = new MsgResp.Sender();
                    sender.setUserId(xin.vanilla.banira.util.JsonUtils.getString(data, "uin", null));
                    sender.setNickname(xin.vanilla.banira.util.JsonUtils.getString(data, "name", null));
                    sender.setCard(sender.getNickname());
                    msgResp.setSender(sender);
                    msgResp.setUserId(StringUtils.toLong(sender.getUserId()));

                    msgResp.setMessage(xin.vanilla.banira.util.JsonUtils.getString(data, "content", null));
                    msgResp.setRawMessage(msgResp.getMessage());
                    msgResp.setArrayMsg(MessageConverser.stringToArray(msgResp.getMessage()));
                    msgResp.setTime(StringUtils.toLong(xin.vanilla.banira.util.JsonUtils.getString(data, "time", "0")));
                }
                result.add(msgResp);
            }
        }
        return result;
    }

    /**
     * 是否包含复杂的消息
     */
    public static boolean hasComplexMsg(List<ArrayMsg> msgs) {
        return msgs.stream().anyMatch(e ->
                e.getType() == MsgTypeEnum.mface
                        || e.getType() == MsgTypeEnum.marketface
                        || e.getType() == MsgTypeEnum.basketball
                        || e.getType() == MsgTypeEnum.record
                        || e.getType() == MsgTypeEnum.video
                        || e.getType() == MsgTypeEnum.rps
                        || e.getType() == MsgTypeEnum.new_rps
                        || e.getType() == MsgTypeEnum.dice
                        || e.getType() == MsgTypeEnum.new_dice
                        || e.getType() == MsgTypeEnum.shake
                        || e.getType() == MsgTypeEnum.share
                        || e.getType() == MsgTypeEnum.contact
                        || e.getType() == MsgTypeEnum.location
                        || e.getType() == MsgTypeEnum.music
                        || e.getType() == MsgTypeEnum.redbag
                        || e.getType() == MsgTypeEnum.poke
                        || e.getType() == MsgTypeEnum.gift
                        || e.getType() == MsgTypeEnum.forward
                        || e.getType() == MsgTypeEnum.xml
                        || e.getType() == MsgTypeEnum.json
                        || e.getType() == MsgTypeEnum.cardimage
                        || e.getType() == MsgTypeEnum.tts
                        || e.getType() == MsgTypeEnum.unknown
        );
    }

    /**
     * 是否普通消息
     */
    public static boolean isOrdinaryMsg(List<ArrayMsg> msgs) {
        return !hasComplexMsg(msgs);
    }

    // endregion 合并转发

    // region 敏感内容处理

    public static String replaceSensitiveContent(String msg) {
        if (StringUtils.isNotNullOrEmpty(msg)) {
            List<String> sensitiveList = SensitiveWordHelper.findAll(msg);
            if (CollectionUtils.isNotNullOrEmpty(sensitiveList)) {
                for (String sensitive : sensitiveList) {
                    msg = msg.replace(sensitive, PlantCipher.encode(sensitive));
                }
            }
        }
        return msg;
    }

    public static List<ArrayMsg> replaceSensitiveContent(List<ArrayMsg> arrayMsgList) {
        if (CollectionUtils.isNotNullOrEmpty(arrayMsgList)) {
            arrayMsgList.forEach(arrayMsg -> {
                if (arrayMsg.getType() == MsgTypeEnum.text) {
                    arrayMsg.setData(
                            mutableMapOf(
                                    MsgTypeEnum.text.toString()
                                    , replaceSensitiveContent(arrayMsg.getStringData(MsgTypeEnum.text.toString()))
                            )
                    );
                    JsonNode node = arrayMsg.getData();
                    if (node instanceof ObjectNode objectNode) {
                        JsonNode jsonNode = node.get(MsgTypeEnum.text.toString());
                        if (jsonNode.isNumber()) {
                            objectNode.set(MsgTypeEnum.text.toString(), new TextNode(jsonNode.asText()));
                        }
                    }
                }
            });
        }
        return arrayMsgList;
    }

    public static Object replaceSensitiveContent(Object obj) {
        if (obj instanceof String str) {
            return replaceSensitiveContent(str);
        } else if (obj instanceof Collection<?> list) {
            return list.stream()
                    .map(BaniraUtils::replaceSensitiveContent)
                    .collect(Collectors.toList());
        } else if (obj instanceof Map<?, ?> map) {
            return map.entrySet().stream()
                    .map(e -> Map.entry(e.getKey(), replaceSensitiveContent(e.getValue())))
                    .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
        } else if (obj instanceof ArrayMsg arrayMsg && arrayMsg.getType() == MsgTypeEnum.text) {
            arrayMsg.setData(
                    mutableMapOf(
                            MsgTypeEnum.text.toString()
                            , replaceSensitiveContent(arrayMsg.getStringData(MsgTypeEnum.text.toString()))
                    )
            );
            JsonNode node = arrayMsg.getData();
            if (node instanceof ObjectNode objectNode) {
                JsonNode jsonNode = node.get(MsgTypeEnum.text.toString());
                if (jsonNode.isNumber()) {
                    objectNode.set(MsgTypeEnum.text.toString(), new TextNode(jsonNode.asText()));
                }
            }
            return arrayMsg;
        }
        return obj;
    }

    // end region 敏感内容处理

    // region 其他

    public static boolean isGroupIdValid(Long groupId) {
        return groupId != null && groupId > 10000;
    }

    public static boolean isUserIdValid(Long friendId) {
        return friendId != null && friendId > 10000;
    }


    @Nonnull
    public static Set<Long> getUserIds(String[] args) {
        Set<Long> qqs = BaniraUtils.mutableSetOf();
        for (String arg : args) {
            if (BaniraUtils.hasAt(arg)) {
                qqs.add(BaniraUtils.getAtUserId(arg));
            } else {
                long l = StringUtils.toLong(arg, -1L);
                if (BaniraUtils.isUserIdValid(l)) qqs.add(l);
            }
        }
        return qqs;
    }

    @Nonnull
    public static Set<Long> getUserIdsWithoutReply(List<ArrayMsg> arrayMsgList, @Nonnull String[] args) {
        Set<Long> result = BaniraUtils.mutableSetOf();
        if (CollectionUtils.isNotNullOrEmpty(arrayMsgList)) {
            result.addAll(ShiroUtils.getAtList(arrayMsgList));
        }
        result.addAll(getUserIds(args));
        return result;
    }

    @Nonnull
    public static Set<Long> getUserIdsWithReply(@Nonnull BaniraBot bot, Long groupId, List<ArrayMsg> arrayMsgList, @Nonnull String[] args) {
        Set<Long> result = BaniraUtils.mutableSetOf();
        if (CollectionUtils.isNotNullOrEmpty(arrayMsgList)) {
            if (BaniraUtils.hasReply(arrayMsgList)) {
                result.add(BaniraUtils.getReplyUserId(bot, groupId, arrayMsgList));
            } else if (BaniraUtils.hasAtAll(arrayMsgList)) {
                result.add(233L);
            }
        }
        result.addAll(getUserIdsWithoutReply(arrayMsgList, args));
        return result;
    }

    // endregion 其他

    // endregion 消息处理

    // region 权限判断

    /**
     * 获取主人
     */
    public static Long getOwner() {
        return getGlobalConfig().owner();
    }

    /**
     * 获取管家
     */
    public static List<PermissionConfig> getButler() {
        return getGroupConfig().maid().computeIfAbsent(0L, k -> new ArrayList<>());
    }

    /**
     * 获取女仆
     */
    public static Map<Long, List<PermissionConfig>> getMaid() {
        return getGroupConfig().maid();
    }

    /**
     * 获取女仆
     */
    public static List<PermissionConfig> getMaid(Long groupId) {
        if (groupId == null || groupId == 0L) return new ArrayList<>();
        return getGroupConfig().maid().computeIfAbsent(groupId, k -> new ArrayList<>());
    }

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
        List<PermissionConfig> butler = getButler();
        return CollectionUtils.isNotNullOrEmpty(butler) && butler.stream().anyMatch(p -> qq.equals(p.id()));
    }

    /**
     * 判断是否全局管理员
     */
    public static boolean isGlobalOp(@Nonnull Long qq) {
        return isOwner(qq) || isButler(qq);
    }

    /**
     * 判断是否女仆
     */
    public static boolean isServant(@Nullable Long groupId, @Nonnull Long qq) {
        if (groupId == null || !isGroupIdValid(groupId)) return false;
        List<PermissionConfig> servant = getMaid(groupId);
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
        if (bot == null || !isGroupIdValid(groupId)) return false;
        ActionData<GroupMemberInfoResp> groupMemberInfo = bot.getGroupMemberInfo(groupId, qq, false);
        return bot.isActionDataNotEmpty(groupMemberInfo) && "owner".equalsIgnoreCase(groupMemberInfo.getData().getRole());
    }

    /**
     * 判断是否群管理
     */
    public static boolean isGroupAdmin(@Nullable BaniraBot bot, @Nullable Long groupId, @Nonnull Long qq) {
        if (bot == null || !isGroupIdValid(groupId)) return false;
        ActionData<GroupMemberInfoResp> groupMemberInfo = bot.getGroupMemberInfo(groupId, qq, false);
        return bot.isActionDataNotEmpty(groupMemberInfo) && "admin".equalsIgnoreCase(groupMemberInfo.getData().getRole());
    }

    /**
     * 判断是否群成员
     */
    public static boolean isGroupMember(@Nullable BaniraBot bot, @Nullable Long groupId, @Nonnull Long qq) {
        if (bot == null || !isGroupIdValid(groupId)) return false;
        ActionData<GroupMemberInfoResp> groupMemberInfo = bot.getGroupMemberInfo(groupId, qq, false);
        if (!bot.isActionDataNotEmpty(groupMemberInfo)) return false;
        String role = groupMemberInfo.getData().getRole();
        return !"admin".equalsIgnoreCase(role) && !"owner".equalsIgnoreCase(role);
    }

    /**
     * 判断是否在群内
     */
    public static boolean isInGroup(@Nullable BaniraBot bot, @Nullable Long groupId, @Nonnull Long qq) {
        if (bot == null || !isGroupIdValid(groupId)) return false;
        ActionData<GroupMemberInfoResp> groupMemberInfo = bot.getGroupMemberInfo(groupId, qq, false);
        return bot.isActionDataNotEmpty(groupMemberInfo);
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
        if (isMaid(groupId, a))
            return !isGroupOwner(bot, groupId, b)
                    && !isOwner(b)
                    && !isButler(b)
                    && !isGroupAdmin(bot, groupId, b)
                    && !isMaid(groupId, b);
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
        List<PermissionConfig> butler = getGroupConfig().maid().computeIfAbsent(0L, k -> new ArrayList<>());
        if (CollectionUtils.isNotNullOrEmpty(butler)) {
            butler.stream()
                    .filter(p -> qq.equals(p.id()))
                    .findFirst()
                    .ifPresent(p -> permissions.addAll(p.permissions()));
        }
        if (groupId != null && groupId > 0L) {
            List<PermissionConfig> servant = getGroupConfig().maid().computeIfAbsent(groupId, k -> new ArrayList<>());
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

    /**
     * 获取权限名称
     */
    public static List<String> getPermissionNames(EnumPermission permission) {
        InstructionsConfig insConfig = getInsConfig();
        switch (permission) {
            case APER, RPER -> {
                return insConfig.kanri().op();
            }
            case MUTE, MALL -> {
                return insConfig.kanri().mute();
            }
            case LOUD, LALL -> {
                return insConfig.kanri().loud();
            }
            case ATAL -> {
                return insConfig.base().atAll();
            }
            case CARD -> {
                return insConfig.kanri().card();
            }
            case TAG -> {
                return insConfig.kanri().tag();
            }
            case KICK -> {
                return insConfig.kanri().kick();
            }
            case RECA -> {
                return insConfig.kanri().withdraw();
            }
            case GARE -> {
                return insConfig.kanri().approve();
            }
            case AESS, RESS -> {
                return insConfig.kanri().essence();
            }
            case GNAM -> {
                return insConfig.kanri().groupName();
            }
            case AADM, RADM -> {
                return insConfig.kanri().admin();
            }
            case AMAI, RMAI -> {
                return insConfig.kanri().maid();
            }
            case ABUT, RBUT -> {
                return insConfig.kanri().butler();
            }
            default -> {
                return List.of();
            }
        }
    }

    // endregion 权限判断

    // region 指令

    public static String getInsPrefix() {
        String prefix = getInsConfig().prefix();
        return StringUtils.isNotNullOrEmpty(prefix) ? prefix : "";
    }

    public static String getInsPrefixWithSpace() {
        return getInsPrefix().trim() + " ";
    }

    public static String getKanriInsPrefix() {
        InstructionsConfig insConfig = getInsConfig();
        String prefix = StringUtils.isNotNullOrEmpty(insConfig.prefix()) ? insConfig.prefix().trim() + " " : "";
        if (CollectionUtils.isNotNullOrEmpty(insConfig.kanri().prefix())) {
            prefix += CollectionUtils.getRandomElement(insConfig.kanri().prefix());
        }
        return prefix;
    }

    public static String getKanriInsPrefixWithSpace() {
        return getKanriInsPrefix().trim() + " ";
    }

    // endregion 指令

    // region 文件缓存


    /**
     * 下载文件到缓存目录
     *
     * @return 文件名称列表
     */
    public static String downloadFileToCachePath(String url, EnumCacheFileType type) {
        byte[] bytes = HttpUtils.downloadBytes(url);
        if (bytes != null) {
            File file = new File(String.format("cache/%s/%s", type.name(), StringUtils.md5(bytes)));
            file = FileUtil.writeBytes(bytes, file);
            if (file != null) {
                return file.getName();
            }
        }
        return null;
    }

    /**
     * 下载文件到缓存目录
     *
     * @return 文件名称列表
     */
    public static List<String> downloadFileToCachePath(List<ArrayMsg> arrayMsgList) {
        List<String> fileNameList = new ArrayList<>();
        for (KeyValue<String, KeyValue<String, String>> url : arrayMsgList
                .stream()
                .filter(it -> MsgTypeEnum.unknown == it.getType() || MsgTypeEnum.image == it.getType() || MsgTypeEnum.video == it.getType())
                .filter(it -> StringUtils.isNotNullOrEmpty(it.getStringData("file")))
                .map(it -> new KeyValue<>(it.getStringData("url"), new KeyValue<>(it.getType().name(), it.getStringData("file"))))
                .toList()
        ) {
            byte[] bytes = HttpUtils.downloadBytes(url.getKey());
            if (bytes != null) {
                String type = url.getValue().getKey();
                if ("unknown".equals(type)) type = "file";
                File file = new File(String.format("cache/%s/%s", type, StringUtils.md5(bytes)));
                file = FileUtil.writeBytes(bytes, file);
                if (file != null) {
                    fileNameList.add(file.getName());
                    FileUtil.writeString(url.getKey()
                            , new File(file.getParent(), String.format("%s_%s.info", file.getName(), url.getValue().getValue()))
                            , StandardCharsets.UTF_8
                    );
                }
            }
        }
        return fileNameList;
    }

    /**
     * 下载消息中的附件(图片、视频、文件)至缓存路径并将其转为文件码
     */
    public static String replaceBaniraFileCode(String msg) {
        if (PlantCipher.isPlantToken(msg)) msg = PlantCipher.decode(msg);
        List<ArrayMsg> arrayMsgList = MessageConverser.stringToArray(msg);
        List<String> strings = downloadFileToCachePath(arrayMsgList);
        int index = 0;
        for (ArrayMsg it : arrayMsgList) {
            switch (it.getType()) {
                case unknown: {
                    if (StringUtils.isNotNullOrEmpty(it.getStringData("file_id"))) {
                        it.setType(MsgTypeEnum.text);
                        String file = getCacheRelativePath(strings.get(index++), EnumCacheFileType.file);
                        it.setData(mutableMapOf("text", FileCode.build(file)));
                    }
                }
                break;
                case image: {
                    it.setType(MsgTypeEnum.text);
                    String file = getCacheRelativePath(strings.get(index++), EnumCacheFileType.image);
                    it.setData(mutableMapOf("text", ImageCode.build(file)));
                }
                break;
                case video: {
                    it.setType(MsgTypeEnum.text);
                    String file = getCacheRelativePath(strings.get(index++), EnumCacheFileType.video);
                    it.setData(mutableMapOf("text", VideoCode.build(file)));
                }
                break;
                case record: {
                }
                break;
            }
        }
        return MessageConverser.arraysToString(arrayMsgList);
    }

    /**
     * 获取缓存文件相对路径
     */
    public static String getCacheRelativePath(String fileName, EnumCacheFileType fileType) {
        return String.format("cache/%s/%s", fileType, fileName);
    }

    /**
     * 获取缓存文件绝对路径
     */
    public static String getCacheAbsolutePath(String fileName, EnumCacheFileType fileType) {
        if (StringUtils.isNullOrEmpty(fileName)) return null;
        return new File(getCacheRelativePath(fileName, fileType)).getAbsolutePath();
    }

    public static boolean isLocalFile(String uri) {
        try {
            File file = new File(uri);
            return file.exists();
        } catch (Exception ignored) {
            return false;
        }
    }

    public static boolean isLocalCacheFile(String uri) {
        return Arrays.stream(EnumCacheFileType.values()).anyMatch(it -> isLocalCacheFile(uri, it));
    }

    public static boolean isLocalCacheFile(String uri, EnumCacheFileType type) {
        return isLocalFile(getCacheRelativePath(uri, type));
    }

    /**
     * 将文件路径转换为绝对路径
     *
     * @param uri 本地文件路径|本地缓存文件名|网络文件路径
     * @return 文件绝对路径
     */
    public static String convertFileUri(String uri) {
        if (isLocalFile(uri)) {
            return new File(uri).getAbsolutePath();
        } else if (isLocalCacheFile(uri)) {
            return Arrays.stream(EnumCacheFileType.values())
                    .filter(it -> isLocalCacheFile(uri, it))
                    .findFirst()
                    .map(it -> getCacheRelativePath(uri, it))
                    .orElse(uri);
        } else {
            String fileName = downloadFileToCachePath(uri, EnumCacheFileType.file);
            if (StringUtils.isNullOrEmpty(fileName)) return uri;
            return BaniraUtils.getCacheAbsolutePath(fileName, EnumCacheFileType.file);
        }
    }

    // endregion 文件缓存

    // region 其他

    @Nullable
    public static BaniraBot getBot(Long botId) {
        try {
            return new BaniraBot(SpringContextHolder.getBean(BotContainer.class).robots.get(botId));
        } catch (Exception e) {
            LOGGER.error("Failed to get bot: {}", botId, e);
            return null;
        }
    }

    public static boolean isValidUri(String uri) {
        try {
            URI.create(uri);
            return true;
        } catch (Exception e) {
            return false;
        }
    }

    public static boolean isValidNetAddress(String url) {
        try {
            return InetAddress.getByName(url) != null;
        } catch (Exception e) {
            return false;
        }
    }

    public static boolean isValidPath(String path) {
        try {
            Path.of(path);
            return true;
        } catch (Exception e) {
            return false;
        }
    }

    // endregion 其他

}
