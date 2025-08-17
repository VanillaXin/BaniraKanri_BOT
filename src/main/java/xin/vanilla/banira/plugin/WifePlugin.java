package xin.vanilla.banira.plugin;

import com.mikuac.shiro.annotation.GroupMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.common.utils.ShiroUtils;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.action.common.ActionData;
import com.mikuac.shiro.dto.action.common.ActionList;
import com.mikuac.shiro.dto.action.common.MsgId;
import com.mikuac.shiro.dto.action.response.GroupMemberInfoResp;
import com.mikuac.shiro.dto.action.response.LoginInfoResp;
import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.basic.OtherConfig;
import xin.vanilla.banira.config.entity.extended.WifeConfig;
import xin.vanilla.banira.domain.WifeRecord;
import xin.vanilla.banira.mapper.param.WifeRecordQueryParam;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.service.IWifeRecordManager;
import xin.vanilla.banira.util.*;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 抽老婆
 */
@Slf4j
@Shiro
@Component
public class WifePlugin extends BasePlugin {

    @Resource
    private IWifeRecordManager wifeRecordManager;

    private static final String SUCCESS_CONTENT = "$atUser 今天你的群友$wifeNick是\n$wifeHead『$wifeName』($wifeId) 喵！";
    private static final String FAIL_CONTENT = "$atUser 今天你已经有$wifeNick了喵！";
    private static final WifeConfig DISABLED = new WifeConfig("_DISABLED_", "_DISABLED_", "_DISABLED_", "_DISABLED_");
    private static final Map<String, Pattern> PATTERN_CACHE = BaniraUtils.mutableMapOf();
    private static final Random RANDOM = new Random();

    /**
     * 抽取
     */
    @GroupMessageHandler
    public boolean draw(Bot tob, GroupMessageEvent event) {
        BaniraBot bot = new BaniraBot(tob);

        Set<WifeConfig> configs = getWifeConfig(event.getGroupId());
        if (CollectionUtils.isNotNullOrEmpty(configs)) {
            String message = event.getMessage();
            Optional<WifeConfig> optional = configs.stream()
                    .filter(config -> this.getPattern(config).matcher(message).find())
                    .findFirst();
            if (optional.isPresent()) {
                WifeConfig config = optional.get();
                WifeRecord wifeRecord = this.getWifeRecord(event.getGroupId(), event.getUserId());

                Matcher matcher = this.getPattern(config).matcher(message);
                String wifeNick = RegUtils.extractParams(matcher, config.nick());

                if (wifeRecord == null) {
                    GroupMemberInfoResp wife = this.getRandomWife(bot, event.getGroupId());
                    if (wife != null) {

                        wifeRecord = new WifeRecord()
                                .setMsgId(String.valueOf(event.getMessageId()))
                                .setGroupId(event.getGroupId())
                                .setSenderId(event.getUserId())
                                .setTime(event.getTime())
                                .setWifeId(wife.getUserId())
                                .setWifeName(wife.getNickname())
                                .setWifeNick(wifeNick);
                        wifeRecordManager.addWifeRecord(wifeRecord);
                    }
                }
                if (wifeRecord == null) {
                    return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
                }
                // 今日首次 或 与首次相同
                else if (wifeNick.equals(wifeRecord.getWifeNick())) {
                    String content = this.replaceArgs(this.getSuccessContent(config), event, wifeRecord);

                    ActionData<MsgId> msgIdData = bot.sendGroupMsg(event.getGroupId(), content, false);
                    return bot.isActionDataMsgIdNotEmpty(msgIdData);
                }
                // 不是首次 且 与首次不同
                else {
                    String content = this.replaceArgs(this.getFailContent(config), event, wifeRecord);
                    ActionData<MsgId> msgIdData = bot.sendGroupMsg(event.getGroupId(), content, false);
                    return bot.isActionDataMsgIdNotEmpty(msgIdData);
                }
            }
        }
        return false;
    }

    /**
     * 配置
     */
    @GroupMessageHandler
    public boolean config(Bot tob, GroupMessageEvent event) {
        BaniraBot bot = new BaniraBot(tob);
        String message = event.getMessage();
        if (super.isCommand(message)
                && globalConfig.get().otherConfig().wifeInsConfig() != null
                && globalConfig.get().otherConfig().wifeInsConfig().stream().anyMatch(ins -> super.replaceCommand(message).startsWith(ins))
        ) {
            String argString = super.replaceCommand(message);
            String[] split = argString.split("\\s+");
            if (split.length < 2) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());

            String operate = split[1];
            // 启用
            if (globalConfig.get().instConfig().base().enable().contains(operate)) {
                groupConfig.get().otherConfig()
                        .computeIfAbsent(event.getGroupId(), k -> OtherConfig.empty())
                        .wifeConfig()
                        .removeIf(wife -> DISABLED.equals(wife)
                                || (DISABLED.reg().equals(wife.reg()) && DISABLED.nick().equals(wife.nick()))
                        );
                BaniraUtils.saveGroupConfig();
                return bot.setMsgEmojiLikeOk(event.getMessageId());
            }
            // 禁用
            else if (globalConfig.get().instConfig().base().disable().contains(operate)) {
                groupConfig.get().otherConfig()
                        .computeIfAbsent(event.getGroupId(), k -> OtherConfig.empty())
                        .wifeConfig()
                        .add(DISABLED);
                BaniraUtils.saveGroupConfig();
                return bot.setMsgEmojiLikeOk(event.getMessageId());
            }
            // 添加
            else if (globalConfig.get().instConfig().base().add().contains(operate)) {
                String[] args = argString.split("\\r\\n|\\r|\\n");
                if (args.length < 2 || args.length > 5) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
                WifeConfig wifeConfig = new WifeConfig(args[1]
                        , CollectionUtils.getOrDefault(args, 2, "老婆")
                        , CollectionUtils.getOrDefault(args, 3, SUCCESS_CONTENT)
                        , CollectionUtils.getOrDefault(args, 4, FAIL_CONTENT)
                );
                groupConfig.get().otherConfig()
                        .computeIfAbsent(event.getGroupId(), k -> OtherConfig.empty())
                        .wifeConfig().add(wifeConfig);
                BaniraUtils.saveGroupConfig();
                return bot.setMsgEmojiLikeOk(event.getMessageId());
            }
            // 删除
            else if (globalConfig.get().instConfig().base().del().contains(operate)) {
                String[] args = argString.split("\\r\\n|\\r|\\n");
                if (args.length < 2 || args.length > 5) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
                WifeConfig wifeConfig = new WifeConfig(args[1]
                        , CollectionUtils.getOrDefault(args, 2, null)
                        , CollectionUtils.getOrDefault(args, 3, null)
                        , CollectionUtils.getOrDefault(args, 4, null)
                );
                groupConfig.get().otherConfig()
                        .computeIfAbsent(event.getGroupId(), k -> OtherConfig.empty())
                        .wifeConfig()
                        .removeIf(config -> config.reg().equals(wifeConfig.reg())
                                && (StringUtils.isNullOrEmpty(wifeConfig.nick()) || config.nick().equals(wifeConfig.nick()))
                                && (StringUtils.isNullOrEmpty(wifeConfig.success()) || config.success().equals(wifeConfig.success()))
                                && (StringUtils.isNullOrEmpty(wifeConfig.fail()) || config.fail().equals(wifeConfig.fail()))
                        );
                BaniraUtils.saveGroupConfig();
                return bot.setMsgEmojiLikeOk(event.getMessageId());

            }
            // 查询
            else if (globalConfig.get().instConfig().base().list().contains(operate)) {
                Set<WifeConfig> wifeConfigs = groupConfig.get().otherConfig()
                        .computeIfAbsent(event.getGroupId(), k -> OtherConfig.empty())
                        .wifeConfig();
                if (wifeConfigs.isEmpty()) {
                    ActionData<MsgId> msgIdData = bot.sendGroupMsg(event.getGroupId(), "该群没有独立的配置喵！", false);
                    return bot.isActionDataMsgIdNotEmpty(msgIdData);
                } else {
                    LoginInfoResp loginInfoEx = bot.getLoginInfoEx();
                    List<Map<String, Object>> msg = new ArrayList<>();
                    msg.add(ShiroUtils.generateSingleMsg(event.getUserId(), event.getSender().getNickname(), event.getMessage()));
                    wifeConfigs.forEach(config -> msg.add(
                            ShiroUtils.generateSingleMsg(
                                    bot.getSelfId()
                                    , loginInfoEx.getNickname()
                                    , MsgUtils.builder()
                                            .text("表达式：").text(config.reg()).text("\n")
                                            .text("昵称：").text(config.nick()).text("\n")
                                            .text("成功：").text(config.success()).text("\n")
                                            .text("失败：").text(config.fail()).text("\n")
                                            .build()
                            )
                    ));
                    ActionData<MsgId> msgId = bot.sendGroupForwardMsg(event.getGroupId(), msg);
                    return bot.isActionDataMsgIdNotEmpty(msgId);
                }
            }
            // 未知
            else {
                return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            }
        }

        return false;
    }

    /**
     * TODO 统计
     */
    @GroupMessageHandler
    public boolean statistics(Bot tob, GroupMessageEvent event) {
        BaniraBot bot = new BaniraBot(tob);

        return false;
    }


    private Set<WifeConfig> getWifeConfig(long groupId) {
        Set<WifeConfig> wifeConfig = BaniraUtils.mutableSetOf();
        // 群聊配置
        if (groupConfig.get().otherConfig().containsKey(groupId)) {
            if (CollectionUtils.isNotNullOrEmpty(groupConfig.get().otherConfig().get(groupId).wifeConfig())) {
                wifeConfig = groupConfig.get().otherConfig().get(groupId).wifeConfig();
                // 判断群聊是否禁用
                if (wifeConfig.stream()
                        .anyMatch(wife -> DISABLED.equals(wife)
                                || (DISABLED.reg().equals(wife.reg()) && DISABLED.nick().equals(wife.nick()))
                        )
                ) {
                    return BaniraUtils.mutableSetOf();
                }
            }
        }
        // 全局配置
        if (wifeConfig.isEmpty()) {
            if (CollectionUtils.isNotNullOrEmpty(globalConfig.get().otherConfig().wifeConfig())) {
                wifeConfig = globalConfig.get().otherConfig().wifeConfig();
                // 判断全局是否禁用
                if (wifeConfig.stream()
                        .anyMatch(wife -> DISABLED.equals(wife)
                                || (DISABLED.reg().equals(wife.reg()) && DISABLED.nick().equals(wife.nick()))
                        )
                ) {
                    return BaniraUtils.mutableSetOf();
                }
            }
        }
        return wifeConfig;
    }

    private String getSuccessContent(WifeConfig config) {
        return StringUtils.isNotNullOrEmpty(config.success()) ? config.success() : SUCCESS_CONTENT;
    }

    private String getFailContent(WifeConfig config) {
        return StringUtils.isNotNullOrEmpty(config.fail()) ? config.fail() : FAIL_CONTENT;
    }

    private String replaceArgs(String content, GroupMessageEvent event, WifeRecord wifeRecord) {
        return content
                .replaceAll("\\$atUser|\\$atSender|\\$@user|\\$@sender|\\$atSenderId|\\$@senderId|\\$atUserId|\\$@userId", MsgUtils.builder().at(event.getUserId()).build())
                .replaceAll("\\$atWife|\\$atWifeId|\\$@wife|\\$@wifeId", MsgUtils.builder().at(wifeRecord.getWifeId()).build())
                .replaceAll("\\$wifeNick", ShiroUtils.escape2(wifeRecord.getWifeNick()))
                .replaceAll("\\$wifeName", ShiroUtils.escape2(wifeRecord.getWifeName()))
                .replaceAll("\\$wifeId", String.valueOf(wifeRecord.getWifeId()))
                .replaceAll("\\$wifeHead", MsgUtils.builder().img(ShiroUtils.getUserAvatar(wifeRecord.getWifeId(), 0)).build())
                .replaceAll("\\$userId|\\$senderId", String.valueOf(event.getUserId()))
                .replaceAll("\\$userName|\\$senderName|\\$senderNick", ShiroUtils.escape2(event.getSender().getNickname()))
                .replaceAll("\\$userHead|\\$senderHead", MsgUtils.builder().img(ShiroUtils.getUserAvatar(event.getUserId(), 0)).build())
                .replaceAll("\\$groupId", String.valueOf(event.getGroupId()))
                .replaceAll("\\$reply", MsgUtils.builder().reply(event.getMessageId()).build());
    }

    private Pattern getPattern(WifeConfig config) {
        return PATTERN_CACHE.computeIfAbsent(config.reg(), Pattern::compile);
    }

    private WifeRecord getWifeRecord(long groupId, long senderId) {
        Date current = new Date();
        long theDayStart = DateUtils.toTheDayStart(current).getTime() / 1000;
        long theDayEnd = DateUtils.toTheDayEnd(current).getTime() / 1000;

        WifeRecordQueryParam param = new WifeRecordQueryParam();
        param.setGroupId(groupId);
        param.setSenderId(senderId);
        param.setTimeByRange(theDayStart, theDayEnd);
        List<WifeRecord> wifeRecords = wifeRecordManager.getWifeRecordList(param);
        return CollectionUtils.isNotNullOrEmpty(wifeRecords) ? wifeRecords.getFirst() : null;
    }

    private GroupMemberInfoResp getRandomWife(BaniraBot bot, long groupId) {
        GroupMemberInfoResp result = null;
        ActionList<GroupMemberInfoResp> groupMemberList = bot.getGroupMemberList(groupId);
        if (bot.isActionDataNotEmpty(groupMemberList)) {
            List<GroupMemberInfoResp> data = groupMemberList.getData();
            result = data.get(RANDOM.nextInt(data.size()));
        }
        return result;
    }

}
