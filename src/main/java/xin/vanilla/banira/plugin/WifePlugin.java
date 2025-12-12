package xin.vanilla.banira.plugin;

import com.kennycason.kumo.CollisionMode;
import com.kennycason.kumo.WordCloud;
import com.kennycason.kumo.WordFrequency;
import com.kennycason.kumo.bg.CircleBackground;
import com.kennycason.kumo.font.FontWeight;
import com.kennycason.kumo.font.KumoFont;
import com.kennycason.kumo.font.scale.SqrtFontScalar;
import com.kennycason.kumo.nlp.FrequencyAnalyzer;
import com.kennycason.kumo.nlp.tokenizers.ChineseWordTokenizer;
import com.kennycason.kumo.palette.ColorPalette;
import com.mikuac.shiro.annotation.GroupMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.common.utils.ShiroUtils;
import com.mikuac.shiro.dto.action.common.ActionData;
import com.mikuac.shiro.dto.action.common.ActionList;
import com.mikuac.shiro.dto.action.common.MsgId;
import com.mikuac.shiro.dto.action.response.GroupMemberInfoResp;
import com.mikuac.shiro.dto.action.response.LoginInfoResp;
import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.basic.OtherConfig;
import xin.vanilla.banira.config.entity.extended.WifeConfig;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.WifeRecord;
import xin.vanilla.banira.mapper.param.WifeRecordQueryParam;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.service.IWifeRecordManager;
import xin.vanilla.banira.util.*;

import java.awt.*;
import java.io.ByteArrayOutputStream;
import java.util.*;
import java.util.List;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

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
     * 词云fontMetrics(用于计算文本长度)
     */
    private static final FontMetrics fontMetrics = new WordCloud(
            new Dimension(50, 50), CollisionMode.PIXEL_PERFECT
    ).getBufferedImage().createGraphics().getFontMetrics();

    /**
     * 获取帮助信息
     *
     * @param groupId 群组ID
     * @param types   帮助类型
     */
    @Nonnull
    @Override
    public List<String> getHelpInfo(Long groupId, @Nonnull String... types) {
        List<String> result = new ArrayList<>();
        String type = CollectionUtils.getFirst(types);
        if (insConfig.get().wife().stream().anyMatch(s -> StringUtils.isNullOrEmptyEx(type) || s.equalsIgnoreCase(type))) {
            List<WifeConfig> wifeConfig = getWifeConfig(groupId);
            result.add("抽老婆：\n" +
                    "抽取每日群友老婆喵。\n\n" +
                    wifeConfig.stream().map(WifeConfig::reg).sorted().toList()
            );
            result.add("抽老婆 - 年度统计：\n\n" +
                    BaniraUtils.getInsPrefixWithSpace() +
                    insConfig.get().wife() + " " +
                    insConfig.get().base().status()
            );
            result.add("抽老婆 - 设置规则：\n\n" +
                    "启用：\n" +
                    BaniraUtils.getInsPrefixWithSpace() +
                    insConfig.get().wife() + " " +
                    insConfig.get().base().enable() + "\n\n" +
                    "禁用：\n" +
                    BaniraUtils.getInsPrefixWithSpace() +
                    insConfig.get().wife() + " " +
                    insConfig.get().base().disable() + "\n\n" +
                    "添加规则：\n" +
                    BaniraUtils.getInsPrefixWithSpace() +
                    insConfig.get().wife() + " " +
                    insConfig.get().base().add() + "\n" +
                    "<正则表达式>\n" + "[<昵称表达式>]\n" + "[<抽取成功提示>]\n" + "[<抽取失败提示>]" + "\n\n" +
                    "删除规则：\n" +
                    BaniraUtils.getInsPrefixWithSpace() +
                    insConfig.get().wife() + " " +
                    insConfig.get().base().del() + "\n" +
                    "<正则表达式>\n" + "[<昵称表达式>]\n" + "[<抽取成功提示>]\n" + "[<抽取失败提示>]" + "\n\n" +
                    "查询规则：\n" +
                    BaniraUtils.getInsPrefixWithSpace() +
                    insConfig.get().wife() + " " +
                    insConfig.get().base().list()
            );
        }
        return result;
    }

    /**
     * 抽取
     */
    @GroupMessageHandler
    public boolean draw(BaniraBot bot, GroupMessageEvent event) {
        List<WifeConfig> configs = getWifeConfig(event.getGroupId());
        if (CollectionUtils.isNotNullOrEmpty(configs)) {
            String message = event.getMessage();
            Optional<WifeConfig> optional = configs.stream()
                    .filter(config -> this.getPattern(config).matcher(message).find())
                    .findFirst();
            if (optional.isPresent()) {
                WifeConfig config = optional.get();
                WifeRecord wifeRecord = this.getWifeRecord(event.getGroupId(), event.getUserId());

                Matcher matcher = this.getPattern(config).matcher(message);
                String wifeNick = RegexpHelper.extractParams(matcher, config.nick());
                if (wifeNick == null) wifeNick = config.nick();

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
    public boolean config(BaniraBot bot, GroupMessageEvent event) {
        BaniraCodeContext context = new BaniraCodeContext(bot, event);
        if (super.isCommand(context)
                && insConfig.get().wife().stream().anyMatch(ins -> super.deleteCommandPrefix(context).startsWith(ins + " "))
        ) {
            String argString = super.deleteCommandPrefix(context);
            String[] split = argString.split("\\s+");
            if (split.length < 2) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());

            String operate = split[1];
            // 启用
            if (insConfig.get().base().enable().contains(operate)) {
                if (!bot.isAdmin(event.getGroupId(), event.getUserId()))
                    return bot.setMsgEmojiLikeNo(event.getMessageId());
                groupConfig.get().otherConfig()
                        .computeIfAbsent(event.getGroupId(), k -> new OtherConfig())
                        .wifeConfig()
                        .removeIf(wife -> DISABLED.equals(wife)
                                || (DISABLED.reg().equals(wife.reg()) && DISABLED.nick().equals(wife.nick()))
                        );
                BaniraUtils.saveGroupConfig();
                return bot.setMsgEmojiLikeOk(event.getMessageId());
            }
            // 禁用
            else if (insConfig.get().base().disable().contains(operate)) {
                if (!bot.isAdmin(event.getGroupId(), event.getUserId()))
                    return bot.setMsgEmojiLikeNo(event.getMessageId());
                groupConfig.get().otherConfig()
                        .computeIfAbsent(event.getGroupId(), k -> new OtherConfig())
                        .wifeConfig()
                        .add(DISABLED);
                BaniraUtils.saveGroupConfig();
                return bot.setMsgEmojiLikeOk(event.getMessageId());
            }
            // 添加
            else if (insConfig.get().base().add().contains(operate)) {
                if (!bot.isAdmin(event.getGroupId(), event.getUserId()))
                    return bot.setMsgEmojiLikeNo(event.getMessageId());
                String[] args = argString.split("\\r\\n|\\r|\\n");
                if (args.length < 2 || args.length > 5) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
                WifeConfig wifeConfig = new WifeConfig(args[1]
                        , CollectionUtils.getOrDefault(args, 2, "老婆")
                        , CollectionUtils.getOrDefault(args, 3, SUCCESS_CONTENT)
                        , CollectionUtils.getOrDefault(args, 4, FAIL_CONTENT)
                );
                groupConfig.get().otherConfig()
                        .computeIfAbsent(event.getGroupId(), k -> new OtherConfig())
                        .wifeConfig().add(wifeConfig);
                BaniraUtils.saveGroupConfig();
                return bot.setMsgEmojiLikeOk(event.getMessageId());
            }
            // 删除
            else if (insConfig.get().base().del().contains(operate)) {
                if (!bot.isAdmin(event.getGroupId(), event.getUserId()))
                    return bot.setMsgEmojiLikeNo(event.getMessageId());
                String[] args = argString.split("\\r\\n|\\r|\\n");
                if (args.length < 2 || args.length > 5) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
                WifeConfig wifeConfig = new WifeConfig(args[1]
                        , CollectionUtils.getOrDefault(args, 2, null)
                        , CollectionUtils.getOrDefault(args, 3, null)
                        , CollectionUtils.getOrDefault(args, 4, null)
                );
                groupConfig.get().otherConfig()
                        .computeIfAbsent(event.getGroupId(), k -> new OtherConfig())
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
            else if (insConfig.get().base().list().contains(operate)) {
                List<WifeConfig> wifeConfigs = groupConfig.get().otherConfig()
                        .computeIfAbsent(event.getGroupId(), k -> new OtherConfig())
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
            // 统计
            else if (insConfig.get().base().status().contains(operate)) {
                return this.statistics(bot, event, Arrays.copyOfRange(split, 2, split.length));
            }
            // 未知
            else {
                return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            }
        }

        return false;
    }

    /**
     * 统计
     */
    private boolean statistics(BaniraBot bot, GroupMessageEvent event, String[] args) {
        LoginInfoResp loginInfoEx = bot.getLoginInfoEx();

        Date current = new Date();
        Date theYearStart = DateUtils.toTheYearStart(current);
        Date theYearEnd = DateUtils.toTheYearEnd(current);

        List<Map<String, Object>> forwardMsg = new ArrayList<>();
        forwardMsg.add(ShiroUtils.generateSingleMsg(event.getUserId(), event.getSender().getNickname(), event.getMessage()));

        // 个人
        {
            WifeRecordQueryParam param = new WifeRecordQueryParam();
            param.setSenderId(event.getUserId());
            param.setTimeByRange(DateUtils.getTimestamp(theYearStart), DateUtils.getTimestamp(theYearEnd));
            List<WifeRecord> wifeRecordList = wifeRecordManager.getWifeRecordList(param);

            Map<Long, WifeRecord> recordMap = wifeRecordList.stream()
                    .sorted(Comparator.comparingLong(WifeRecord::getTime))
                    .collect(Collectors.toMap(WifeRecord::getWifeId, Function.identity(), (oldVal, newVal) -> newVal));

            int allCount = wifeRecordList.size();
            // 分别wifeId和wifeNick次数最多的情况
            Map<String, Long> nickCurrentMap = wifeRecordList.stream()
                    .filter(record -> record.getTime() >= DateUtils.getTimestamp(theYearStart))
                    .filter(record -> record.getTime() <= DateUtils.getTimestamp(theYearEnd))
                    .map(WifeRecord::getWifeNick)
                    .collect(Collectors.groupingBy(s -> s, Collectors.counting()));
            String nickCurrent = nickCurrentMap
                    .entrySet().stream().max(Map.Entry.comparingByValue())
                    .map(Map.Entry::getKey).orElse("");

            Map<Long, Long> wifeCurrentMap = wifeRecordList.stream()
                    .filter(record -> record.getTime() >= DateUtils.getTimestamp(theYearStart))
                    .filter(record -> record.getTime() <= DateUtils.getTimestamp(theYearEnd))
                    .map(WifeRecord::getWifeId)
                    .collect(Collectors.groupingBy(s -> s, Collectors.counting()));
            Long wifeCurrent = wifeCurrentMap
                    .entrySet().stream().max(Map.Entry.comparingByValue())
                    .map(Map.Entry::getKey).orElse(null);

            if (nickCurrentMap.isEmpty()) {
                forwardMsg.add(ShiroUtils.generateSingleMsg(
                        bot.getSelfId()
                        , loginInfoEx.getNickname()
                        , MsgUtils.builder()
                                .text("暂无个人记录")
                                .build()
                ));
            }
            //
            else {
                forwardMsg.add(ShiroUtils.generateSingleMsg(
                        bot.getSelfId()
                        , loginInfoEx.getNickname()
                        , MsgUtils.builder()
                                .text(event.getSender().getNickname())
                                .text(" 的年度抽老婆报告")
                                .build()
                ));

                forwardMsg.add(ShiroUtils.generateSingleMsg(
                        bot.getSelfId()
                        , loginInfoEx.getNickname()
                        , MsgUtils.builder()
                                .text("阁下在")
                                .text(String.valueOf(DateUtils.getYearPart(current)))
                                .text("年总计抽取老婆")
                                .text(String.valueOf(allCount))
                                .text("次")
                                .build()
                ));

                forwardMsg.add(ShiroUtils.generateSingleMsg(
                        bot.getSelfId()
                        , loginInfoEx.getNickname()
                        , MsgUtils.builder()
                                .text("其中使用得最多的昵称是: ")
                                .text(nickCurrent)
                                .text("\n共使用了")
                                .text(String.valueOf(nickCurrentMap.get(nickCurrent)))
                                .text("次")
                                .text("\n是对")
                                .text(nickCurrent)
                                .text("情有独钟吗")
                                .build()
                ));

                if (wifeCurrent != null) {
                    forwardMsg.add(ShiroUtils.generateSingleMsg(
                                    bot.getSelfId()
                                    , loginInfoEx.getNickname()
                                    , MsgUtils.builder()
                                            .text("与阁下最有缘分的群友是:")
                                            .img(ShiroUtils.getUserAvatar(wifeCurrent, 0))
                                            .text("\n『")
                                            .text(recordMap.get(wifeCurrent).getWifeName())
                                            .text("』(")
                                            .text(String.valueOf(wifeCurrent))
                                            .text(")")
                                            .text("\n共邂逅了")
                                            .text(String.valueOf(wifeCurrentMap.get(wifeCurrent)))
                                            .text("次")
                                            .build()
                            )
                    );
                }

                // 词云
                {
                    final List<WordFrequency> wordFrequencies = nickCurrentMap.entrySet().stream()
                            .filter(entry -> StringUtils.isNotNullOrEmpty(entry.getKey()))
                            .filter(entry -> fontMetrics.stringWidth(entry.getKey()) > 0)
                            .map(entry -> new WordFrequency(entry.getKey(), Math.toIntExact(entry.getValue())))
                            .collect(Collectors.toList());
                    final FrequencyAnalyzer frequencyAnalyzer = new FrequencyAnalyzer();
                    frequencyAnalyzer.setWordFrequenciesToReturn(600);
                    frequencyAnalyzer.setMinWordLength(2);
                    frequencyAnalyzer.setWordTokenizer(new ChineseWordTokenizer());
                    final Dimension dimension = new Dimension(600, 600);
                    final WordCloud wordCloud = new WordCloud(dimension, CollisionMode.PIXEL_PERFECT);
                    wordCloud.setPadding(2);
                    // wordCloud.setBackgroundColor(Color.WHITE);
                    wordCloud.setBackground(new CircleBackground(300));
                    wordCloud.setColorPalette(new ColorPalette(new Color(0xD5CFFA), new Color(0xBBB1FA), new Color(0x9A8CF5), new Color(0x806EF5)));
                    wordCloud.setKumoFont(new KumoFont("YueYuan", FontWeight.PLAIN));
                    wordCloud.setFontScalar(new SqrtFontScalar(12, 45));
                    wordCloud.build(wordFrequencies);
                    ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
                    wordCloud.writeToStreamAsPNG(outputStream);

                    forwardMsg.add(ShiroUtils.generateSingleMsg(
                            bot.getSelfId()
                            , loginInfoEx.getNickname()
                            , MsgUtils.builder()
                                    .text("阁下的年度词云")
                                    .img(outputStream.toByteArray())
                                    .build()
                    ));
                }

                // 老婆云
                {
                    final List<WordFrequency> wordFrequencies = wifeCurrentMap.entrySet().stream()
                            .filter(entry -> entry.getKey() != null)
                            .map(entry -> {
                                WifeRecord wifeRecord = recordMap.get(entry.getKey());
                                return new WordFrequency(wifeRecord != null
                                        && StringUtils.isNotNullOrEmpty(wifeRecord.getWifeName())
                                        && fontMetrics.stringWidth(wifeRecord.getWifeName()) > 0
                                        ? wifeRecord.getWifeName()
                                        : String.valueOf(entry.getKey()), Math.toIntExact(entry.getValue()));
                            })
                            .collect(Collectors.toList());
                    final FrequencyAnalyzer frequencyAnalyzer = new FrequencyAnalyzer();
                    frequencyAnalyzer.setWordFrequenciesToReturn(600);
                    frequencyAnalyzer.setMinWordLength(2);
                    frequencyAnalyzer.setWordTokenizer(new ChineseWordTokenizer());
                    final Dimension dimension = new Dimension(600, 600);
                    final WordCloud wordCloud = new WordCloud(dimension, CollisionMode.PIXEL_PERFECT);
                    wordCloud.setPadding(2);
                    // wordCloud.setBackgroundColor(Color.WHITE);
                    wordCloud.setBackground(new CircleBackground(300));
                    wordCloud.setColorPalette(new ColorPalette(new Color(0xD5CFFA), new Color(0xBBB1FA), new Color(0x9A8CF5), new Color(0x806EF5)));
                    wordCloud.setKumoFont(new KumoFont("YueYuan", FontWeight.PLAIN));
                    wordCloud.setFontScalar(new SqrtFontScalar(12, 45));
                    wordCloud.build(wordFrequencies);
                    ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
                    wordCloud.writeToStreamAsPNG(outputStream);

                    forwardMsg.add(ShiroUtils.generateSingleMsg(
                            bot.getSelfId()
                            , loginInfoEx.getNickname()
                            , MsgUtils.builder()
                                    .text("阁下的年度老婆云")
                                    .img(outputStream.toByteArray())
                                    .build()
                    ));
                }
            }
        }

        // 群内
        {
            WifeRecordQueryParam param = new WifeRecordQueryParam();
            param.setGroupId(event.getGroupId());
            param.setTimeByRange(DateUtils.getTimestamp(theYearStart), DateUtils.getTimestamp(theYearEnd));
            List<WifeRecord> wifeRecordList = wifeRecordManager.getWifeRecordList(param);

            Map<Long, WifeRecord> recordMap = wifeRecordList.stream()
                    .sorted(Comparator.comparingLong(WifeRecord::getTime))
                    .collect(Collectors.toMap(WifeRecord::getWifeId, Function.identity(), (oldVal, newVal) -> newVal));

            // 分别wifeId和wifeNick次数最多的情况
            Map<String, Long> nickCurrentMap = wifeRecordList.stream()
                    .filter(record -> record.getTime() >= DateUtils.getTimestamp(theYearStart))
                    .filter(record -> record.getTime() <= DateUtils.getTimestamp(theYearEnd))
                    .map(WifeRecord::getWifeNick)
                    .collect(Collectors.groupingBy(s -> s, Collectors.counting()));
            String nickCurrent = nickCurrentMap
                    .entrySet().stream().max(Map.Entry.comparingByValue())
                    .map(Map.Entry::getKey).orElse("");

            Map<Long, Long> wifeCurrentMap = wifeRecordList.stream()
                    .filter(record -> record.getTime() >= DateUtils.getTimestamp(theYearStart))
                    .filter(record -> record.getTime() <= DateUtils.getTimestamp(theYearEnd))
                    .map(WifeRecord::getWifeId)
                    .collect(Collectors.groupingBy(s -> s, Collectors.counting()));
            Long wifeCurrent = wifeCurrentMap
                    .entrySet().stream().max(Map.Entry.comparingByValue())
                    .map(Map.Entry::getKey).orElse(null);

            if (nickCurrentMap.isEmpty()) {
                forwardMsg.add(ShiroUtils.generateSingleMsg(
                        bot.getSelfId()
                        , loginInfoEx.getNickname()
                        , MsgUtils.builder()
                                .text("暂无群组记录")
                                .build()
                ));
            }
            //
            else {
                forwardMsg.add(ShiroUtils.generateSingleMsg(
                        bot.getSelfId()
                        , loginInfoEx.getNickname()
                        , MsgUtils.builder()
                                .text(String.valueOf(DateUtils.getYearPart(current)))
                                .text("群内年度最佳昵称: ")
                                .text(nickCurrent)
                                .text("\n共被使用")
                                .text(String.valueOf(nickCurrentMap.get(nickCurrent)))
                                .text("次")
                                .build()
                ));

                if (wifeCurrent != null) {
                    forwardMsg.add(ShiroUtils.generateSingleMsg(
                            bot.getSelfId()
                            , loginInfoEx.getNickname()
                            , MsgUtils.builder()
                                    .text(String.valueOf(DateUtils.getYearPart(current)))
                                    .text("群内年度最佳群友: ")
                                    .img(ShiroUtils.getUserAvatar(wifeCurrent, 0))
                                    .text("\n『")
                                    .text(recordMap.get(wifeCurrent).getWifeName())
                                    .text("』(")
                                    .text(String.valueOf(wifeCurrent))
                                    .text(")")
                                    .text("\n共被抽中")
                                    .text(String.valueOf(wifeCurrentMap.get(wifeCurrent)))
                                    .text("次")
                                    .build()
                    ));
                }

                // 词云
                {
                    final List<WordFrequency> wordFrequencies = nickCurrentMap.entrySet().stream()
                            .filter(entry -> StringUtils.isNotNullOrEmpty(entry.getKey()))
                            .filter(entry -> fontMetrics.stringWidth(entry.getKey()) > 0)
                            .map(entry -> new WordFrequency(entry.getKey(), Math.toIntExact(entry.getValue())))
                            .collect(Collectors.toList());
                    final FrequencyAnalyzer frequencyAnalyzer = new FrequencyAnalyzer();
                    frequencyAnalyzer.setWordFrequenciesToReturn(600);
                    frequencyAnalyzer.setMinWordLength(2);
                    frequencyAnalyzer.setWordTokenizer(new ChineseWordTokenizer());
                    final Dimension dimension = new Dimension(600, 600);
                    final WordCloud wordCloud = new WordCloud(dimension, CollisionMode.PIXEL_PERFECT);
                    wordCloud.setPadding(2);
                    // wordCloud.setBackgroundColor(Color.WHITE);
                    wordCloud.setBackground(new CircleBackground(300));
                    wordCloud.setColorPalette(new ColorPalette(new Color(0xD5CFFA), new Color(0xBBB1FA), new Color(0x9A8CF5), new Color(0x806EF5)));
                    wordCloud.setKumoFont(new KumoFont("YueYuan", FontWeight.PLAIN));
                    wordCloud.setFontScalar(new SqrtFontScalar(12, 45));
                    wordCloud.build(wordFrequencies);
                    ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
                    wordCloud.writeToStreamAsPNG(outputStream);

                    forwardMsg.add(ShiroUtils.generateSingleMsg(
                            bot.getSelfId()
                            , loginInfoEx.getNickname()
                            , MsgUtils.builder()
                                    .text("群内年度词云")
                                    .img(outputStream.toByteArray())
                                    .build()
                    ));
                }
            }
        }

        // 全局
        {
            WifeRecordQueryParam param = new WifeRecordQueryParam();
            param.setTimeByRange(DateUtils.getTimestamp(theYearStart), DateUtils.getTimestamp(theYearEnd));
            List<WifeRecord> wifeRecordList = wifeRecordManager.getWifeRecordList(param);

            Map<Long, WifeRecord> recordMap = wifeRecordList.stream()
                    .sorted(Comparator.comparingLong(WifeRecord::getTime))
                    .collect(Collectors.toMap(WifeRecord::getWifeId, Function.identity(), (oldVal, newVal) -> newVal));

            // 分别wifeId和wifeNick次数最多的情况
            Map<String, Long> nickCurrentMap = wifeRecordList.stream()
                    .filter(record -> record.getTime() >= DateUtils.getTimestamp(theYearStart))
                    .filter(record -> record.getTime() <= DateUtils.getTimestamp(theYearEnd))
                    .map(WifeRecord::getWifeNick)
                    .collect(Collectors.groupingBy(s -> s, Collectors.counting()));
            String nickCurrent = nickCurrentMap
                    .entrySet().stream().max(Map.Entry.comparingByValue())
                    .map(Map.Entry::getKey).orElse("");

            Map<Long, Long> wifeCurrentMap = wifeRecordList.stream()
                    .filter(record -> record.getTime() >= DateUtils.getTimestamp(theYearStart))
                    .filter(record -> record.getTime() <= DateUtils.getTimestamp(theYearEnd))
                    .map(WifeRecord::getWifeId)
                    .collect(Collectors.groupingBy(s -> s, Collectors.counting()));
            Long wifeCurrent = wifeCurrentMap
                    .entrySet().stream().max(Map.Entry.comparingByValue())
                    .map(Map.Entry::getKey).orElse(null);
            if (nickCurrentMap.isEmpty()) {
                forwardMsg.add(ShiroUtils.generateSingleMsg(
                        bot.getSelfId()
                        , loginInfoEx.getNickname()
                        , MsgUtils.builder()
                                .text("暂无全局记录")
                                .build()
                ));
            }
            //
            else {
                forwardMsg.add(ShiroUtils.generateSingleMsg(
                        bot.getSelfId()
                        , loginInfoEx.getNickname()
                        , MsgUtils.builder()
                                .text(String.valueOf(DateUtils.getYearPart(current)))
                                .text("年度最佳昵称: ")
                                .text(nickCurrent)
                                .text("\n共被使用")
                                .text(String.valueOf(nickCurrentMap.get(nickCurrent)))
                                .text("次")
                                .build()
                ));

                if (wifeCurrent != null) {
                    forwardMsg.add(ShiroUtils.generateSingleMsg(
                            bot.getSelfId()
                            , loginInfoEx.getNickname()
                            , MsgUtils.builder()
                                    .text(String.valueOf(DateUtils.getYearPart(current)))
                                    .text("年度最佳群友: ")
                                    .img(ShiroUtils.getUserAvatar(wifeCurrent, 0))
                                    .text("\n『")
                                    .text(recordMap.get(wifeCurrent).getWifeName())
                                    .text("』(")
                                    .text(String.valueOf(wifeCurrent))
                                    .text(")")
                                    .text("\n共被抽中")
                                    .text(String.valueOf(wifeCurrentMap.get(wifeCurrent)))
                                    .text("次")
                                    .build()
                    ));
                }

                // 词云
                {
                    final List<WordFrequency> wordFrequencies = nickCurrentMap.entrySet().stream()
                            .filter(entry -> StringUtils.isNotNullOrEmpty(entry.getKey()))
                            .filter(entry -> fontMetrics.stringWidth(entry.getKey()) > 0)
                            .map(entry -> new WordFrequency(entry.getKey(), Math.toIntExact(entry.getValue())))
                            .collect(Collectors.toList());
                    final FrequencyAnalyzer frequencyAnalyzer = new FrequencyAnalyzer();
                    frequencyAnalyzer.setWordFrequenciesToReturn(600);
                    frequencyAnalyzer.setMinWordLength(2);
                    frequencyAnalyzer.setWordTokenizer(new ChineseWordTokenizer());
                    final Dimension dimension = new Dimension(600, 600);
                    final WordCloud wordCloud = new WordCloud(dimension, CollisionMode.PIXEL_PERFECT);
                    wordCloud.setPadding(2);
                    // wordCloud.setBackgroundColor(Color.WHITE);
                    wordCloud.setBackground(new CircleBackground(300));
                    wordCloud.setColorPalette(new ColorPalette(new Color(0xD5CFFA), new Color(0xBBB1FA), new Color(0x9A8CF5), new Color(0x806EF5)));
                    wordCloud.setKumoFont(new KumoFont("YueYuan", FontWeight.PLAIN));
                    wordCloud.setFontScalar(new SqrtFontScalar(12, 45));
                    wordCloud.build(wordFrequencies);
                    ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
                    wordCloud.writeToStreamAsPNG(outputStream);

                    forwardMsg.add(ShiroUtils.generateSingleMsg(
                            bot.getSelfId()
                            , loginInfoEx.getNickname()
                            , MsgUtils.builder()
                                    .text("年度词云")
                                    .img(outputStream.toByteArray())
                                    .build()
                    ));
                }
            }
        }

        ActionData<MsgId> msgId = bot.sendGroupForwardMsg(event.getGroupId(), forwardMsg);
        return bot.isActionDataMsgIdNotEmpty(msgId);
    }


    private List<WifeConfig> getWifeConfig(Long groupId) {
        List<WifeConfig> wifeConfig = new ArrayList<>();
        OtherConfig othersConfig = BaniraUtils.getOthersConfig(groupId);
        if (CollectionUtils.isNotNullOrEmpty(othersConfig.wifeConfig())) {
            wifeConfig = othersConfig.wifeConfig();
            // 判断群聊是否禁用
            if (wifeConfig.stream()
                    .anyMatch(wife -> DISABLED.equals(wife)
                            || (DISABLED.reg().equals(wife.reg()) && DISABLED.nick().equals(wife.nick()))
                    )
            ) {
                return new ArrayList<>();
            }
        }
        // 全局配置
        if (wifeConfig.isEmpty()) {
            othersConfig = BaniraUtils.getOthersConfig();
            if (CollectionUtils.isNotNullOrEmpty(othersConfig.wifeConfig())) {
                wifeConfig = othersConfig.wifeConfig();
                // 判断全局是否禁用
                if (wifeConfig.stream()
                        .anyMatch(wife -> DISABLED.equals(wife)
                                || (DISABLED.reg().equals(wife.reg()) && DISABLED.nick().equals(wife.nick()))
                        )
                ) {
                    return new ArrayList<>();
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
