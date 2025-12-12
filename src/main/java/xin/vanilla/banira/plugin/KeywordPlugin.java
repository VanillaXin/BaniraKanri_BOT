package xin.vanilla.banira.plugin;

import com.google.gson.JsonObject;
import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.GroupIncreaseHandler;
import com.mikuac.shiro.annotation.GroupPokeNoticeHandler;
import com.mikuac.shiro.annotation.PrivatePokeNoticeHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.common.utils.ShiroUtils;
import com.mikuac.shiro.constant.ActionParams;
import com.mikuac.shiro.dto.action.common.ActionData;
import com.mikuac.shiro.dto.action.common.MsgId;
import com.mikuac.shiro.dto.action.response.GetForwardMsgResp;
import com.mikuac.shiro.dto.action.response.LoginInfoResp;
import com.mikuac.shiro.dto.action.response.MsgResp;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import com.mikuac.shiro.dto.event.message.MessageEvent;
import com.mikuac.shiro.dto.event.notice.GroupIncreaseNoticeEvent;
import com.mikuac.shiro.dto.event.notice.PokeNoticeEvent;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.coder.common.EventCoder;
import xin.vanilla.banira.coder.event.InGroupCode;
import xin.vanilla.banira.coder.event.PokeCode;
import xin.vanilla.banira.config.entity.basic.BaseInstructionsConfig;
import xin.vanilla.banira.config.entity.basic.KeyInstructionsConfig;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.KeyValue;
import xin.vanilla.banira.domain.KeywordRecord;
import xin.vanilla.banira.domain.PageResult;
import xin.vanilla.banira.enums.EnumKeywordType;
import xin.vanilla.banira.enums.EnumMessageType;
import xin.vanilla.banira.mapper.param.KeywordRecordQueryParam;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.service.IKeywordManager;
import xin.vanilla.banira.service.IKeywordRecordManager;
import xin.vanilla.banira.start.SpringContextHolder;
import xin.vanilla.banira.util.*;

import java.util.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Matcher;

/**
 * 关键词回复
 */
@Slf4j
@Shiro
@Component
public class KeywordPlugin extends BasePlugin {

    @Resource
    private IKeywordManager keywordManager;
    @Resource
    private IKeywordRecordManager keywordRecordManager;
    @Autowired(required = false)
    private List<EventCoder> eventCoders = new ArrayList<>();

    /**
     * 获取帮助信息
     *
     * @param groupId 群组ID
     * @param types   帮助类型
     */
    @Nonnull
    @Override
    public List<String> getHelpInfo(@Nullable Long groupId, @Nonnull String... types) {
        List<String> result = new ArrayList<>();
        String type = CollectionUtils.getFirst(types);
        KeyInstructionsConfig keyIns = BaniraUtils.getKeyIns();
        BaseInstructionsConfig baseIns = BaniraUtils.getBaseIns();
        String prefixWithSpace = BaniraUtils.getInsPrefixWithSpace();
        String operate = CollectionUtils.getOrDefault(types, 1, "");

        if (keyIns.locator().stream().anyMatch(s -> StringUtils.isNullOrEmptyEx(type) || s.getKey().equalsIgnoreCase(type))) {
            if (baseIns.example().contains(operate)) {
                result.add("关键词回复 - 增加：\n\n" +
                        "例子1：" + "\n" +
                        prefixWithSpace +
                        keyIns.locator().getFirst().getKey() + " " +
                        baseIns.add().getFirst() + " " +
                        keyIns.exactly().getFirst() + " " +
                        "关键词" + " " +
                        keyIns.locator().getFirst().getValue() + " " +
                        "回复内容" + "\n\n" +
                        "例子2：" + "\n" +
                        prefixWithSpace +
                        keyIns.locator().getLast().getKey() + " " +
                        baseIns.add().getLast() + " " +
                        baseIns.global().getFirst() + " " +
                        keyIns.contain().getFirst() + " " +
                        "关键词" + " " +
                        keyIns.locator().getLast().getValue() + " " +
                        "回复内容"
                );
                result.add("关键词回复 - 删除：\n\n" +
                        "例子1：" + "\n" +
                        prefixWithSpace +
                        keyIns.locator().getFirst().getKey() + " " +
                        baseIns.del().getFirst() + " " +
                        baseIns.global().getFirst() + " " +
                        keyIns.pinyin().getFirst() + " " +
                        "关键词" + " " +
                        keyIns.locator().getFirst().getValue() + " " +
                        "回复内容" + "\n\n" +
                        "例子2：" + "\n" +
                        prefixWithSpace +
                        keyIns.locator().getLast().getKey() + " " +
                        baseIns.del().getLast() + " " +
                        baseIns.global().getLast() + " " +
                        keyIns.regex().getFirst() + " " +
                        "关键词"
                );
            } else {
                Set<String> keywordTargets = BaniraUtils.mutableSetOf("<\\d{5,10}>");
                keywordTargets.addAll(baseIns.that());
                keywordTargets.addAll(baseIns.global());

                Set<String> keywordTypes = BaniraUtils.mutableSetOf();
                keywordTypes.addAll(keyIns.exactly());
                keywordTypes.addAll(keyIns.contain());
                keywordTypes.addAll(keyIns.pinyin());
                keywordTypes.addAll(keyIns.regex());

                result.add("关键词回复 - 增加：\n" +
                        "增加关键词回复规则。可选帮助参数：" + baseIns.example() + "\n\n" +
                        "用法1：\n" +
                        prefixWithSpace +
                        keyIns.locator().stream().map(KeyValue::getKey).toList() + " " +
                        baseIns.add() + " " +
                        String.format("[%s, %s, <群号>]"
                                , String.join(", ", baseIns.global())
                                , String.join(", ", baseIns.that())
                        ) + " " +
                        keywordTypes + " " +
                        "<关键词>" + " " +
                        keyIns.locator().stream().map(KeyValue::getValue).toList() + " " +
                        "<回复内容>"
                );
                result.add("关键词回复 - 删除：\n" +
                        "删除关键词回复规则。可选帮助参数：" + baseIns.example() + "\n\n" +
                        "用法1：\n" +
                        prefixWithSpace +
                        keyIns.locator().stream().map(KeyValue::getKey).toList() + " " +
                        baseIns.del() + " " +
                        String.format("[%s, %s, <群号>]"
                                , String.join(", ", baseIns.global())
                                , String.join(", ", baseIns.that())
                        ) + " " +
                        keywordTypes + " " +
                        "<关键词>" + " " +
                        keyIns.locator().stream().map(KeyValue::getValue).toList() + " " +
                        "<回复内容>" + "\n\n" +
                        "用法2：(根据关键词编号删除)\n" +
                        prefixWithSpace +
                        keyIns.locator().stream().map(KeyValue::getKey).toList() + " " +
                        baseIns.del() + " " +
                        "<关键词编号> ..." + "\n\n" +
                        "用法3：(回复添加成功的响应消息)\n" +
                        prefixWithSpace +
                        keyIns.locator().stream().map(KeyValue::getKey).toList() + " " +
                        baseIns.del()
                );
                result.add("关键词回复 - 启用：\n" +
                        "启用关键词回复规则。\n\n" +
                        "用法1：(根据关键词编号启用)\n" +
                        prefixWithSpace +
                        keyIns.locator().stream().map(KeyValue::getKey).toList() + " " +
                        baseIns.enable() + " " +
                        "<关键词编号> ..." + "\n\n" +
                        "用法2：(回复添加成功的响应消息)\n" +
                        prefixWithSpace +
                        keyIns.locator().stream().map(KeyValue::getKey).toList() + " " +
                        baseIns.add()
                );
                result.add("关键词回复 - 查询：\n\n" +
                        prefixWithSpace +
                        keyIns.locator().stream().map(KeyValue::getKey).toList() + " " +
                        baseIns.list() + " " +
                        "[<页数>]" + " " +
                        "<关键词内容>"
                );
            }
        }
        return result;
    }

    @AnyMessageHandler
    public boolean config(BaniraBot bot, AnyMessageEvent event) {
        BaniraCodeContext context = new BaniraCodeContext(bot, event);

        KeyInstructionsConfig keyIns = BaniraUtils.getKeyIns();
        BaseInstructionsConfig baseIns = BaniraUtils.getBaseIns();

        // 精准添加/删除
        if (super.isKeywordCommand(context)) {
            Matcher matcher = super.getKeywordCommandMatcher(context);
            if (matcher == null || !matcher.find()) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());

            String type = matcher.group("keywordType");
            String keyword = matcher.group("keywordKey");
            String reply = matcher.group("keywordValue");
            if (StringUtils.isNullOrEmptyEx(type)) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            if (StringUtils.isNullOrEmpty(keyword)) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            if (StringUtils.isNullOrEmpty(reply)) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());

            EnumKeywordType keywordType = EnumKeywordType.valueFrom(type);

            String action = StringUtils.orDefault(matcher.group("keywordAction"), baseIns.add().getFirst());
            String group = StringUtils.orDefault(matcher.group("keywordTarget"), baseIns.that().getFirst());
            if (group.startsWith("<") && group.endsWith(">")) group = group.substring(1, group.length() - 1);

            if (baseIns.that().contains(group)) group = String.valueOf(event.getGroupId());
            if (baseIns.global().contains(group)) group = "0";

            LoginInfoResp loginInfoEx = bot.getLoginInfoEx();

            List<Map<String, Object>> forwardMsg = new ArrayList<>();
            forwardMsg.add(ShiroUtils.generateSingleMsg(event.getUserId(), event.getSender().getNickname(), event.getMessage()));
            // 添加
            if (baseIns.add().contains(action)) {
                KeywordRecord keywordRecord = new KeywordRecord()
                        .setBotId(bot.getSelfId())
                        .setGroupId(StringUtils.toLong(group))
                        .setCreatorId(event.getUserId())
                        .setTime(event.getTime())
                        .setKeywordType(keywordType)
                        .setKeyword(keyword)
                        .setReplyMsg(reply)
                        .setAudited(BaniraUtils.isGlobalOp(event.getUserId()));

                String reason = "";
                // 权限及规则判断
                {
                    boolean owner = BaniraUtils.isOwner(event.getUserId());
                    boolean butler = BaniraUtils.isButler(event.getUserId());
                    boolean maid = BaniraUtils.isMaid(event.getGroupId(), event.getUserId());
                    boolean inGroup = bot.isInGroup(keywordRecord.getGroupId(), event.getUserId());
                    boolean groupOwner = inGroup && bot.isGroupOwner(event.getGroupId(), event.getUserId());
                    boolean groupAdmin = inGroup && bot.isGroupAdmin(event.getGroupId(), event.getUserId());
                    boolean globalOp = owner || butler;
                    boolean groupOp = groupOwner || groupAdmin || maid;
                    boolean op = globalOp || groupOp;
                    boolean targetGroupIdValid = BaniraUtils.isGroupIdValid(keywordRecord.getGroupId());
                    boolean groupIdEquals = keywordRecord.getGroupId().equals(event.getGroupId());
                    if (!targetGroupIdValid && !globalOp) {
                        reason = "添加失败：权限不足(全局关键词)";
                    } else if (!groupIdEquals && !globalOp && !inGroup) {
                        reason = "添加失败：权限不足(不在目标群)";
                    } else if (keywordType == EnumKeywordType.PINYIN) {
                        if (!op) {
                            reason = "添加失败：权限不足(拼音匹配)";
                        } else if (StringUtils.isNullOrEmptyEx(keywordRecord.getKeyword())) {
                            reason = "添加失败：规则为空(拼音匹配)";
                        }
                    } else if (keywordType == EnumKeywordType.REGEX) {
                        if (!op) {
                            reason = "添加失败：权限不足(正则匹配)";
                        } else if (RegexpHelper.isRegexTooBroad(keywordRecord.getKeyword())) {
                            reason = "添加失败：触发规则过于宽泛(正则匹配)";
                        }
                    } else if (keywordType == EnumKeywordType.CONTAIN) {
                        if (StringUtils.isNullOrEmptyEx(keywordRecord.getKeyword())) {
                            reason = "添加失败：触发规则为空(拼音匹配)";
                        }
                    }
                }

                if (StringUtils.isNullOrEmptyEx(reason)) {
                    try {
                        if (PlantCipher.isPlantToken(keywordRecord.getReplyMsg())) {
                            keywordRecord.setReplyMsg(PlantCipher.decode(keywordRecord.getReplyMsg()));
                        }
                        keywordRecord.setReplyMsg(BaniraUtils.replaceBaniraFileCode(keywordRecord.getReplyMsg()));
                        keywordRecordManager.addKeywordRecord(keywordRecord);
                        if (keywordRecord.getId() == 0) {
                            reason = "添加失败";
                        }
                    } catch (Exception e) {
                        reason = "添加失败：" + e.getMessage();
                    }
                }

                forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                        , (keywordRecord.getId() != null && keywordRecord.getId() != 0 ? "关键词编号：" + keywordRecord.getId() + "\n" : "") +
                                "关键词类型：" + keywordType.getDesc() + "\n" +
                                "群号：" + keywordRecord.getGroupId() + "\n" +
                                "审核状态：" + (keywordRecord.getAudited() ? "已审核" : "待审核")
                ));
                forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                        , "触发内容：\n" + keywordRecord.getKeyword()
                ));
                forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                        , "回复内容：\n" + keywordRecord.getReplyMsg()
                ));
                if (StringUtils.isNotNullOrEmpty(reason)) {
                    forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                            , reason
                    ));
                }
            }
            // 删除
            else if (baseIns.del().contains(action)) {
                KeywordRecordQueryParam param = new KeywordRecordQueryParam();
                param.setBotId(bot.getSelfId());
                param.setGroupId(StringUtils.toLong(group));
                param.setKeywordType(keywordType.name());
                param.setKeyword(keyword);
                param.setReplyMsg(reply);
                param.setEnable(true);
                List<KeywordRecord> recordList = keywordRecordManager.getKeywordRecordList(param);
                if (CollectionUtils.isNotNullOrEmpty(recordList)) {
                    for (KeywordRecord record : recordList) {
                        this.deleteKeywordRecord(bot, event, record, loginInfoEx, forwardMsg);
                    }
                } else {
                    forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                            , "未查询到关键词"
                    ));
                }
            }
            // 其他
            else {
                return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            }

            ActionData<MsgId> msgIdData = bot.sendForwardMsg(event, forwardMsg);
            return bot.isActionDataMsgIdNotEmpty(msgIdData);
        }
        //
        else {
            // 回复添加成功记录删除 或 审核
            if (BaniraUtils.hasReply(event.getArrayMsg())) {
                if (super.isCommand(context)
                        && keyIns.locator() != null
                        && keyIns.locator().stream().anyMatch(ins -> super.deleteCommandPrefix(context).startsWith(ins.getKey() + " "))
                ) {
                    String[] split = super.deleteCommandPrefix(context).split("\\s+");
                    String operate = split[1];
                    if (split.length != 2 && (!baseIns.del().contains(operate)
                            || !baseIns.add().contains(operate)
                            || !baseIns.enable().contains(operate))
                    ) {
                        return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
                    }
                    if (BaniraUtils.getReplyUserId(bot, event.getGroupId(), event.getArrayMsg()) != bot.getSelfId()) {
                        return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
                    }
                    Long replyId = BaniraUtils.getReplyId(event.getArrayMsg());
                    if (replyId == null) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
                    ActionData<MsgResp> replyMsgData = bot.getMsg(replyId.intValue());
                    if (!bot.isActionDataNotEmpty(replyMsgData) || !BaniraUtils.hasForward(replyMsgData.getData().getArrayMsg())) {
                        return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
                    } else {
                        ActionData<GetForwardMsgResp> forwardMsgResp = bot.getForwardMsg(event.getGroupId(), replyMsgData.getData().getUserId(), replyId.intValue());
                        if (!bot.isActionDataNotEmpty(forwardMsgResp)) {
                            return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
                        } else {
                            LoginInfoResp loginInfoEx = bot.getLoginInfoEx();
                            List<Map<String, Object>> forwardMsg = new ArrayList<>();
                            forwardMsg.add(ShiroUtils.generateSingleMsg(event.getUserId(), event.getSender().getNickname(), event.getMessage()));

                            List<Long> ids = forwardMsgResp.getData().getMessages().stream()
                                    .filter(data -> StringUtils.toLong(data.getSender().getUserId()) == bot.getSelfId())
                                    .map(MessageEvent::getMessage)
                                    .filter(StringUtils::isNotNullOrEmpty)
                                    .filter(data -> data.startsWith("关键词编号："))
                                    .map(data -> CollectionUtils.getOrDefault(data.split("关键词编号："), 1, "").strip())
                                    .map(data -> CollectionUtils.getFirst(data.split("\\s")))
                                    .map(StringUtils::toLong)
                                    .filter(data -> data > 0).toList();
                            for (Long id : ids) {
                                KeywordRecord record = keywordRecordManager.getKeywordRecord(id);
                                if (baseIns.del().contains(operate)) {
                                    this.deleteKeywordRecord(bot, event, record, loginInfoEx, forwardMsg);
                                } else {
                                    this.enableKeywordRecord(bot, event, record, loginInfoEx, forwardMsg);
                                }
                            }
                            ActionData<MsgId> msgIdData = bot.sendForwardMsg(event, forwardMsg);
                            return bot.isActionDataMsgIdNotEmpty(msgIdData);
                        }
                    }

                } else return false;
            }
            //
            else if (super.isCommand(context)
                    && keyIns.locator() != null
                    && keyIns.locator().stream().anyMatch(ins -> super.deleteCommandPrefix(context).startsWith(ins.getKey() + " "))
            ) {
                String[] split = super.deleteCommandPrefix(context).split("\\s+");
                if (split.length < 2) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());

                // 根据ID删除
                String operate = split[1];
                if (baseIns.del().contains(operate) || baseIns.add().contains(operate) || baseIns.enable().contains(operate)) {
                    LoginInfoResp loginInfoEx = bot.getLoginInfoEx();
                    List<Map<String, Object>> forwardMsg = new ArrayList<>();
                    forwardMsg.add(ShiroUtils.generateSingleMsg(event.getUserId(), event.getSender().getNickname(), event.getMessage()));

                    Long[] ids = Arrays.stream(split).skip(2)
                            .map(StringUtils::toLong)
                            .filter(data -> data > 0)
                            .distinct()
                            .toArray(Long[]::new);
                    if (ids.length == 0) {
                        return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
                    } else {
                        List<KeywordRecord> recordList = keywordRecordManager.getKeywordRecordList(new KeywordRecordQueryParam().setId(ids).setEnable(true));
                        if (!recordList.isEmpty()) {
                            for (KeywordRecord record : recordList) {
                                if (baseIns.del().contains(operate)) {
                                    this.deleteKeywordRecord(bot, event, record, loginInfoEx, forwardMsg);
                                } else {
                                    this.enableKeywordRecord(bot, event, record, loginInfoEx, forwardMsg);
                                }
                            }
                        } else {
                            forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                                    , "未查询到关键词"
                            ));
                        }
                    }
                    ActionData<MsgId> msgIdData = bot.sendForwardMsg(event, forwardMsg);
                    return bot.isActionDataMsgIdNotEmpty(msgIdData);
                }
                // 查询
                else if (baseIns.list().contains(operate)) {
                    long page = StringUtils.toLong(CollectionUtils.getOrDefault(split, 2, ""), 0);
                    String keyWord = String.join("", Arrays.copyOfRange(split, page > 0 && split.length > 3 ? 3 : 2, split.length));
                    if (page <= 0) page = 1;
                    PageResult<KeywordRecord> pageList = keywordRecordManager.getKeywordRecordPagedList(
                            new KeywordRecordQueryParam(true, page, 98)
                                    .setBotId(bot.getSelfId())
                                    .setGroupId(0L, event.getGroupId())
                                    .addKeyWord(String.format("%%%s%%", keyWord))
                                    .setEnable(true)
                                    .addOrderBy(KeywordRecordQueryParam.ORDER_ID, true)
                    );
                    LoginInfoResp loginInfoEx = bot.getLoginInfoEx();
                    List<Map<String, Object>> forwardMsg = new ArrayList<>();
                    forwardMsg.add(ShiroUtils.generateSingleMsg(event.getUserId(), event.getSender().getNickname(), event.getMessage()));
                    if (pageList.isEmpty()) {
                        forwardMsg.add(ShiroUtils.generateSingleMsg(loginInfoEx.getUserId(), loginInfoEx.getNickname()
                                , "未查询到关键词"
                        ));
                    } else {
                        forwardMsg.add(ShiroUtils.generateSingleMsg(loginInfoEx.getUserId(), loginInfoEx.getNickname()
                                , "关键词总数：" + pageList.getTotal() + "\n" +
                                        "当前页：" + pageList.getPage() + "\n" +
                                        "总页数：" + pageList.getTotalPages() + "\n" +
                                        "每页数量：" + pageList.getSize() + "\n" +
                                        "当前页数量：" + pageList.getRecords().size()
                        ));
                        for (KeywordRecord record : pageList.getRecords()) {
                            forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                                    , "关键词编号：" + record.getId() + "\n" +
                                            "关键词类型：" + record.getKeywordType().getDesc() + "\n" +
                                            "群号：" + record.getGroupId() + "\n" +
                                            "审核状态：" + (record.getAudited() ? "已审核" : "待审核") + "\n\n" +
                                            "触发内容：\n" + record.getKeyword() + "\n\n" +
                                            "回复内容：\n" + record.getReplyMsg()
                            ));
                        }
                    }
                    ActionData<MsgId> msgIdData = bot.sendForwardMsg(event, forwardMsg);
                    return bot.isActionDataMsgIdNotEmpty(msgIdData);
                } else return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            }
        }
        return false;
    }

    @AnyMessageHandler
    public boolean reply(BaniraBot bot, AnyMessageEvent event) {
        AtomicReference<String> message = new AtomicReference<>(event.getMessage());
        eventCoders.forEach(coder -> message.set(coder.escape(message.get())));
        BaniraCodeContext context = this.searchReply(
                new BaniraCodeContext(bot
                        , event.getArrayMsg()
                        , event.getGroupId()
                        , event.getUserId()
                        , event.getUserId()
                )
                        .msg(message.get())
                        .msgId(event.getMessageId())
                        .time(event.getTime())
                        .msgType(EnumMessageType.getType(event))
        );
        if (context != null) {
            ActionData<MsgId> msgId;
            message.set(context.msg());
            eventCoders.forEach(coder -> message.set(coder.unescape(message.get())));
            if (ActionParams.PRIVATE.equals(event.getMessageType())) {
                msgId = bot.sendPrivateMsg(context.sender(), context.msg(), false);
            } else if (ActionParams.GROUP.equals(event.getMessageType())) {
                msgId = bot.sendGroupMsg(context.group(), context.msg(), false);
            } else msgId = null;
            return bot.isActionDataMsgIdNotEmpty(msgId);
        }
        return false;
    }

    @GroupPokeNoticeHandler
    @PrivatePokeNoticeHandler
    public void reply(BaniraBot bot, PokeNoticeEvent event) {
        JsonObject data = new JsonObject();
        JsonUtils.setLong(data, "targetId", event.getTargetId());
        JsonUtils.setLong(data, "senderId", event.getUserId());
        String eventMsg = getEventCoder(PokeCode.class).build(data);

        BaniraCodeContext context = this.searchReply(
                new BaniraCodeContext(bot
                        , new ArrayList<>()
                        , event.getGroupId()
                        , event.getUserId()
                        , event.getTargetId()
                )
                        .operator(event.getUserId())
                        .msg(eventMsg)
                        .time(event.getTime())
        );
        if (context != null) {
            if (BaniraUtils.isGroupIdValid(event.getGroupId())) {
                bot.sendGroupMsg(context.group(), context.msg(), false);
            } else {
                bot.sendPrivateMsg(context.sender(), context.msg(), false);
            }
        }
    }

    @GroupIncreaseHandler
    public void reply(BaniraBot bot, GroupIncreaseNoticeEvent event) {
        JsonObject data = new JsonObject();
        JsonUtils.setLong(data, "groupId", event.getGroupId());
        JsonUtils.setLong(data, "userId", event.getUserId());
        JsonUtils.setLong(data, "operatorId", event.getOperatorId());
        String eventMsg = getEventCoder(InGroupCode.class).build(data);

        BaniraCodeContext context = this.searchReply(
                new BaniraCodeContext(bot
                        , new ArrayList<>()
                        , event.getGroupId()
                        , event.getUserId()
                        , event.getUserId()
                )
                        .operator(event.getOperatorId())
                        .msg(eventMsg)
                        .time(event.getTime())
        );
        if (context != null) {
            if (BaniraUtils.isGroupIdValid(event.getGroupId())) {
                bot.sendGroupMsg(context.group(), context.msg(), false);
            } else {
                bot.sendPrivateMsg(context.sender(), context.msg(), false);
            }
        }
    }


    private void deleteKeywordRecord(BaniraBot bot, AnyMessageEvent event
            , KeywordRecord record, LoginInfoResp loginInfoEx, List<Map<String, Object>> forwardMsg
    ) {
        Long groupId = BaniraUtils.isGroupIdValid(record.getGroupId())
                ? record.getGroupId()
                : BaniraUtils.isGroupIdValid(event.getGroupId()) ? event.getGroupId() : null;
        boolean hasOp = Objects.equals(event.getUserId(), record.getCreatorId())
                || bot.isUpper(groupId, event.getUserId(), record.getCreatorId());
        boolean enable = record.getEnable();
        String reason = "";
        if (!hasOp) {
            reason = "\n删除失败：权限不足";
        } else if (!enable) {
            reason = "\n删除失败：关键词未启用";
        }
        if (StringUtils.isNullOrEmptyEx(reason)) {
            try {
                if (keywordRecordManager.deleteKeywordRecord(record.getId()) > 0) {
                    reason = "\n删除成功";
                } else {
                    reason = "\n删除失败";
                }
            } catch (Exception e) {
                reason = "\n删除失败：" + e.getMessage();
            }
        }
        forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                , "关键词编号：" + record.getId() + "\n" +
                        "关键词类型：" + record.getKeywordType().getDesc() + "\n" +
                        "群号：" + record.getGroupId() + reason + "\n" +
                        "审核状态：" + (record.getAudited() ? "已审核" : "待审核")
        ));
        forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                , "触发内容：\n" + record.getKeyword()
        ));
        forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                , "回复内容：\n" + record.getReplyMsg()
        ));
    }

    private void enableKeywordRecord(BaniraBot bot, AnyMessageEvent event
            , KeywordRecord record, LoginInfoResp loginInfoEx, List<Map<String, Object>> forwardMsg
    ) {
        String reason = "";
        if (!BaniraUtils.isGlobalOp(event.getUserId())) {
            reason = "\n启用失败：权限不足";
        }
        if (StringUtils.isNullOrEmptyEx(reason)) {
            try {
                keywordRecordManager.modifyKeywordRecord(record.setAudited(true).setEnable(true));
                reason = "\n启用成功";
            } catch (Exception e) {
                reason = "\n启用失败：" + e.getMessage();
            }
        }
        forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                , "关键词编号：" + record.getId() + "\n" +
                        "关键词类型：" + record.getKeywordType().getDesc() + "\n" +
                        "群号：" + record.getGroupId() + reason
        ));
        forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                , "触发内容：\n" + record.getKeyword()
        ));
        forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                , "回复内容：\n" + record.getReplyMsg()
        ));
    }

    private BaniraCodeContext searchReply(BaniraCodeContext context) {
        BaniraCodeContext result = null;
        KeywordRecord matchReply = keywordManager.findMatchReply(context.msg(), context.bot().getSelfId(), context.group());
        if (matchReply != null) {
            context.msg(matchReply.getReplyMsg())
                    .opId(matchReply.getCreatorId())
                    .keywordRecord(matchReply);
            result = codeHandler.decode(context);
        }
        return result;
    }

    private EventCoder getEventCoder(Class<? extends EventCoder> clazz) {
        return SpringContextHolder.getBean(clazz);
    }
}
