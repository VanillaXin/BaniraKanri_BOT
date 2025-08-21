package xin.vanilla.banira.plugin;

import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.common.utils.ShiroUtils;
import com.mikuac.shiro.constant.ActionParams;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.action.common.ActionData;
import com.mikuac.shiro.dto.action.common.MsgId;
import com.mikuac.shiro.dto.action.response.GetForwardMsgResp;
import com.mikuac.shiro.dto.action.response.LoginInfoResp;
import com.mikuac.shiro.dto.action.response.MsgResp;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import com.mikuac.shiro.dto.event.message.MessageEvent;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.coder.BaniraCodeHandler;
import xin.vanilla.banira.config.entity.basic.BaseInstructionsConfig;
import xin.vanilla.banira.config.entity.basic.KeyInstructionsConfig;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.KeyValue;
import xin.vanilla.banira.domain.KeywordRecord;
import xin.vanilla.banira.domain.PageResult;
import xin.vanilla.banira.enums.EnumKeywordType;
import xin.vanilla.banira.mapper.param.KeywordRecordQueryParam;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.service.IKeywordManager;
import xin.vanilla.banira.service.IKeywordRecordManager;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.*;
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
    @Resource
    private BaniraCodeHandler codeHandler;

    private static final Set<String> helpType = BaniraUtils.mutableSetOf(
            "keyword", "keyWord", "关键词", "关键词回复"
    );

    /**
     * 获取帮助信息
     *
     * @param type    帮助类型
     * @param groupId 群组ID
     */
    @Nonnull
    @Override
    public List<String> getHelpInfo(@Nonnull String type, @Nullable Long groupId) {
        List<String> result = new ArrayList<>();
        if (helpType.stream().anyMatch(s -> StringUtils.isNullOrEmptyEx(type) || s.equalsIgnoreCase(type))) {
            BaseInstructionsConfig baseIns = BaniraUtils.getBaseIns();
            KeyInstructionsConfig keyIns = BaniraUtils.getKeyIns();

            Set<String> keywordTargets = BaniraUtils.mutableSetOf("<\\d{5,10}>");
            keywordTargets.addAll(baseIns.that());
            keywordTargets.addAll(baseIns.global());

            Set<String> keywordTypes = BaniraUtils.mutableSetOf();
            keywordTypes.addAll(keyIns.exactly());
            keywordTypes.addAll(keyIns.contain());
            keywordTypes.addAll(keyIns.pinyin());
            keywordTypes.addAll(keyIns.regex());

            result.add("关键词回复 - 增加：\n" +
                    BaniraUtils.getInsPrefixWithSpace() +
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
                    "用法1：\n" +
                    BaniraUtils.getInsPrefixWithSpace() +
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
                    BaniraUtils.getInsPrefixWithSpace() +
                    keyIns.locator().stream().map(KeyValue::getKey).toList() + " " +
                    baseIns.del() + " " +
                    "<关键词编号> ..." + "\n\n" +
                    "用法3：(回复添加成功的响应消息)\n" +
                    BaniraUtils.getInsPrefixWithSpace() +
                    keyIns.locator().stream().map(KeyValue::getKey).toList() + " " +
                    baseIns.del()
            );
            result.add("关键词回复 - 查询：\n" +
                    BaniraUtils.getInsPrefixWithSpace() +
                    keyIns.locator().stream().map(KeyValue::getKey).toList() + " " +
                    baseIns.list() + " " +
                    "[<页数>]" + " " +
                    "<关键词内容>"
            );
        }
        return result;
    }

    @AnyMessageHandler
    public boolean config(Bot tob, AnyMessageEvent event) {
        BaniraBot bot = new BaniraBot(tob);
        String message = event.getMessage();
        KeyInstructionsConfig keyIns = BaniraUtils.getKeyIns();

        // 精准添加/删除
        if (super.isKeywordCommand(message)) {
            Matcher matcher = super.getKeywordCommandMatcher(message);
            if (matcher == null || !matcher.find()) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());

            String type = matcher.group("keywordType");
            String keyword = matcher.group("keywordKey");
            String reply = matcher.group("keywordValue");
            if (StringUtils.isNullOrEmptyEx(type)) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            if (StringUtils.isNullOrEmpty(keyword)) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            if (StringUtils.isNullOrEmpty(reply)) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());

            EnumKeywordType keywordType = EnumKeywordType.valueFrom(type);

            BaseInstructionsConfig baseIns = BaniraUtils.getBaseIns();

            String action = StringUtils.orDefault(matcher.group("keywordAction"), baseIns.add().iterator().next());
            String group = StringUtils.orDefault(matcher.group("keywordTarget"), baseIns.that().iterator().next());

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
                        .setReplyMsg(reply);
                keywordRecordManager.addKeywordRecord(keywordRecord);
                forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                        , (keywordRecord.getId() != 0 ? "关键词编号：" + keywordRecord.getId() + "\n" : "") +
                                "关键词类型：" + keywordType.getDesc() + "\n" +
                                "群号：" + keywordRecord.getGroupId()
                ));
                forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                        , "触发内容：\n" + keywordRecord.getKeyword()
                ));
                forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                        , "回复内容：\n" + keywordRecord.getReplyMsg()
                ));
                if (keywordRecord.getId() == 0) {
                    forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                            , "添加失败"
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
                        boolean b = keywordRecordManager.deleteKeywordRecord(record.getId()) > 0;
                        this.buildForwardMsg(bot, loginInfoEx, forwardMsg, record, b);
                    }
                } else {
                    forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                            , "未找到目标关键词"
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
            // 回复添加成功记录删除
            if (BaniraUtils.hasReply(event.getArrayMsg())) {
                String msg = BaniraUtils.replaceReply(message);
                if (super.isCommand(msg)
                        && keyIns.locator() != null
                        && keyIns.locator().stream().anyMatch(ins -> super.replaceCommand(msg).startsWith(ins.getKey()))
                ) {
                    String[] split = super.replaceCommand(msg).split("\\s+");
                    if (split.length != 2 && !BaniraUtils.getBaseIns().del().contains(split[1])) {
                        return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
                    }
                    if (BaniraUtils.getReplyQQ(bot, event.getGroupId(), event.getArrayMsg()) != bot.getSelfId()) {
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

                            long id = forwardMsgResp.getData().getMessages().stream()
                                    .filter(data -> StringUtils.toLong(data.getSender().getUserId()) == bot.getSelfId())
                                    .map(MessageEvent::getMessage)
                                    .filter(StringUtils::isNotNullOrEmpty)
                                    .filter(data -> data.startsWith("关键词编号："))
                                    .map(data -> CollectionUtils.getOrDefault(data.split("关键词编号："), 1, "").strip())
                                    .map(data -> CollectionUtils.getFirst(data.split("\\s")))
                                    .map(StringUtils::toLong)
                                    .filter(data -> data > 0).findFirst().orElse(0L);
                            KeywordRecord record = keywordRecordManager.getKeywordRecord(id);
                            boolean b = keywordRecordManager.deleteKeywordRecord(id) > 0;
                            this.buildForwardMsg(bot, loginInfoEx, forwardMsg, record, b);
                            ActionData<MsgId> msgIdData = bot.sendForwardMsg(event, forwardMsg);
                            return bot.isActionDataMsgIdNotEmpty(msgIdData);
                        }
                    }

                } else return false;
            }
            // 根据ID删除
            else if (super.isCommand(message)
                    && keyIns.locator() != null
                    && keyIns.locator().stream().anyMatch(ins -> super.replaceCommand(message).startsWith(ins.getKey()))
            ) {
                String[] split = super.replaceCommand(message).split("\\s+");
                if (split.length <= 2) {
                    return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
                }
                BaseInstructionsConfig baseIns = BaniraUtils.getBaseIns();

                // 删除
                if (baseIns.del().contains(split[1])) {
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
                                boolean b = keywordRecordManager.deleteKeywordRecord(record.getId()) > 0;
                                this.buildForwardMsg(bot, loginInfoEx, forwardMsg, record, b);
                            }
                        } else {
                            forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                                    , "未找到目标关键词"
                            ));
                        }
                    }
                    ActionData<MsgId> msgIdData = bot.sendForwardMsg(event, forwardMsg);
                    return bot.isActionDataMsgIdNotEmpty(msgIdData);
                }
                // 查询
                else if (baseIns.list().contains(split[1])) {
                    long page = StringUtils.toLong(split[2], 0);
                    String keyword = String.join("", Arrays.copyOfRange(split, page > 0 && split.length > 3 ? 3 : 2, split.length));
                    if (page <= 0) page = 1;
                    PageResult<KeywordRecord> pageList = keywordRecordManager.getKeywordRecordPagedList(
                            new KeywordRecordQueryParam(true, page, 98)
                                    .setBotId(bot.getSelfId())
                                    .setGroupId(0L, event.getGroupId())
                                    .setKeyword(keyword)
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
                                            "群号：" + record.getGroupId() + "\n\n" +
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
    public boolean reply(Bot tob, AnyMessageEvent event) {
        BaniraBot bot = new BaniraBot(tob);

        BaniraCodeContext context = this.searchReply(
                new BaniraCodeContext(bot, event.getArrayMsg())
                        .setGroup(event.getGroupId())
                        .setSender(event.getUserId())
                        .setTarget(event.getUserId())
                        .setMsg(event.getMessage())
                        .setTime(event.getTime())
        );
        if (context != null) {
            ActionData<MsgId> msgId;
            if (ActionParams.PRIVATE.equals(event.getMessageType())) {
                msgId = bot.sendPrivateMsg(context.getTarget(), context.getMsg(), false);
            } else if (ActionParams.GROUP.equals(event.getMessageType())) {
                msgId = bot.sendGroupMsg(context.getGroup(), context.getMsg(), false);
            } else msgId = null;
            return bot.isActionDataMsgIdNotEmpty(msgId);
        }
        return false;
    }


    private void buildForwardMsg(BaniraBot bot
            , LoginInfoResp loginInfoEx
            , List<Map<String, Object>> forwardMsg
            , KeywordRecord record
            , boolean success
    ) {
        forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                , "关键词编号：" + record.getId() + "\n" +
                        "关键词类型：" + record.getKeywordType().getDesc() + "\n" +
                        "群号：" + record.getGroupId() + "\n" +
                        "删除" + (success ? "成功" : "失败") + (record.getEnable() ? "" : "\n关键词未启用")
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
        KeywordRecord matchReply = keywordManager.findMatchReply(context.getMsg(), context.getBot().getSelfId(), context.getGroup());
        if (matchReply != null) {
            context.setMsg(matchReply.getReplyMsg())
                    .setOpId(matchReply.getCreatorId());
            result = codeHandler.decode(context);
        }
        return result;
    }
}
