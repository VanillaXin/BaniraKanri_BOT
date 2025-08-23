package xin.vanilla.banira.plugin;

import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.common.utils.ShiroUtils;
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
import xin.vanilla.banira.config.entity.basic.BaseInstructionsConfig;
import xin.vanilla.banira.config.entity.basic.TimerInstructionsConfig;
import xin.vanilla.banira.domain.PageResult;
import xin.vanilla.banira.domain.TimerRecord;
import xin.vanilla.banira.mapper.param.TimerRecordQueryParam;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.service.ITimerRecordManager;
import xin.vanilla.banira.util.*;

import java.util.*;
import java.util.regex.Matcher;
import java.util.stream.Collectors;

/**
 * 定时任务
 */
@Slf4j
@Shiro
@Component
public class TimerPlugin extends BasePlugin {

    @Resource
    private ITimerRecordManager timerRecordManager;

    private static final Set<String> helpType = BaniraUtils.mutableSetOf(
            "timer", "time", "cron", "定时任务", "定时"
    );

    private static final Set<String> exampleOperate = BaniraUtils.mutableSetOf(
            "example", "case", "例子", "实例", "栗子"
    );

    @Nonnull
    @Override
    public List<String> getHelpInfo(@Nullable Long groupId, @Nonnull String... types) {
        return List.of();
    }


    @AnyMessageHandler
    public boolean config(BaniraBot bot, AnyMessageEvent event) {
        String message = event.getMessage();
        TimerInstructionsConfig timerIns = BaniraUtils.getTimerIns();
        BaseInstructionsConfig baseIns = BaniraUtils.getBaseIns();

        // 精准添加/删除
        if (super.isTimerCommand(message)) {
            Matcher matcher = super.getTimerCommandMatcher(message);
            if (matcher == null || !matcher.find()) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());

            String cron = matcher.group("timerKey");
            String reply = matcher.group("timerValue");
            if (StringUtils.isNullOrEmpty(cron)) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            if (StringUtils.isNullOrEmpty(reply)) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());

            String action = StringUtils.orDefault(matcher.group("timerAction"), baseIns.add().getFirst());
            String group = StringUtils.orDefault(matcher.group("timerTarget"), baseIns.that().getFirst());

            if (baseIns.that().contains(group)) group = String.valueOf(event.getGroupId());
            if (baseIns.global().contains(group)) group = "0";

            LoginInfoResp loginInfoEx = bot.getLoginInfoEx();

            List<Map<String, Object>> forwardMsg = new ArrayList<>();
            forwardMsg.add(ShiroUtils.generateSingleMsg(event.getUserId(), event.getSender().getNickname(), event.getMessage()));
            // 添加
            if (baseIns.add().contains(action)) {
                TimerRecord timerRecord = new TimerRecord()
                        .setBotId(bot.getSelfId())
                        .setGroupId(StringUtils.toLong(group))
                        .setCreatorId(event.getUserId())
                        .setTime(event.getTime())
                        .setCron(cron)
                        .setReplyMsg(reply);

                String reason = "";
                // 权限及规则判断
                {
                    boolean owner = BaniraUtils.isOwner(event.getUserId());
                    boolean butler = BaniraUtils.isButler(event.getUserId());
                    boolean maid = BaniraUtils.isMaid(event.getGroupId(), event.getUserId());
                    boolean inGroup = bot.isInGroup(timerRecord.getGroupId(), event.getUserId());
                    boolean groupOwner = inGroup && bot.isGroupOwner(event.getGroupId(), event.getUserId());
                    boolean groupAdmin = inGroup && bot.isGroupAdmin(event.getGroupId(), event.getUserId());
                    boolean globalOp = owner || butler;
                    boolean groupOp = groupOwner || groupAdmin || maid;
                    boolean op = globalOp || groupOp;
                    boolean groupIdEquals = timerRecord.getGroupId().equals(event.getGroupId());
                    boolean groupIdValid = BaniraUtils.isGroupIdValid(timerRecord.getGroupId());
                    if (groupIdValid && !op) {
                        reason = "添加失败：权限不足";
                    } else if (groupIdValid && !groupIdEquals && !globalOp && !inGroup) {
                        reason = "添加失败：权限不足(不在目标群)";
                    } else if (!CronUtils.isValidCron(cron)) {
                        reason = "添加失败：cron表达式不合法";
                    } else if (CronUtils.hasTooShortInterval(cron, 30, 20)) {
                        reason = "添加失败：表达式执行间隔过短";
                    }
                }

                if (StringUtils.isNullOrEmptyEx(reason)) {
                    try {
                        timerRecordManager.addTimerRecord(timerRecord);
                        if (timerRecord.getId() == 0) {
                            reason = "添加失败";
                        }
                    } catch (Exception e) {
                        reason = "添加失败：" + e.getMessage();
                    }
                }

                forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                        , (timerRecord.getId() != null && timerRecord.getId() != 0 ? "定时任务编号：" + timerRecord.getId() + "\n" : "") +
                                "群号：" + timerRecord.getGroupId() + "\n" +
                                "表达式：" + timerRecord.getCron()
                ));
                String dates = CronUtils.getNextFireTimes(timerRecord.getCron(), 6).stream()
                        .map(DateUtils::toDateTimeString)
                        .collect(Collectors.joining("\n"));
                forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                        , "未来执行时间：\n" +
                                (StringUtils.isNotNullOrEmpty(dates) ? StringUtils.replaceLine(dates, 6, "...") : "无")
                ));
                forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                        , "定时任务内容：\n" + timerRecord.getReplyMsg()
                ));
                if (StringUtils.isNotNullOrEmpty(reason)) {
                    forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                            , reason
                    ));
                }
            }
            // 删除
            else if (baseIns.del().contains(action)) {
                TimerRecordQueryParam param = new TimerRecordQueryParam();
                param.setBotId(bot.getSelfId());
                param.setGroupId(StringUtils.toLong(group));
                param.setReplyMsg(reply);
                param.setEnable(true);
                List<TimerRecord> recordList = timerRecordManager.getTimerRecordList(param);
                if (CollectionUtils.isNotNullOrEmpty(recordList)) {
                    for (TimerRecord record : recordList) {
                        this.deleteTimerRecord(bot, event, record, loginInfoEx, forwardMsg);
                    }
                } else {
                    forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                            , "未查询到定时任务"
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
                        && timerIns.locator() != null
                        && timerIns.locator().stream().anyMatch(ins -> super.replaceCommand(msg).startsWith(ins.getKey()))
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

                            List<Long> ids = forwardMsgResp.getData().getMessages().stream()
                                    .filter(data -> StringUtils.toLong(data.getSender().getUserId()) == bot.getSelfId())
                                    .map(MessageEvent::getMessage)
                                    .filter(StringUtils::isNotNullOrEmpty)
                                    .filter(data -> data.startsWith("定时任务编号："))
                                    .map(data -> CollectionUtils.getOrDefault(data.split("定时任务编号："), 1, "").strip())
                                    .map(data -> CollectionUtils.getFirst(data.split("\\s")))
                                    .map(StringUtils::toLong)
                                    .filter(data -> data > 0).toList();
                            for (Long id : ids) {
                                TimerRecord record = timerRecordManager.getTimerRecord(id);
                                this.deleteTimerRecord(bot, event, record, loginInfoEx, forwardMsg);
                            }
                            ActionData<MsgId> msgIdData = bot.sendForwardMsg(event, forwardMsg);
                            return bot.isActionDataMsgIdNotEmpty(msgIdData);
                        }
                    }

                } else return false;
            }
            //
            else if (super.isCommand(message)
                    && timerIns.locator() != null
                    && timerIns.locator().stream().anyMatch(ins -> super.replaceCommand(message).startsWith(ins.getKey()))
            ) {
                String[] split = super.replaceCommand(message).split("\\s+");
                if (split.length < 2) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());

                // 根据ID删除
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
                        List<TimerRecord> recordList = timerRecordManager.getTimerRecordList(new TimerRecordQueryParam().setId(ids).setEnable(true));
                        if (!recordList.isEmpty()) {
                            for (TimerRecord record : recordList) {
                                this.deleteTimerRecord(bot, event, record, loginInfoEx, forwardMsg);
                            }
                        } else {
                            forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                                    , "未查询到定时任务"
                            ));
                        }
                    }
                    ActionData<MsgId> msgIdData = bot.sendForwardMsg(event, forwardMsg);
                    return bot.isActionDataMsgIdNotEmpty(msgIdData);
                }
                // 查询
                else if (baseIns.list().contains(split[1])) {
                    long page = StringUtils.toLong(CollectionUtils.getOrDefault(split, 2, ""), 0);
                    String keyWord = String.join("", Arrays.copyOfRange(split, page > 0 && split.length > 3 ? 3 : 2, split.length));
                    if (page <= 0) page = 1;
                    PageResult<TimerRecord> pageList = timerRecordManager.getTimerRecordPagedList(
                            new TimerRecordQueryParam(true, page, 98)
                                    .setBotId(bot.getSelfId())
                                    .setGroupId(0L, event.getGroupId())
                                    .setEnable(true)
                                    .addKeyWord(String.format("%%%s%%", keyWord))
                                    .addOrderBy(TimerRecordQueryParam.ORDER_ID, true)
                    );
                    LoginInfoResp loginInfoEx = bot.getLoginInfoEx();
                    List<Map<String, Object>> forwardMsg = new ArrayList<>();
                    forwardMsg.add(ShiroUtils.generateSingleMsg(event.getUserId(), event.getSender().getNickname(), event.getMessage()));
                    if (pageList.isEmpty()) {
                        forwardMsg.add(ShiroUtils.generateSingleMsg(loginInfoEx.getUserId(), loginInfoEx.getNickname()
                                , "未查询到定时任务"
                        ));
                    } else {
                        forwardMsg.add(ShiroUtils.generateSingleMsg(loginInfoEx.getUserId(), loginInfoEx.getNickname()
                                , "定时任务总数：" + pageList.getTotal() + "\n" +
                                        "当前页：" + pageList.getPage() + "\n" +
                                        "总页数：" + pageList.getTotalPages() + "\n" +
                                        "每页数量：" + pageList.getSize() + "\n" +
                                        "当前页数量：" + pageList.getRecords().size()
                        ));
                        for (TimerRecord record : pageList.getRecords()) {
                            forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                                    , "定时任务编号：" + record.getId() + "\n" +
                                            "群号：" + record.getGroupId() + "\n\n" +
                                            "表达式：\n" + record.getCron() + "\n\n" +
                                            "定时任务内容：\n" + record.getReplyMsg()
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

    private void deleteTimerRecord(BaniraBot bot, AnyMessageEvent event
            , TimerRecord record, LoginInfoResp loginInfoEx, List<Map<String, Object>> forwardMsg
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
            reason = "\n删除失败：定时任务未启用";
        }
        if (StringUtils.isNullOrEmptyEx(reason)) {
            try {
                if (timerRecordManager.deleteTimerRecord(record.getId()) > 0) {
                    reason = "\n删除成功";
                } else {
                    reason = "\n删除失败";
                }
            } catch (Exception e) {
                reason = "\n删除失败：" + e.getMessage();
            }
        }
        forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                , "定时任务编号：" + record.getId() + "\n" +
                        "群号：" + record.getGroupId() + "\n" +
                        "表达式：" + record.getCron() + reason
        ));
        // String dates = CronChecker.getNextFireTimes(record.getCron(), 6).stream()
        //         .map(DateUtils::toDateTimeString)
        //         .collect(Collectors.joining("\n"));
        // forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
        //         , "未来执行时间：\n" +
        //                 (StringUtils.isNotNullOrEmpty(dates) ? StringUtils.replaceLine(dates, 6, "...") : "无")
        // ));
        forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                , "定时任务内容：\n" + record.getReplyMsg()
        ));
    }
}
