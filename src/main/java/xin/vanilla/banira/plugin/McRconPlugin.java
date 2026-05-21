package xin.vanilla.banira.plugin;

import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.PrivateMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.common.utils.ShiroUtils;
import com.mikuac.shiro.dto.action.common.ActionData;
import com.mikuac.shiro.dto.action.common.MsgId;
import com.mikuac.shiro.dto.action.response.GetForwardMsgResp;
import com.mikuac.shiro.dto.action.response.LoginInfoResp;
import com.mikuac.shiro.dto.action.response.MsgResp;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import com.mikuac.shiro.dto.event.message.MessageEvent;
import com.mikuac.shiro.dto.event.message.PrivateMessageEvent;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.InstructionsConfig;
import xin.vanilla.banira.config.entity.basic.BaseInstructionsConfig;
import xin.vanilla.banira.config.entity.extended.McConfig;
import xin.vanilla.banira.config.entity.group.McQueryGroupConfig;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.MinecraftRecord;
import xin.vanilla.banira.domain.PageResult;
import xin.vanilla.banira.mapper.param.MinecraftRecordQueryParam;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.plugin.help.HelpTopic;
import xin.vanilla.banira.plugin.help.HelpTopics;
import xin.vanilla.banira.service.IMinecraftRecordManager;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.NetAddressUtils;
import xin.vanilla.banira.util.StringUtils;
import xin.vanilla.rcon.Rcon;

import java.io.IOException;
import java.util.*;

/**
 * MC 服务器 RCON 远程控制
 */
@Slf4j
@Shiro
@Component
public class McRconPlugin extends BasePlugin {

    private static final int DEFAULT_RCON_PORT = 25575;

    @Resource
    private IMinecraftRecordManager minecraftRecordManager;

    @Override
    public void registerHelpTopics(@Nonnull List<HelpTopic> topics, Long groupId) {
        InstructionsConfig ins = insConfig.get();
        BaseInstructionsConfig base = BaniraUtils.getBaseIns();
        String prefix = BaniraUtils.getInsPrefixWithSpace();
        List<String> mcRcon = ins.mcRcon();
        String rconCmd = prefix + mcRcon.getFirst();
        String slashCmd = "/" + mcRcon.getFirst();
        String pswCmd = prefix + mcRcon.getFirst() + " " + ins.mcRconPsw().getFirst();

        HelpTopic topic = HelpTopics.of("MC远程", "通过 RCON 远程执行 Minecraft 服务器命令。", 99, mcRcon);
        topic.child(HelpTopics.opAdd(base,
                "绑定 RCON 地址到已保存或新建的服务器记录。\n\n"
                        + "用法1：\n" + rconCmd + " " + base.add().getFirst() + " [<名称>] <RCON地址> <RCON端口>\n\n"
                        + "用法2：\n" + rconCmd + " " + base.add().getFirst() + " [<名称>] <RCON地址:RCON端口>\n\n"
                        + "未指定名称时自动生成；RCON 密码请私聊设置。"));
        topic.child(HelpTopics.opDel(base,
                "用法1（按编号）：\n" + rconCmd + " " + base.del().getFirst() + " <编号> ...\n\n"
                        + "用法2（回复绑定消息）：\n" + rconCmd + " " + base.del().getFirst() + "\n\n"
                        + "清除对应记录的 RCON 配置。"));
        topic.child(HelpTopics.opList(base,
                "查看已配置 RCON 的服务器。\n\n"
                        + "用法：\n" + rconCmd + " " + base.list().getFirst() + " [<页数>] [<名称>]"));
        topic.child(HelpTopics.sub("执行命令", "向服务器发送 RCON 指令。", 4, mcRcon,
                "用法：\n" + slashCmd + " <编号|名称> <命令...>\n\n"
                        + "示例：\n" + slashCmd + " 1 list\n"
                        + slashCmd + " 我的服务器 say Hello"));
        topic.child(HelpTopics.sub("设置密码", "私聊设置 RCON 密码（不在群内发送）。", 5, ins.mcRconPsw(),
                "用法（仅私聊）：\n" + pswCmd + " <编号> <密码>\n\n"
                        + "需要为服务器记录的添加者或群管理员。"));
        topics.add(topic);
    }

    @AnyMessageHandler
    public boolean execute(BaniraBot bot, AnyMessageEvent event) {
        String message = event.getMessage();
        String matchedSlash = matchSlashRcon(message);
        if (matchedSlash == null) return false;

        String body = stripSlashPrefix(message, matchedSlash);
        if (StringUtils.isNullOrEmptyEx(body)) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());

        String[] tokens = body.split("\\s+", 2);
        if (tokens.length < 2 || StringUtils.isNullOrEmptyEx(tokens[1])) {
            return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
        }

        MinecraftRecord record = resolveRecord(tokens[0], event);
        if (record == null) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
        if (!hasRconOp(bot, event.getGroupId(), event.getUserId(), record))
            return bot.setMsgEmojiLikeNo(event.getMessageId());

        bot.setMsgEmojiLikeOk(event.getMessageId());
        String result = executeRcon(record, tokens[1]);
        return sendRconResult(bot, event, record, tokens[1], result);
    }

    @AnyMessageHandler
    public boolean config(BaniraBot bot, AnyMessageEvent event) {
        BaniraCodeContext context = new BaniraCodeContext(bot, event);
        if (!super.isCommand(context)) return false;
        String cmdBody = super.deleteCommandPrefix(context);
        List<String> mcRcon = insConfig.get().mcRcon();
        if (mcRcon.stream().noneMatch(ins -> cmdBody.startsWith(ins + " "))) return false;

        String[] split = cmdBody.split("\\s+");
        if (split.length < 2) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
        String operate = split[1];
        String[] args = Arrays.copyOfRange(split, 2, split.length);
        BaseInstructionsConfig baseIns = BaniraUtils.getBaseIns();
        LoginInfoResp loginInfoEx = bot.getLoginInfoEx();

        List<Map<String, Object>> forwardMsg = new ArrayList<>();
        forwardMsg.add(ShiroUtils.generateSingleMsg(event.getUserId(), event.getSender().getNickname(), event.getMessage()));

        if (baseIns.add().contains(operate)) {
            return handleAdd(bot, event, args, loginInfoEx, forwardMsg);
        } else if (baseIns.del().contains(operate)) {
            return handleDel(bot, event, args, loginInfoEx, forwardMsg);
        } else if (baseIns.list().contains(operate)) {
            return handleList(bot, event, args, loginInfoEx, forwardMsg);
        }
        return false;
    }

    @PrivateMessageHandler
    public boolean setPassword(BaniraBot bot, PrivateMessageEvent event) {
        BaniraCodeContext context = new BaniraCodeContext(bot, event.getArrayMsg(), 0L, event.getUserId(), event.getUserId())
                .msg(event.getMessage());
        if (!super.isCommand(context)) return false;
        String cmdBody = super.deleteCommandPrefix(context);
        InstructionsConfig ins = insConfig.get();
        List<String> mcRcon = ins.mcRcon();
        if (mcRcon.stream().noneMatch(alias -> cmdBody.startsWith(alias + " "))) return false;

        String[] split = cmdBody.split("\\s+");
        if (split.length < 3) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
        if (!ins.mcRconPsw().contains(split[1])) return false;

        long id = StringUtils.toLong(split[2], 0);
        if (id <= 0) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
        if (split.length < 4 || StringUtils.isNullOrEmptyEx(split[3])) {
            return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
        }

        MinecraftRecord record = minecraftRecordManager.getMinecraftRecord(id);
        if (record == null || !record.getEnable()) {
            bot.sendPrivateMsg(event.getUserId(), "未找到编号为 " + id + " 的服务器记录。", false);
            return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
        }
        if (!hasRconOp(bot, 0L, event.getUserId(), record)) {
            bot.sendPrivateMsg(event.getUserId(), "权限不足，无法设置该服务器的 RCON 密码。", false);
            return bot.setMsgEmojiLikeNo(event.getMessageId());
        }

        String password = String.join(" ", Arrays.copyOfRange(split, 3, split.length));
        record.setRconPsw(password);
        try {
            minecraftRecordManager.modifyMinecraftRecord(record);
            bot.sendPrivateMsg(event.getUserId(),
                    "RCON 密码已更新。\nMC服务器编号：" + record.getId() + "\nMC服务器名称：" + record.getName(), false);
            return bot.setMsgEmojiLikeOk(event.getMessageId());
        } catch (Exception e) {
            LOGGER.error("Failed to set RCON password for record {}", id, e);
            bot.sendPrivateMsg(event.getUserId(), "密码设置失败：" + e.getMessage(), false);
            return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
        }
    }

    // region 配置操作

    private boolean handleAdd(BaniraBot bot, AnyMessageEvent event, String[] args
            , LoginInfoResp loginInfoEx, List<Map<String, Object>> forwardMsg
    ) {
        NetAddressUtils.NetAddress addr = NetAddressUtils.findAddressAndPort(args, 0);
        if (addr == null) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());

        String name = (addr.index() > 0)
                ? joinTokens(args, 0, addr.index())
                : "Minecraft Server #" + StringUtils.md5(addr.host() + addr.port()).toLowerCase().substring(0, 6);

        MinecraftRecord record = findRecordByName(name, event.getSelfId(), event.getGroupId());
        boolean isNew = record == null;
        if (isNew) {
            record = new MinecraftRecord()
                    .setBotId(event.getSelfId())
                    .setGroupId(event.getGroupId())
                    .setCreatorId(event.getUserId())
                    .setTime(event.getTime())
                    .setName(name)
                    .setQueryIp(addr.host())
                    .setQueryPort(25565);
        }
        record.setRconIp(addr.host()).setRconPort(addr.port());

        String reason;
        try {
            if (isNew) {
                minecraftRecordManager.addMinecraftRecord(record);
            } else {
                minecraftRecordManager.modifyMinecraftRecord(record);
            }
            reason = "RCON 绑定成功";
        } catch (Exception e) {
            reason = "RCON 绑定失败：" + e.getMessage();
            LOGGER.error("Failed to bind RCON", e);
        }

        forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname(), formatRconRecord(record, reason)
                + "\n请私聊发送 " + BaniraUtils.getInsPrefixWithSpace() + insConfig.get().mcRcon().getFirst()
                + " " + insConfig.get().mcRconPsw().getFirst() + " " + record.getId() + " <密码> 完成密码设置。"));
        ActionData<MsgId> msgIdData = bot.sendForwardMsg(event, forwardMsg);
        return bot.isActionDataMsgIdNotEmpty(msgIdData);
    }

    private boolean handleDel(BaniraBot bot, AnyMessageEvent event, String[] args
            , LoginInfoResp loginInfoEx, List<Map<String, Object>> forwardMsg
    ) {
        if (BaniraUtils.hasReply(event.getArrayMsg())) {
            if (BaniraUtils.getReplyUserId(bot, event.getGroupId(), event.getArrayMsg()) != bot.getSelfId()) {
                return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            }
            Long replyId = BaniraUtils.getReplyId(event.getArrayMsg());
            if (replyId == null) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            ActionData<MsgResp> replyMsgData = bot.getMsg(replyId.intValue());
            if (!bot.isActionDataNotEmpty(replyMsgData) || !BaniraUtils.hasForward(replyMsgData.getData().getArrayMsg())) {
                return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            }
            ActionData<GetForwardMsgResp> forwardMsgResp = bot.getForwardMsg(event.getGroupId(), replyMsgData.getData().getUserId(), replyId.intValue());
            if (!bot.isActionDataNotEmpty(forwardMsgResp)) {
                return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            }
            List<Long> ids = forwardMsgResp.getData().getMessages().stream()
                    .filter(data -> StringUtils.toLong(data.getSender().getUserId()) == bot.getSelfId())
                    .map(MessageEvent::getMessage)
                    .filter(StringUtils::isNotNullOrEmpty)
                    .filter(data -> data.startsWith("MC服务器编号："))
                    .map(data -> CollectionUtils.getOrDefault(data.split("MC服务器编号："), 1, "").strip())
                    .map(data -> CollectionUtils.getFirst(data.split("\\s")))
                    .map(StringUtils::toLong)
                    .filter(data -> data > 0).toList();
            for (Long id : ids) {
                clearRconConfig(bot, event, minecraftRecordManager.getMinecraftRecord(id), loginInfoEx, forwardMsg);
            }
            ActionData<MsgId> msgIdData = bot.sendForwardMsg(event, forwardMsg);
            return bot.isActionDataMsgIdNotEmpty(msgIdData);
        } else if (args.length > 0) {
            Long[] ids = Arrays.stream(args)
                    .map(StringUtils::toLong)
                    .filter(data -> data > 0)
                    .distinct()
                    .toArray(Long[]::new);
            List<MinecraftRecord> recordList = minecraftRecordManager.getMinecraftRecordList(
                    new MinecraftRecordQueryParam().setId(ids).setEnable(true));
            if (recordList.isEmpty()) {
                forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname(), "未查询到MC服务器"));
            } else {
                for (MinecraftRecord record : recordList) {
                    clearRconConfig(bot, event, record, loginInfoEx, forwardMsg);
                }
            }
            ActionData<MsgId> msgIdData = bot.sendForwardMsg(event, forwardMsg);
            return bot.isActionDataMsgIdNotEmpty(msgIdData);
        }
        return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
    }

    private boolean handleList(BaniraBot bot, AnyMessageEvent event, String[] args
            , LoginInfoResp loginInfoEx, List<Map<String, Object>> forwardMsg
    ) {
        long page = StringUtils.toLong(CollectionUtils.getOrDefault(args, 0, ""), 0);
        String keyWord = String.join("", Arrays.copyOfRange(args, page > 0 && args.length > 1 ? 1 : 0, args.length));
        if (page <= 0) page = 1;
        PageResult<MinecraftRecord> pageList = minecraftRecordManager.getMinecraftRecordPagedList(
                new MinecraftRecordQueryParam(true, page, 98)
                        .setBotId(bot.getSelfId())
                        .setGroupId(0L, event.getGroupId())
                        .addKeyWord(String.format("%%%s%%", keyWord))
                        .setEnable(true)
                        .addOrderBy(MinecraftRecordQueryParam.ORDER_ID, true)
        );
        List<MinecraftRecord> rconRecords = pageList.getRecords().stream()
                .filter(this::hasRconConfig)
                .toList();
        if (rconRecords.isEmpty()) {
            forwardMsg.add(ShiroUtils.generateSingleMsg(loginInfoEx.getUserId(), loginInfoEx.getNickname(), "未查询到已配置 RCON 的服务器"));
        } else {
            forwardMsg.add(ShiroUtils.generateSingleMsg(loginInfoEx.getUserId(), loginInfoEx.getNickname()
                    , "RCON 服务器总数：" + rconRecords.size() + "\n当前页：" + pageList.getPage()));
            for (MinecraftRecord record : rconRecords) {
                forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname(), formatRconRecord(record, null)));
            }
        }
        ActionData<MsgId> msgIdData = bot.sendForwardMsg(event, forwardMsg);
        return bot.isActionDataMsgIdNotEmpty(msgIdData);
    }

    // endregion 配置操作

    // region RCON 执行

    private String executeRcon(MinecraftRecord record, String command) {
        String host = resolveRconHost(record);
        int port = resolveRconPort(record);
        String password = record.getRconPsw();
        if (StringUtils.isNullOrEmptyEx(password)) {
            return "RCON 密码未设置，请私聊发送密码设置指令。";
        }
        try (Rcon rcon = Rcon.open(host, port)) {
            rcon.tryAuthenticate(password);
            String response = rcon.sendCommand(command);
            return StringUtils.isNullOrEmptyEx(response) ? "(无返回内容)" : response.strip();
        } catch (IOException e) {
            LOGGER.warn("RCON command failed for {}:{} - {}", host, port, e.getMessage());
            return formatRconError(record, e);
        }
    }

    private String formatRconError(MinecraftRecord record, IOException e) {
        String msg = e.getMessage() == null ? "" : e.getMessage().toLowerCase(Locale.ROOT);
        McConfig mcConfig = BaniraUtils.getGroupConfigOrGlobal(McQueryGroupConfig.class, 0L).mcConfig();
        if (msg.contains("auth")) {
            if (CollectionUtils.isNotNullOrEmpty(mcConfig.pswError())) {
                return CollectionUtils.getRandomElement(mcConfig.pswError(), new Random())
                        .replace("%s", record.getName());
            }
            return record.getName() + " RCON 密码错误。";
        }
        return record.getName() + " RCON 执行失败：" + e.getMessage();
    }

    private boolean sendRconResult(BaniraBot bot, AnyMessageEvent event, MinecraftRecord record, String command, String result) {
        LoginInfoResp loginInfoEx = bot.getLoginInfoEx();
        List<Map<String, Object>> forwardMsg = new ArrayList<>();
        forwardMsg.add(ShiroUtils.generateSingleMsg(event.getUserId(), event.getSender().getNickname(), event.getMessage()));
        forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                , "MC服务器：" + record.getName() + " (#" + record.getId() + ")\n"
                        + "命令：" + command + "\n"
                        + "结果：\n" + result
        ));
        ActionData<MsgId> msgIdData = bot.sendForwardMsg(event, forwardMsg);
        if (!bot.isActionDataMsgIdNotEmpty(msgIdData)) {
            return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
        }
        return true;
    }

    // endregion RCON 执行

    // region 工具方法

    private String matchSlashRcon(String message) {
        if (StringUtils.isNullOrEmptyEx(message)) return null;
        for (String ins : insConfig.get().mcRcon()) {
            String prefix = "/" + ins;
            if (message.equals(prefix) || message.startsWith(prefix + " ")) {
                return ins;
            }
        }
        return null;
    }

    private String stripSlashPrefix(String message, String matchedSlash) {
        String prefix = "/" + matchedSlash;
        return message.equals(prefix) ? "" : message.substring(prefix.length()).trim();
    }

    private MinecraftRecord resolveRecord(String key, AnyMessageEvent event) {
        long id = StringUtils.toLong(key, 0);
        if (id > 0) {
            MinecraftRecord record = minecraftRecordManager.getMinecraftRecord(id);
            if (record != null && record.getEnable() && Objects.equals(record.getBotId(), event.getSelfId())) {
                return record;
            }
            return null;
        }
        List<MinecraftRecord> records = minecraftRecordManager.getMinecraftRecordList(
                new MinecraftRecordQueryParam()
                        .setBotId(event.getSelfId())
                        .setGroupId(0L, event.getGroupId())
                        .setName(String.format("%%%s%%", key))
                        .setEnable(true)
        );
        if (records.size() == 1) return records.getFirst();
        return null;
    }

    private MinecraftRecord findRecordByName(String name, Long botId, Long groupId) {
        List<MinecraftRecord> records = minecraftRecordManager.getMinecraftRecordList(
                new MinecraftRecordQueryParam()
                        .setBotId(botId)
                        .setGroupId(0L, groupId)
                        .setName(name)
                        .setEnable(true)
        );
        return records.isEmpty() ? null : records.getFirst();
    }

    private boolean hasRconConfig(MinecraftRecord record) {
        return StringUtils.isNotNullOrEmpty(resolveRconHost(record)) && resolveRconPort(record) > 0;
    }

    private String resolveRconHost(MinecraftRecord record) {
        if (StringUtils.isNotNullOrEmpty(record.getRconIp())) return record.getRconIp();
        return record.getQueryIp();
    }

    private int resolveRconPort(MinecraftRecord record) {
        if (record.getRconPort() != null && record.getRconPort() > 0) return record.getRconPort();
        return DEFAULT_RCON_PORT;
    }

    private boolean hasRconOp(BaniraBot bot, Long groupId, Long userId, MinecraftRecord record) {
        Long effectiveGroupId = BaniraUtils.isGroupIdValid(record.getGroupId())
                ? record.getGroupId()
                : BaniraUtils.isGroupIdValid(groupId) ? groupId : null;
        return Objects.equals(userId, record.getCreatorId())
                || bot.isUpper(effectiveGroupId, userId, record.getCreatorId());
    }

    private void clearRconConfig(BaniraBot bot, AnyMessageEvent event, MinecraftRecord record
            , LoginInfoResp loginInfoEx, List<Map<String, Object>> forwardMsg
    ) {
        if (record == null) return;
        String reason;
        if (!hasRconOp(bot, event.getGroupId(), event.getUserId(), record)) {
            reason = "RCON 清除失败：权限不足";
        } else {
            record.setRconIp("").setRconPort(0).setRconPsw("");
            try {
                minecraftRecordManager.modifyMinecraftRecord(record);
                reason = "RCON 配置已清除";
            } catch (Exception e) {
                reason = "RCON 清除失败：" + e.getMessage();
            }
        }
        forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                , formatRconRecord(record, reason)));
    }

    private String formatRconRecord(MinecraftRecord record, String reason) {
        StringBuilder sb = new StringBuilder();
        if (record.getId() != null && record.getId() > 0) {
            sb.append("MC服务器编号：").append(record.getId()).append('\n');
        }
        sb.append("MC服务器名称：").append(record.getName()).append('\n');
        sb.append("RCON地址：").append(resolveRconHost(record)).append('\n');
        sb.append("RCON端口：").append(resolveRconPort(record)).append('\n');
        sb.append("密码状态：").append(StringUtils.isNotNullOrEmpty(record.getRconPsw()) ? "已设置" : "未设置");
        if (!StringUtils.isNullOrEmptyEx(reason)) {
            sb.append('\n').append(reason);
        }
        return sb.toString();
    }

    private String joinTokens(String[] tokens, int fromIndex, int toIndex) {
        StringBuilder sb = new StringBuilder();
        for (int i = fromIndex; i < toIndex && i < tokens.length; i++) {
            if (!sb.isEmpty()) sb.append(' ');
            sb.append(tokens[i]);
        }
        return sb.toString();
    }

    // endregion 工具方法

}
