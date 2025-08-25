package xin.vanilla.banira.plugin;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.microsoft.playwright.Browser;
import com.microsoft.playwright.Page;
import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.common.utils.MsgUtils;
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
import xin.vanilla.banira.coder.McQueryCode;
import xin.vanilla.banira.config.entity.basic.BaseInstructionsConfig;
import xin.vanilla.banira.domain.KeyValue;
import xin.vanilla.banira.domain.MinecraftRecord;
import xin.vanilla.banira.domain.PageResult;
import xin.vanilla.banira.mapper.param.MinecraftRecordQueryParam;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.service.IMinecraftRecordManager;
import xin.vanilla.banira.util.*;
import xin.vanilla.banira.util.html.HtmlScreenshotConfig;
import xin.vanilla.banira.util.html.HtmlScreenshotResult;
import xin.vanilla.banira.util.html.HtmlScreenshotUtils;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * MC服务器查询
 */
@Slf4j
@Shiro
@Component
public class McQueryPlugin extends BasePlugin {

    @Resource
    private IMinecraftRecordManager minecraftRecordManager;

    private static final File HTML_FILE = new File("config/mc_query_plugin/index.html");
    private static final File CONFIG_FILE = new File("config/mc_query_plugin/config.js");

    private static final Set<String> helpType = BaniraUtils.mutableSetOf(
            "mc", "mcquery", "mcrcon"
    );

    @Nonnull
    @Override
    public List<String> getHelpInfo(@Nullable Long groupId, @Nonnull String... types) {
        List<String> result = new ArrayList<>();
        String type = CollectionUtils.getFirst(types);
        if (helpType.stream().anyMatch(s -> StringUtils.isNullOrEmptyEx(type) || s.equalsIgnoreCase(type))) {
            BaseInstructionsConfig baseIns = BaniraUtils.getBaseIns();

            result.add("MC服务器配置 - 增加：\n" +
                    "增加MC服务器配置信息，下次可直接使用名称查询。" + "\n\n" +
                    "用法1：\n" +
                    BaniraUtils.getInsPrefixWithSpace() +
                    "mc" + " " +
                    baseIns.add() + " " +
                    "[<服务器名称>]" + " " +
                    "<查询地址>" + " " +
                    "<查询端口>" + "\n\n" +
                    "用法2：\n" +
                    BaniraUtils.getInsPrefixWithSpace() +
                    "mc" + " " +
                    baseIns.add() + " " +
                    "[<服务器名称>]" + " " +
                    "<查询地址:查询端口>"
            );
            result.add("MC服务器配置 - 删除：\n" +
                    "删除MC服务器配置信息。" + "\n\n" +
                    "用法1：(根据MC服务器编号删除)\n" +
                    BaniraUtils.getInsPrefixWithSpace() +
                    "mc" + " " +
                    baseIns.del() + " " +
                    "<MC服务器编号> ..." + "\n\n" +
                    "用法2：(回复添加成功的响应消息)\n" +
                    BaniraUtils.getInsPrefixWithSpace() +
                    "mc" + " " +
                    baseIns.del()
            );
            result.add("MC服务器配置 - 查询：\n\n" +
                    BaniraUtils.getInsPrefixWithSpace() +
                    "mc" + " " +
                    baseIns.list() + " " +
                    "[<页数>]" + " " +
                    "<MC服务器名称>"
            );
            result.add("MC服务器查询：\n" +
                    "直接通过地址查询服务器信息。" + "\n\n" +
                    "用法1：\n" +
                    querys + " " +
                    "[<服务器名称>]" + " " +
                    "<查询地址>" + " " +
                    "<查询端口>" + "\n\n" +
                    "用法2：\n" +
                    querys + " " +
                    "[<服务器名称>]" + " " +
                    "<查询地址:查询端口>"
            );
        }
        return result;
    }

    private static final Set<String> querys = BaniraUtils.mutableSetOf(
            "/list", "/ls"
    );

    @AnyMessageHandler
    public boolean query(BaniraBot bot, AnyMessageEvent event) {
        String message = event.getMessage();
        if (querys.stream().anyMatch(message::startsWith)) {
            List<MinecraftRecord> records = extractMinecraftRecords(message, event);

            if (CollectionUtils.isNotNullOrEmpty(records)) {
                // 合并转发文字
                if (records.size() > 1) {
                    LoginInfoResp loginInfoEx = bot.getLoginInfoEx();
                    List<Map<String, Object>> msg = new ArrayList<>();
                    msg.add(ShiroUtils.generateSingleMsg(event.getUserId(), event.getSender().getNickname(), event.getMessage()));
                    for (MinecraftRecord record : records) {
                        msg.add(ShiroUtils.generateSingleMsg(loginInfoEx.getUserId(), loginInfoEx.getNickname()
                                , McQueryCode.getQueryInfo(record.getName(), record.getQueryIp(), record.getQueryPort())
                        ));
                    }
                    ActionData<MsgId> msgIdData = bot.sendForwardMsg(event, msg);
                    return bot.isActionDataMsgIdNotEmpty(msgIdData);
                }
                // 图片
                else {
                    bot.setMsgEmojiLikeOk(event.getMessageId());
                    String msg = getQueryInfoImg(records.getFirst());
                    if (StringUtils.isNullOrEmptyEx(msg)) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
                    ActionData<MsgId> msgIdData = bot.sendMsg(event, msg, false);
                    return bot.isActionDataMsgIdNotEmpty(msgIdData);
                }
            }
        }
        return false;
    }

    @AnyMessageHandler
    public boolean config(BaniraBot bot, AnyMessageEvent event) {
        String message = event.getMessage();
        if (super.isCommand(message)
                && insConfig.get().mcQuery().stream().anyMatch(ins -> super.replaceCommand(message).startsWith(ins))
        ) {
            String[] split = super.replaceCommand(message).split("\\s+");
            if (split.length < 2) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            String operate = split[1];
            String[] args = Arrays.copyOfRange(split, 2, split.length);
            BaseInstructionsConfig baseIns = BaniraUtils.getBaseIns();
            LoginInfoResp loginInfoEx = bot.getLoginInfoEx();

            List<Map<String, Object>> forwardMsg = new ArrayList<>();
            forwardMsg.add(ShiroUtils.generateSingleMsg(event.getUserId(), event.getSender().getNickname(), event.getMessage()));
            // 添加
            if (baseIns.add().contains(operate)) {
                NetAddressUtils.NetAddress addr = NetAddressUtils.findAddressAndPort(args, 0);
                if (addr == null) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
                String name = (addr.index() > 0)
                        ? joinTokens(args, 0, addr.index())
                        : "Minecraft Server #" + StringUtils.md5(addr.host() + addr.port()).toLowerCase().substring(0, 6);

                MinecraftRecord record = new MinecraftRecord()
                        .setBotId(event.getSelfId())
                        .setGroupId(event.getGroupId())
                        .setCreatorId(event.getUserId())
                        .setTime(event.getTime())
                        .setName(name)
                        .setQueryIp(addr.host())
                        .setQueryPort(addr.port());

                String reason;
                try {
                    minecraftRecordManager.addMinecraftRecord(record);
                    reason = "添加成功";
                } catch (Exception e) {
                    reason = "添加失败：" + e.getMessage();
                    LOGGER.error("Failed to add MinecraftRecord", e);
                }

                forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                        , (record.getId() != null && record.getId() != 0 ? "MC服务器编号：" + record.getId() + "\n" : "") +
                                "MC服务器名称：" + record.getName() + "\n" +
                                "群号：" + record.getGroupId() + "\n" +
                                "查询地址：" + record.getQueryIp() + "\n" +
                                "查询端口：" + record.getQueryPort() + "\n" +
                                reason
                ));
                forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                        , getQueryInfoImg(record)
                ));
                ActionData<MsgId> msgIdData = bot.sendForwardMsg(event, forwardMsg);
                return bot.isActionDataMsgIdNotEmpty(msgIdData);
            }
            // 删除
            else if (baseIns.del().contains(operate)) {
                // 回复删除
                if (BaniraUtils.hasReply(event.getArrayMsg())) {
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
                                MinecraftRecord record = minecraftRecordManager.getMinecraftRecord(id);
                                this.deleteMinecraftRecord(bot, event, record, loginInfoEx, forwardMsg);
                            }
                            ActionData<MsgId> msgIdData = bot.sendForwardMsg(event, forwardMsg);
                            return bot.isActionDataMsgIdNotEmpty(msgIdData);
                        }
                    }
                }
                // ID删除
                else if (args.length > 0) {
                    Long[] ids = Arrays.stream(args)
                            .map(StringUtils::toLong)
                            .filter(data -> data > 0)
                            .distinct()
                            .toArray(Long[]::new);

                    List<MinecraftRecord> recordList = minecraftRecordManager.getMinecraftRecordList(new MinecraftRecordQueryParam().setId(ids).setEnable(true));
                    if (!recordList.isEmpty()) {
                        for (MinecraftRecord record : recordList) {
                            this.deleteMinecraftRecord(bot, event, record, loginInfoEx, forwardMsg);
                        }
                    } else {
                        forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                                , "未查询到MC服务器"
                        ));
                    }
                    ActionData<MsgId> msgIdData = bot.sendForwardMsg(event, forwardMsg);
                    return bot.isActionDataMsgIdNotEmpty(msgIdData);
                }
                //
                else return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            }
            // 查询
            else if (baseIns.list().contains(operate)) {
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
                if (pageList.isEmpty()) {
                    forwardMsg.add(ShiroUtils.generateSingleMsg(loginInfoEx.getUserId(), loginInfoEx.getNickname()
                            , "未查询到MC服务器"
                    ));
                } else {
                    forwardMsg.add(ShiroUtils.generateSingleMsg(loginInfoEx.getUserId(), loginInfoEx.getNickname()
                            , "MC服务器总数：" + pageList.getTotal() + "\n" +
                                    "当前页：" + pageList.getPage() + "\n" +
                                    "总页数：" + pageList.getTotalPages() + "\n" +
                                    "每页数量：" + pageList.getSize() + "\n" +
                                    "当前页数量：" + pageList.getRecords().size()
                    ));
                    for (MinecraftRecord record : pageList.getRecords()) {
                        forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                                , "MC服务器编号：" + record.getId() + "\n" +
                                        "MC服务器名称：" + record.getName() + "\n" +
                                        "群号：" + record.getGroupId() + "\n" +
                                        "查询地址：" + record.getQueryIp() + "\n" +
                                        "查询端口：" + record.getQueryPort()
                        ));
                    }
                }
                ActionData<MsgId> msgIdData = bot.sendForwardMsg(event, forwardMsg);
                return bot.isActionDataMsgIdNotEmpty(msgIdData);
            }
            // 全局
            else if (baseIns.global().contains(operate)) {
                if (!BaniraUtils.isOwner(event.getUserId()) && BaniraUtils.isButler(event.getUserId()))
                    return bot.setMsgEmojiLikeNo(event.getMessageId());

                // 回复修改
                if (BaniraUtils.hasReply(event.getArrayMsg())) {
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
                                MinecraftRecord record = minecraftRecordManager.getMinecraftRecord(id);
                                record.setGroupId(0L);
                                modifyMinecraftRecord(bot, event, record, loginInfoEx, forwardMsg);
                            }
                            ActionData<MsgId> msgIdData = bot.sendForwardMsg(event, forwardMsg);
                            return bot.isActionDataMsgIdNotEmpty(msgIdData);
                        }
                    }
                }
                // ID修改
                else if (args.length > 0) {
                    Long[] ids = Arrays.stream(args)
                            .map(StringUtils::toLong)
                            .filter(data -> data > 0)
                            .distinct()
                            .toArray(Long[]::new);

                    List<MinecraftRecord> recordList = minecraftRecordManager.getMinecraftRecordList(new MinecraftRecordQueryParam().setId(ids).setEnable(true));
                    if (!recordList.isEmpty()) {
                        for (MinecraftRecord record : recordList) {
                            this.deleteMinecraftRecord(bot, event, record, loginInfoEx, forwardMsg);
                        }
                    } else {
                        forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                                , "未查询到MC服务器"
                        ));
                    }
                    ActionData<MsgId> msgIdData = bot.sendForwardMsg(event, forwardMsg);
                    return bot.isActionDataMsgIdNotEmpty(msgIdData);
                }
                //
                else return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            } else return false;
        }
        return false;
    }

    public List<MinecraftRecord> extractMinecraftRecords(String message, AnyMessageEvent event) {
        if (message == null) {
            return Collections.emptyList();
        }

        String trimmed = message.trim();
        if (trimmed.isEmpty()) {
            return Collections.emptyList();
        }

        String[] tokens = trimmed.split("\\s+");
        List<MinecraftRecord> records = new ArrayList<>();

        // 没有第二个 token：直接返回默认列表
        if (tokens.length < 2 || StringUtils.isNullOrEmptyEx(tokens[1])) {
            records.addAll(fetchRecordsByName(null, event.getSelfId(), event.getGroupId()));
            return records;
        }

        NetAddressUtils.NetAddress addr = NetAddressUtils.findAddressAndPort(tokens, 1);
        if (addr != null) {
            String ip = addr.host();
            int port = addr.port();
            String name = (addr.index() > 1) ? joinTokens(tokens, 1, addr.index()) : "Minecraft Server";

            records.add(new MinecraftRecord()
                    .setBotId(event.getSelfId())
                    .setGroupId(event.getGroupId())
                    .setName(name)
                    .setQueryIp(ip)
                    .setQueryPort(port)
            );
        } else {
            // 未找到 host/host，从库中查询
            String name = message.substring(message.indexOf(tokens[1]));
            records.addAll(fetchRecordsByName(name, event.getSelfId(), event.getGroupId()));
        }
        return records;
    }

    public String getQueryInfoImg(MinecraftRecord record) {
        try {
            if (!HTML_FILE.exists()) {
                ResourceCopyUtils.copyResources("template/mc_query_plugin", HTML_FILE.getParent());
            }
        } catch (Exception e) {
            LOGGER.error("Failed to copy resources", e);
            return null;
        }

        try {
            JsonObject status = this.generateStatus(record);
            FileOutputStream fileOutputStream = new FileOutputStream(CONFIG_FILE);
            OutputStreamWriter outputStreamWriter = new OutputStreamWriter(fileOutputStream, StandardCharsets.UTF_8);
            outputStreamWriter.write("const configData = " + JsonUtils.PRETTY_GSON.toJson(status));
            outputStreamWriter.close();
        } catch (Exception e) {
            LOGGER.error("Failed to write config.json", e);
            return null;
        }

        try {
            HtmlScreenshotResult render = HtmlScreenshotUtils.render(
                    new HtmlScreenshotConfig(new File("config/mc_query_plugin/index.html"))
                            .setContextOptions(new Browser.NewContextOptions()
                                    .setViewportSize(800, 380)
                            )
                            .setScreenshotOptions(new Page.ScreenshotOptions()
                                    .setFullPage(true)
                            )
            );
            return MsgUtils.builder()
                    .img(render.getByte())
                    .build();
        } catch (Exception e) {
            LOGGER.error("Failed to render html", e);
            return null;
        }
    }


    private void deleteMinecraftRecord(BaniraBot bot, AnyMessageEvent event
            , MinecraftRecord record, LoginInfoResp loginInfoEx, List<Map<String, Object>> forwardMsg
    ) {
        if (record == null) return;
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
            reason = "\n删除失败：MC服务器未启用";
        }
        if (StringUtils.isNullOrEmptyEx(reason)) {
            try {
                if (minecraftRecordManager.deleteMinecraftRecord(record.getId()) > 0) {
                    reason = "\n删除成功";
                } else {
                    reason = "\n删除失败";
                }
            } catch (Exception e) {
                reason = "\n删除失败：" + e.getMessage();
            }
        }
        forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                , (record.getId() != null && record.getId() != 0 ? "MC服务器编号：" + record.getId() + "\n" : "") +
                        "MC服务器名称：" + record.getName() + "\n" +
                        "群号：" + record.getGroupId() + "\n" +
                        "查询地址：" + record.getQueryIp() + "\n" +
                        "查询端口：" + record.getQueryPort() + "\n" +
                        reason
        ));
    }

    private void modifyMinecraftRecord(BaniraBot bot, AnyMessageEvent event
            , MinecraftRecord record, LoginInfoResp loginInfoEx, List<Map<String, Object>> forwardMsg
    ) {
        if (record == null) return;
        Long groupId = BaniraUtils.isGroupIdValid(record.getGroupId())
                ? record.getGroupId()
                : BaniraUtils.isGroupIdValid(event.getGroupId()) ? event.getGroupId() : null;
        boolean hasOp = Objects.equals(event.getUserId(), record.getCreatorId())
                || bot.isUpper(groupId, event.getUserId(), record.getCreatorId());
        boolean enable = record.getEnable();
        String reason = "";
        if (!hasOp) {
            reason = "\n修改失败：权限不足";
        } else if (!enable) {
            reason = "\n修改失败：MC服务器未启用";
        }
        if (StringUtils.isNullOrEmptyEx(reason)) {
            try {
                minecraftRecordManager.modifyMinecraftRecord(record);
                reason = "\n修改成功";
            } catch (Exception e) {
                reason = "\n修改失败：" + e.getMessage();
            }
        }
        forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                , (record.getId() != null && record.getId() != 0 ? "MC服务器编号：" + record.getId() + "\n" : "") +
                        "MC服务器名称：" + record.getName() + "\n" +
                        "群号：" + record.getGroupId() + "\n" +
                        "查询地址：" + record.getQueryIp() + "\n" +
                        "查询端口：" + record.getQueryPort() + "\n" +
                        reason
        ));
    }

    /**
     * 将 tokens[fromIndex] ... tokens[toIndex-1] 用空格拼接
     */
    private String joinTokens(String[] tokens, int fromIndex, int toIndex) {
        StringBuilder sb = new StringBuilder();
        for (int i = fromIndex; i < toIndex && i < tokens.length; i++) {
            if (!sb.isEmpty()) sb.append(' ');
            sb.append(tokens[i]);
        }
        return sb.toString();
    }

    private List<MinecraftRecord> fetchRecordsByName(String nameOrNull, Long botId, Long groupId) {
        MinecraftRecordQueryParam param = new MinecraftRecordQueryParam()
                .setBotId(botId)
                .setGroupId(0L, groupId)
                .setEnable(true);
        if (nameOrNull != null) {
            param.setName(String.format("%%%s%%", nameOrNull));
        }
        return minecraftRecordManager.getMinecraftRecordList(param);
    }


    private JsonObject generateStatus(MinecraftRecord record) {
        McQueryHelper mcQuery = McQueryHelper.create(record.getName(), record.getQueryIp() + ":" + record.getQueryPort());
        mcQuery.query();

        JsonObject result = new JsonObject();

        JsonUtils.setString(result, "serverName", mcQuery.serverName());
        JsonUtils.setString(result, "serverIcon", JsonUtils.getString(mcQuery.getServerJson(), "favicon", ""));
        JsonUtils.setString(result, "serverAddress", mcQuery.serverAddress());
        JsonUtils.setString(result, "description", mcQuery.descriptionHtml());
        JsonUtils.setLong(result, "ping", mcQuery.getPing());
        JsonUtils.setInt(result, "maxPlayer", mcQuery.maxPlayers());
        JsonUtils.setInt(result, "onlinePlayer", mcQuery.onlinePlayers());
        JsonUtils.setString(result, "version.name", JsonUtils.getString(mcQuery.getServerJson(), "version.name", ""));

        JsonArray players = new JsonArray();
        for (KeyValue<String, String> kv : mcQuery.playerList()) {
            JsonObject playerObject = new JsonObject();
            JsonUtils.setString(playerObject, "name", kv.getKey());
            JsonUtils.setString(playerObject, "uuid", kv.getValue().replace("-", ""));
            players.add(playerObject);
        }
        result.add("onlinePlayers", players);

        return result;
    }

}
