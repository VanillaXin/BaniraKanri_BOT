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
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.coder.message.McQueryCode;
import xin.vanilla.banira.config.entity.InstructionsConfig;
import xin.vanilla.banira.config.entity.basic.BaseInstructionsConfig;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.KeyValue;
import xin.vanilla.banira.domain.MinecraftRecord;
import xin.vanilla.banira.domain.PageResult;
import xin.vanilla.banira.mapper.param.MinecraftRecordQueryParam;
import xin.vanilla.banira.plugin.chat.capability.AiCapability;
import xin.vanilla.banira.plugin.chat.capability.AiCapabilityArgs;
import xin.vanilla.banira.plugin.chat.capability.AiCapabilityParameter;
import xin.vanilla.banira.plugin.chat.capability.AiCapabilityProvider;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.plugin.help.HelpTopic;
import xin.vanilla.banira.plugin.help.HelpTopics;
import xin.vanilla.banira.plugin.mcquery.McQueryService;
import xin.vanilla.banira.service.IMinecraftRecordManager;
import xin.vanilla.banira.util.*;
import xin.vanilla.banira.util.html.HtmlScreenshotConfig;
import xin.vanilla.banira.util.html.HtmlScreenshotResult;
import xin.vanilla.banira.util.html.HtmlScreenshotUtils;

import java.io.File;
import java.util.*;

/**
 * MC服务器查询
 */
@Slf4j
@Shiro
@Component
public class McQueryPlugin extends BasePlugin implements AiCapabilityProvider {

    @Resource
    private IMinecraftRecordManager minecraftRecordManager;
    @Resource
    private McQueryService mcQueryService;

    private static final File HTML_FILE = new File("config/plugin/mc_query_plugin/index.html");
    private static final String CLIP_SELECTOR = ".card";

    @Override
    public void registerHelpTopics(@Nonnull List<HelpTopic> topics, Long groupId) {
        InstructionsConfig ins = insConfig.get();
        BaseInstructionsConfig base = BaniraUtils.getBaseIns();
        String prefix = BaniraUtils.getInsPrefixWithSpace();
        List<String> mcQuery = ins.mcQuery();
        String mcCmd = prefix + mcQuery.getFirst();
        String slashCmd = "/" + ins.mcQuerySlash().getFirst();
        String textFlag = HelpTopics.formatAliasChoices(textOutputFlags());
        String imgFlag = HelpTopics.formatAliasChoices(imageOutputFlags());

        HelpTopic topic = HelpTopics.of("MC服务器", "查询 Minecraft 服务器在线状态与玩家列表。", 99, mcQuery);
        topic.child(HelpTopics.opAdd(base,
                "保存服务器配置，之后可直接用名称查询。\n\n"
                        + "用法1：\n" + mcCmd + " " + base.add().getFirst() + " [<名称>] <查询地址> <查询端口>\n\n"
                        + "用法2：\n" + mcCmd + " " + base.add().getFirst() + " [<名称>] <查询地址:查询端口>"));
        topic.child(HelpTopics.opDel(base,
                "用法1（按编号）：\n" + mcCmd + " " + base.del().getFirst() + " <编号> ...\n\n"
                        + "用法2（回复添加消息）：\n" + mcCmd + " " + base.del().getFirst()));
        topic.child(HelpTopics.opList(base,
                "查看已保存的服务器列表。\n\n"
                        + "用法：\n" + mcCmd + " " + base.list().getFirst() + " [<页数>] [<名称>]"));
        HelpTopic direct = HelpTopics.sub("直接查询", "无需保存配置，直接查询地址或已保存名称。", 4, ins.mcQuerySlash(),
                "用法1：\n" + slashCmd + " [<名称>] <查询地址> <查询端口>\n\n"
                        + "用法2：\n" + slashCmd + " [<名称>] <查询地址:查询端口>\n\n"
                        + "省略参数时查询当前群全部已保存服务器。\n"
                        + "单条默认图片，多条默认文本；可在参数任意位置加 " + textFlag + " / " + imgFlag + " 指定输出格式。\n\n"
                        + "示例：\n" + slashCmd + " " + textOutputFlags().getFirst() + " hypixel.net\n"
                        + slashCmd + " 我的服务器 " + imageOutputFlags().getFirst());
        topic.child(direct);
        topics.add(topic);
    }

    @Override
    public void registerAiCapabilities(@Nonnull List<AiCapability> capabilities, Long groupId) {
        capabilities.add(new AiCapability()
                .name("query_mc_server")
                .description("查询 Minecraft 服务器在线状态与玩家列表。")
                .parameterHint("host=地址,port=端口(默认25565),name=显示名称(可选)")
                .parameters(List.of(
                        AiCapabilityParameter.required("host", "服务器地址"),
                        AiCapabilityParameter.optional("port", "端口，默认 25565"),
                        AiCapabilityParameter.optional("name", "显示名称")
                ))
                .executor((ctx, args) -> {
                    try {
                        String host = AiCapabilityArgs.require(args, "host");
                        int port = AiCapabilityArgs.parseInt(args, "port", 25565);
                        return mcQueryService.queryServer(args.get("name"), host, port);
                    } catch (IllegalArgumentException e) {
                        return e.getMessage();
                    }
                }));
        capabilities.add(new AiCapability()
                .name("query_mc_server_by_name")
                .description("按已保存的名称查询 MC 服务器。")
                .parameterHint("name=服务器名称")
                .parameters(List.of(
                        AiCapabilityParameter.required("name", "已保存的服务器名称")
                ))
                .executor((ctx, args) -> {
                    String name = args.get("name");
                    if (name == null || name.isBlank()) {
                        return "缺少参数 name";
                    }
                    return mcQueryService.querySavedByName(ctx.botId(), ctx.scopeGroupId(), name);
                }));
        capabilities.add(new AiCapability()
                .name("list_mc_servers")
                .description("列出当前群已保存的 MC 服务器。")
                .parameterHint("无参数")
                .executor((ctx, args) -> mcQueryService.listSavedServers(ctx.botId(), ctx.scopeGroupId())));
    }

    @AnyMessageHandler
    public boolean query(BaniraBot bot, AnyMessageEvent event) {
        String message = event.getMessage();
        String matchedSlash = matchSlashQuery(message);
        if (matchedSlash == null) return false;

        CommandExtendedArgs extendedArgs = parseCommandExtendedArgs(stripCommandOnly(message, matchedSlash));
        CommandOutputMode mode = extendedArgs.outputMode();
        String queryBody = extendedArgs.body();
        List<MinecraftRecord> records = extractMinecraftRecords(queryBody, event);

        if (CollectionUtils.isNullOrEmpty(records)) return false;

        boolean useImage = resolveUseImage(mode, records.size());
        bot.setMsgEmojiLikeOk(event.getMessageId());

        if (useImage) {
            return sendImageResults(bot, event, records);
        }
        return sendTextResults(bot, event, records);
    }

    @AnyMessageHandler
    public boolean config(BaniraBot bot, AnyMessageEvent event) {
        BaniraCodeContext context = new BaniraCodeContext(bot, event);
        if (super.isCommand(context)
                && insConfig.get().mcQuery().stream().anyMatch(ins -> super.deleteCommandPrefix(context).startsWith(ins + " "))
        ) {
            String[] split = super.deleteCommandPrefix(context).split("\\s+");
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
                            record.setGroupId(0L);
                            this.modifyMinecraftRecord(bot, event, record, loginInfoEx, forwardMsg);
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

    // region 直接查询

    private String matchSlashQuery(String message) {
        if (StringUtils.isNullOrEmptyEx(message)) return null;
        for (String ins : insConfig.get().mcQuerySlash()) {
            String prefix = "/" + ins;
            if (message.equals(prefix) || message.startsWith(prefix + " ")) {
                return ins;
            }
        }
        return null;
    }

    private String stripCommandOnly(String message, String matchedSlash) {
        String prefix = "/" + matchedSlash;
        if (message.equals(prefix)) return "";
        return message.substring(prefix.length()).trim();
    }

    private boolean resolveUseImage(CommandOutputMode mode, int recordCount) {
        return switch (mode) {
            case TEXT -> false;
            case IMAGE -> true;
            case AUTO -> recordCount == 1;
        };
    }

    private boolean sendTextResults(BaniraBot bot, AnyMessageEvent event, List<MinecraftRecord> records) {
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

    private boolean sendImageResults(BaniraBot bot, AnyMessageEvent event, List<MinecraftRecord> records) {
        if (records.size() == 1) {
            String msg = getQueryInfoImg(records.getFirst());
            if (StringUtils.isNullOrEmptyEx(msg)) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            ActionData<MsgId> msgIdData = bot.sendMsg(event, msg, false);
            return bot.isActionDataMsgIdNotEmpty(msgIdData);
        }
        LoginInfoResp loginInfoEx = bot.getLoginInfoEx();
        List<Map<String, Object>> forwardMsg = new ArrayList<>();
        forwardMsg.add(ShiroUtils.generateSingleMsg(event.getUserId(), event.getSender().getNickname(), event.getMessage()));
        for (MinecraftRecord record : records) {
            forwardMsg.add(ShiroUtils.generateSingleMsg(loginInfoEx.getUserId(), loginInfoEx.getNickname()
                    , McQueryCode.getQueryInfo(record.getName(), record.getQueryIp(), record.getQueryPort())
            ));
            String img = getQueryInfoImg(record);
            if (!StringUtils.isNullOrEmptyEx(img)) {
                forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname(), img));
            }
        }
        ActionData<MsgId> msgIdData = bot.sendForwardMsg(event, forwardMsg);
        return bot.isActionDataMsgIdNotEmpty(msgIdData);
    }

    // endregion 直接查询

    public List<MinecraftRecord> extractMinecraftRecords(String message, AnyMessageEvent event) {
        if (message == null) {
            return Collections.emptyList();
        }

        String trimmed = message.trim();
        if (trimmed.isEmpty()) {
            return fetchRecordsByName(null, event.getSelfId(), event.getGroupId());
        }

        String[] tokens = trimmed.split("\\s+");
        List<MinecraftRecord> records = new ArrayList<>();

        NetAddressUtils.NetAddress addr = NetAddressUtils.findAddressAndPort(tokens, 0);
        if (addr != null) {
            String ip = addr.host();
            int port = addr.port();
            String name = (addr.index() > 0) ? joinTokens(tokens, 0, addr.index()) : "Minecraft Server";

            records.add(new MinecraftRecord()
                    .setBotId(event.getSelfId())
                    .setGroupId(event.getGroupId())
                    .setName(name)
                    .setQueryIp(ip)
                    .setQueryPort(port)
            );
        } else {
            records.addAll(fetchRecordsByName(trimmed, event.getSelfId(), event.getGroupId()));
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

        JsonObject status = this.generateStatus(record);
        File renderFile = null;
        try {
            renderFile = HtmlScreenshotConfig.buildInlineConfigRenderFile(
                    HTML_FILE, JsonUtils.PRETTY_GSON.toJson(status));
            HtmlScreenshotResult render = HtmlScreenshotUtils.render(
                    new HtmlScreenshotConfig(renderFile)
                            .setContextOptions(new Browser.NewContextOptions()
                                    .setViewportSize(800, 380)
                            )
                            .setScreenshotOptions(new Page.ScreenshotOptions()
                                    .setFullPage(false)
                            )
                            .setClipSelector(CLIP_SELECTOR)
                            .setReadyExpression("window.__mcQueryReady === true")
                            .setReadyTimeout(10000)
            );
            return MsgUtils.builder()
                    .img(render.getByte())
                    .build();
        } catch (Exception e) {
            LOGGER.error("Failed to render html", e);
            return null;
        } finally {
            HtmlScreenshotConfig.deleteQuietly(renderFile);
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
            JsonUtils.setString(playerObject, "uuid", kv.getValue());
            JsonUtils.setString(playerObject, "uuidSimple", kv.getValue().replace("-", ""));
            players.add(playerObject);
        }
        result.add("onlinePlayers", players);

        return result;
    }

}
