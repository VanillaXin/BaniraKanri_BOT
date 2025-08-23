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
import com.mikuac.shiro.dto.action.response.LoginInfoResp;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.coder.McQueryCode;
import xin.vanilla.banira.domain.MinecraftRecord;
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
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

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
        // TODO
        return List.of();
    }

    private static final Set<String> querys = BaniraUtils.mutableSetOf(
            "/list ", "/ls "
    );

    @AnyMessageHandler
    public boolean query(BaniraBot bot, AnyMessageEvent event) {
        String message = event.getMessage();
        if (querys.stream().anyMatch(message::startsWith)) {
            String[] split = message.split("\\s+");
            List<MinecraftRecord> records = new ArrayList<>();
            if (split.length < 2 || StringUtils.isNullOrEmptyEx(split[1])) {
                records.addAll(minecraftRecordManager.getMinecraftRecordList(new MinecraftRecordQueryParam()
                        .setBotId(event.getSelfId()).setGroupId(event.getGroupId()).setEnable(true)
                ));
            }
            //
            else if (split.length == 2) {
                String name = "Minecraft Server";
                String ip = "";
                String port = "25565";
                if (BaniraUtils.isValidNetAddress(split[1]) || split[1].contains(":")) {
                    String[] ipport = split[1].split(":");
                    ip = ipport[0];
                    if (StringUtils.toInt(ipport[1]) > 0) {
                        port = ipport[1];
                    } else {
                        port = "25565";
                    }
                } else {
                    name = split[1];
                }
                if (StringUtils.isNotNullOrEmpty(ip)) {
                    records.add(new MinecraftRecord()
                            .setBotId(event.getSelfId())
                            .setGroupId(event.getGroupId())
                            .setName(name)
                            .setQueryIp(ip)
                            .setQueryPort(StringUtils.toInt(port))
                    );
                } else {
                    records.addAll(minecraftRecordManager.getMinecraftRecordList(new MinecraftRecordQueryParam()
                            .setBotId(event.getSelfId()).setName(String.format("%%%s%%", name)).setGroupId(event.getGroupId()).setEnable(true)
                    ));
                }
            }
            //
            else if (split.length == 3) {
                String name = "Minecraft Server";
                String ip = "";
                String port = "25565";
                if (BaniraUtils.isValidNetAddress(split[1]) || split[1].contains(":")) {
                    String[] ipport = split[1].split(":");
                    ip = ipport[0];
                    if (StringUtils.toInt(ipport[1]) > 0) {
                        port = ipport[1];
                    } else if (StringUtils.toInt(split[2]) > 0) {
                        port = ip;
                    }
                } else if (BaniraUtils.isValidNetAddress(split[2]) || split[2].contains(":")) {
                    String[] ipport = split[2].split(":");
                    name = split[1];
                    ip = ipport[0];
                    if (StringUtils.toInt(ipport[1]) > 0) {
                        port = ipport[1];
                    } else {
                        port = "25565";
                    }
                } else {
                    name = message.substring(message.indexOf(split[1]));
                }
                if (StringUtils.isNotNullOrEmpty(ip)) {
                    records.add(new MinecraftRecord()
                            .setBotId(event.getSelfId())
                            .setGroupId(event.getGroupId())
                            .setName(name)
                            .setQueryIp(ip)
                            .setQueryPort(StringUtils.toInt(port))
                    );
                } else {
                    records.addAll(minecraftRecordManager.getMinecraftRecordList(new MinecraftRecordQueryParam()
                            .setBotId(event.getSelfId()).setName(String.format("%%%s%%", name)).setGroupId(event.getGroupId()).setEnable(true)
                    ));
                }
            }
            //
            else if (split.length == 4) {
                String name = split[1];
                String ip = split[2];
                String port = split[3];
                if (BaniraUtils.isValidNetAddress(ip) && StringUtils.toInt(port) > 0) {
                    records.add(new MinecraftRecord()
                            .setBotId(event.getSelfId())
                            .setGroupId(event.getGroupId())
                            .setName(name)
                            .setQueryIp(ip)
                            .setQueryPort(StringUtils.toInt(port))
                    );
                } else {
                    name = message.substring(message.indexOf(split[1]));
                    records.addAll(minecraftRecordManager.getMinecraftRecordList(new MinecraftRecordQueryParam()
                            .setBotId(event.getSelfId()).setName(String.format("%%%s%%", name)).setGroupId(event.getGroupId()).setEnable(true)
                    ));
                }
            }
            //
            else {
                String name = message.substring(message.indexOf(split[1]));
                records.addAll(minecraftRecordManager.getMinecraftRecordList(new MinecraftRecordQueryParam()
                        .setBotId(event.getSelfId()).setName(String.format("%%%s%%", name)).setGroupId(event.getGroupId()).setEnable(true)
                ));
            }
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

                    try {
                        if (!HTML_FILE.exists()) {
                            ResourceCopyUtils.copyResources("template/mc_query_plugin", HTML_FILE.getParent());
                        }
                    } catch (Exception e) {
                        LOGGER.error("Failed to copy resources", e);
                        return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
                    }

                    try {
                        JsonObject status = this.generateStatus(records.getFirst());
                        FileOutputStream fileOutputStream = new FileOutputStream(CONFIG_FILE);
                        OutputStreamWriter outputStreamWriter = new OutputStreamWriter(fileOutputStream, StandardCharsets.UTF_8);
                        outputStreamWriter.write("const configData = " + JsonUtils.PRETTY_GSON.toJson(status));
                        outputStreamWriter.close();
                    } catch (Exception e) {
                        LOGGER.error("Failed to write config.json", e);
                        return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
                    }

                    try {
                        HtmlScreenshotResult render = HtmlScreenshotUtils.render(
                                new HtmlScreenshotConfig(new File("config/mc_query_plugin/index.html"))
                                        .setContextOptions(new Browser.NewContextOptions()
                                                .setViewportSize(820, 500)
                                        )
                                        .setScreenshotOptions(new Page.ScreenshotOptions()
                                                .setFullPage(true)
                                        )
                        );
                        String msg = MsgUtils.builder()
                                .img(render.getByte())
                                .build();
                        ActionData<MsgId> msgIdData = bot.sendMsg(event, msg, false);
                        return bot.isActionDataMsgIdNotEmpty(msgIdData);
                    } catch (Exception e) {
                        LOGGER.error("Failed to render html", e);
                        return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
                    }
                }
            }
        }
        return false;
    }

    @AnyMessageHandler
    public boolean config(BaniraBot bot, AnyMessageEvent event) {
        String message = event.getMessage();
        // TODO 增删改查配置

        return false;
    }


    private JsonObject generateStatus(MinecraftRecord record) {
        McQueryHelper mcQuery = McQueryHelper.create(record.getName(), record.getQueryIp() + ":" + record.getQueryPort());
        mcQuery.query();

        JsonObject result = new JsonObject();

        JsonUtils.setString(result, "serverName", mcQuery.serverName());
        JsonUtils.setString(result, "serverIcon", JsonUtils.getString(mcQuery.getServerJson(), "favicon", ""));
        JsonUtils.setString(result, "serverAddress", mcQuery.serverAddress());
        JsonUtils.setString(result, "description", mcQuery.descriptionHtml());
        JsonUtils.setInt(result, "maxPlayers", mcQuery.maxPlayers());
        JsonUtils.setString(result, "version.name", JsonUtils.getString(mcQuery.getServerJson(), "version.name", ""));

        JsonArray players = new JsonArray();
        for (String s : mcQuery.playerList()) {
            JsonObject playerObject = new JsonObject();
            JsonUtils.setString(playerObject, "name", s);
            players.add(playerObject);
        }
        result.add("onlinePlayers", players);

        return result;
    }

}
