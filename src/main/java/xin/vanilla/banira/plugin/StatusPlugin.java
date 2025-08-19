package xin.vanilla.banira.plugin;

import cn.hutool.system.oshi.CpuInfo;
import cn.hutool.system.oshi.OshiUtil;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.common.utils.ShiroUtils;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.action.common.ActionData;
import com.mikuac.shiro.dto.action.common.ActionList;
import com.mikuac.shiro.dto.action.common.MsgId;
import com.mikuac.shiro.dto.action.response.FriendInfoResp;
import com.mikuac.shiro.dto.action.response.GroupInfoResp;
import com.mikuac.shiro.dto.action.response.LoginInfoResp;
import com.mikuac.shiro.dto.action.response.VersionInfoResp;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;
import oshi.hardware.CentralProcessor;
import oshi.hardware.GlobalMemory;
import oshi.hardware.VirtualMemory;
import oshi.software.os.OperatingSystem;
import xin.vanilla.banira.config.BaniraVersionInfo;
import xin.vanilla.banira.mapper.param.MessageRecordQueryParam;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.service.IMessageRecordManager;
import xin.vanilla.banira.util.*;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileStore;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.time.Duration;
import java.util.Collection;
import java.util.Date;
import java.util.List;

/**
 * 框架状态
 */
@Slf4j
@Shiro
@Component
public class StatusPlugin extends BasePlugin {

    @Resource
    private ApplicationContext applicationContext;
    @Resource
    private BaniraVersionInfo baniraVersionInfo;
    @Resource
    private IMessageRecordManager messageRecordManager;

    private volatile long lastRender = 0;

    private static final File HTML_FILE = new File("config/status_plugin/index.html");
    private static final File CONFIG_FILE = new File("config/status_plugin/config.js");

    private JsonObject status;

    @AnyMessageHandler
    public boolean status(Bot tob, AnyMessageEvent event) {
        BaniraBot bot = new BaniraBot(tob);
        String message = event.getMessage();
        if (super.isCommand(message)
                && globalConfig.get().instConfig().base().status() != null
                && globalConfig.get().instConfig().base().status().contains(super.replaceCommand(message))
        ) {
            // 限制访问
            if (System.currentTimeMillis() - lastRender < 1000 * 60) return bot.setMsgEmojiLikeNo(event.getMessageId());

            try {
                if (!HTML_FILE.exists()) {
                    ResourceCopyUtils.copyResources("template/status_plugin", HTML_FILE.getParent());
                }
            } catch (Exception e) {
                LOGGER.error("Failed to copy resources", e);
                return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            }

            refreshStatus(bot);

            try {
                FileOutputStream fileOutputStream = new FileOutputStream(CONFIG_FILE);
                OutputStreamWriter outputStreamWriter = new OutputStreamWriter(fileOutputStream, StandardCharsets.UTF_8);
                outputStreamWriter.write("const configData = " + JsonUtils.PRETTY_GSON.toJson(status));
                outputStreamWriter.close();
            } catch (Exception e) {
                LOGGER.error("Failed to write config.json", e);
                return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            }

            try {
                lastRender = System.currentTimeMillis();

                String msg = MsgUtils.builder()
                        .img(HtmlToImageUtils.renderFileToBytes(new File("config/status_plugin/index.html")))
                        .build();
                ActionData<MsgId> msgIdData = bot.sendMsg(event, msg, false);
                return bot.isActionDataMsgIdNotEmpty(msgIdData);
            } catch (Exception e) {
                lastRender = 0;
                LOGGER.error("Failed to render html", e);
                return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            }
        }
        return false;
    }

    private void refreshStatus(BaniraBot bot) {
        LoginInfoResp loginInfoEx = bot.getLoginInfoEx();
        Date now = new Date();
        Date date = new Date(applicationContext.getStartupDate());
        Duration duration = DateUtils.dateOfTwo(date, now);

        VersionInfoResp version = bot.getVersionInfo().getData();

        CentralProcessor processorInfo = OshiUtil.getHardware().getProcessor();
        CpuInfo cpuInfo = OshiUtil.getCpuInfo();

        GlobalMemory memoryInfo = OshiUtil.getMemory();
        VirtualMemory swapInfo = memoryInfo.getVirtualMemory();

        OperatingSystem osInfo = OshiUtil.getOs();
        Date systemStartDate = new Date(now.getTime() - osInfo.getSystemBootTime());

        MessageRecordQueryParam param = new MessageRecordQueryParam();
        param.setBotId(bot.getSelfId());
        param.setTimeByGt(DateUtils.getTimestamp(date));
        long totalCount = messageRecordManager.getMessageRecordCount(param);
        param.setSenderId(bot.getSelfId());
        long sendCount = messageRecordManager.getMessageRecordCount(param);

        long groupCount = 0;
        ActionList<GroupInfoResp> groupList = bot.getGroupList();
        if (bot.isActionDataNotEmpty(groupList)) {
            groupCount = groupList.getData().size();
        }
        long friendCount = 0;
        ActionList<FriendInfoResp> friendList = bot.getFriendList();
        if (bot.isActionDataNotEmpty(friendList)) {
            friendCount = friendList.getData().size();
        }

        JsonObject statusObject = new JsonObject();

        // bot
        {
            JsonObject botObject = new JsonObject();
            JsonUtils.setString(botObject, "title", loginInfoEx.getNickname());
            JsonUtils.setString(botObject, "avatar", ShiroUtils.getUserAvatar(bot.getSelfId(), 0));
            JsonUtils.setString(botObject, "status", String.format("%s-%s"
                    , version.getAppName()
                    , version.getAppVersion()
            ));
            JsonUtils.setString(botObject, "activity", String.format("收%s | 发%s | 群%s | 好友%s"
                    , totalCount - sendCount
                    , sendCount
                    , groupCount
                    , friendCount
            ));
            JsonUtils.setJsonObject(statusObject, "bot", botObject);
        }

        // uptime
        {
            JsonObject uptimeObject = new JsonObject();
            JsonUtils.setString(uptimeObject, "plugin", String.format("BaniraKanri %s_commit%s:%s 已运行%s"
                    , baniraVersionInfo.getVersion()
                    , baniraVersionInfo.getGitCommitCount()
                    , baniraVersionInfo.getGitCommitShortId()
                    , DateUtils.formatDuration(duration)
            ));
            JsonUtils.setString(uptimeObject, "system", String.format("系统已运行%s"
                    , DateUtils.formatDuration(DateUtils.dateOfTwo(systemStartDate, now))
            ));
            JsonUtils.setString(uptimeObject, "compilation", String.format("%s | Compiled by %s | %s %s"
                    , DateUtils.toDateTimeString(date)
                    , JavaVersionUtils.getClassVersion(StatusPlugin.class)
                    , osInfo.getFamily()
                    , System.getProperty("os.arch")
            ));
            JsonUtils.setJsonObject(statusObject, "uptime", uptimeObject);
        }

        // resources
        {
            JsonObject resourcesObject = new JsonObject();

            // cpu
            {
                JsonObject cpuObject = new JsonObject();

                JsonUtils.setDouble(cpuObject, "percentage", BigDecimal.valueOf(cpuInfo.getUsed())
                        .setScale(2, RoundingMode.HALF_UP).doubleValue()
                );
                JsonUtils.setString(cpuObject, "specs", String.format("%sC%sT"
                        , processorInfo.getPhysicalProcessorCount()
                        , processorInfo.getLogicalProcessorCount()
                ));
                JsonUtils.setString(cpuObject, "maxSpeed", String.format("最大 %sGHz"
                        , new BigDecimal(processorInfo.getMaxFreq())
                                .divide(BigDecimal.valueOf(10_0000_0000), 2, RoundingMode.HALF_UP)
                ));

                JsonUtils.setJsonObject(resourcesObject, "cpu", cpuObject);
            }

            // ram
            {
                JsonObject ramObject = new JsonObject();

                JsonUtils.setDouble(ramObject, "percentage", BigDecimal.valueOf(memoryInfo.getTotal() - memoryInfo.getAvailable())
                        .multiply(BigDecimal.valueOf(100))
                        .divide(BigDecimal.valueOf(memoryInfo.getTotal()), 2, RoundingMode.HALF_UP)
                        .doubleValue()
                );
                JsonUtils.setString(ramObject, "total", String.format("总共 %s"
                        , StorageUnitUtils.convert(BigDecimal.valueOf(memoryInfo.getTotal())
                                , StorageUnitUtils.BYTE, 2)
                ));
                JsonUtils.setString(ramObject, "used", String.format("已用 %s"
                        , StorageUnitUtils.convert(BigDecimal.valueOf(memoryInfo.getTotal() - memoryInfo.getAvailable())
                                , StorageUnitUtils.BYTE, 2)
                ));
                JsonUtils.setString(ramObject, "remaining", String.format("剩余 %s"
                        , StorageUnitUtils.convert(BigDecimal.valueOf(memoryInfo.getAvailable())
                                , StorageUnitUtils.BYTE, 2)
                ));

                JsonUtils.setJsonObject(resourcesObject, "ram", ramObject);
            }

            // swap
            {
                JsonObject swapObject = new JsonObject();

                JsonUtils.setDouble(swapObject, "percentage", BigDecimal.valueOf(swapInfo.getSwapTotal() - swapInfo.getSwapUsed())
                        .multiply(BigDecimal.valueOf(100))
                        .divide(BigDecimal.valueOf(swapInfo.getSwapTotal()), 2, RoundingMode.HALF_UP)
                        .doubleValue()
                );
                JsonUtils.setString(swapObject, "total", String.format("总共 %s"
                        , StorageUnitUtils.convert(BigDecimal.valueOf(swapInfo.getSwapTotal())
                                , StorageUnitUtils.BYTE, 2)
                ));
                JsonUtils.setString(swapObject, "used", String.format("已用 %s"
                        , StorageUnitUtils.convert(BigDecimal.valueOf(swapInfo.getSwapUsed())
                                , StorageUnitUtils.BYTE, 2)
                ));
                JsonUtils.setString(swapObject, "remaining", String.format("剩余 %s"
                        , StorageUnitUtils.convert(BigDecimal.valueOf(swapInfo.getSwapTotal() - swapInfo.getSwapUsed())
                                , StorageUnitUtils.BYTE, 2)
                ));

                JsonUtils.setJsonObject(resourcesObject, "swap", swapObject);
            }

            JsonUtils.setJsonObject(statusObject, "resources", resourcesObject);
        }

        // disks
        try {
            JsonArray disksArray = new JsonArray();
            FileSystem fileSystem = FileSystems.getDefault();
            for (Path path : fileSystem.getRootDirectories()) {
                FileStore fileStore = FileSystems.getDefault().provider().getFileStore(path);
                JsonObject diskObject = new JsonObject();

                JsonUtils.setString(diskObject, "label", path.toString());
                JsonUtils.setDouble(diskObject, "percentage", BigDecimal.valueOf(fileStore.getTotalSpace())
                        .subtract(BigDecimal.valueOf(fileStore.getUsableSpace()))
                        .multiply(BigDecimal.valueOf(100))
                        .divide(BigDecimal.valueOf(fileStore.getTotalSpace()), 2, RoundingMode.HALF_UP)
                        .doubleValue()
                );
                JsonUtils.setString(diskObject, "used", StorageUnitUtils.convert(BigDecimal.valueOf(fileStore.getTotalSpace())
                                .subtract(BigDecimal.valueOf(fileStore.getUsableSpace()))
                        , StorageUnitUtils.BYTE, 2)
                );
                JsonUtils.setString(diskObject, "total",
                        StorageUnitUtils.convert(BigDecimal.valueOf(fileStore.getTotalSpace()), StorageUnitUtils.BYTE, 2)
                );

                disksArray.add(diskObject);
            }

            JsonUtils.setJsonArray(statusObject, "disks", disksArray);
        } catch (Exception e) {
            LOGGER.error("Fail to get disks", e);
        }

        // systemSpecs
        {
            JsonObject systemSpecsObject = new JsonObject();
            List<String> pluginList = bot.getAnnotationHandler().values().stream()
                    .flatMap(Collection::stream)
                    .map(context -> context.getType().getName())
                    .distinct().toList();

            JsonUtils.setString(systemSpecsObject, "os", String.format("%s %s %s"
                    , osInfo.getManufacturer()
                    , osInfo.getFamily()
                    , osInfo.getVersionInfo().toString()
            ));
            JsonUtils.setString(systemSpecsObject, "cpu", OshiUtil.getHardware().getProcessor().getProcessorIdentifier().getName());
            JsonUtils.setString(systemSpecsObject, "plugin", String.format("总计%s个 | 启用%s个"
                    , pluginList.size()
                    , pluginList.stream().filter(this::isEnabled).count()
            ));
            JsonUtils.setString(systemSpecsObject, "memory", String.format("已用 %s"
                    , StorageUnitUtils.convert(BigDecimal.valueOf(Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory())
                            , StorageUnitUtils.BYTE, 2)
            ));

            JsonUtils.setJsonObject(statusObject, "systemSpecs", systemSpecsObject);
        }

        this.status = statusObject;
    }

    private boolean isEnabled(String className) {
        if (RecorderPlugin.class.getName().equalsIgnoreCase(className)) return true;
        if (globalConfig.get() == null || globalConfig.get().baseConfig().capability() == null
        ) {
            return false;
        }
        Integer cap = globalConfig.get().baseConfig().capability().get(className);
        return cap != null && cap > 0;
    }
}
