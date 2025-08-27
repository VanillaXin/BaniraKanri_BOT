package xin.vanilla.banira.plugin;

import cn.hutool.system.oshi.CpuInfo;
import cn.hutool.system.oshi.OshiUtil;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.microsoft.playwright.Browser;
import com.microsoft.playwright.Page;
import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.common.utils.ShiroUtils;
import com.mikuac.shiro.dto.action.common.ActionData;
import com.mikuac.shiro.dto.action.common.ActionList;
import com.mikuac.shiro.dto.action.common.MsgId;
import com.mikuac.shiro.dto.action.response.FriendInfoResp;
import com.mikuac.shiro.dto.action.response.GroupInfoResp;
import com.mikuac.shiro.dto.action.response.LoginInfoResp;
import com.mikuac.shiro.dto.action.response.VersionInfoResp;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import jakarta.annotation.Nonnull;
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
import xin.vanilla.banira.util.html.HtmlScreenshotConfig;
import xin.vanilla.banira.util.html.HtmlScreenshotResult;
import xin.vanilla.banira.util.html.HtmlScreenshotUtils;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.time.Duration;
import java.util.*;
import java.util.stream.Stream;

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

    private volatile long lastRenderTime = 0;
    private static final Random RANDOM = new Random();

    private static final File HTML_FILE = new File("config/status_plugin/index.html");
    private static final File CONFIG_FILE = new File("config/status_plugin/config.js");
    private static final File TEMP_BG_FILE = new File("config/status_plugin/temp.png");

    private static final Set<String> helpType = BaniraUtils.mutableSetOf(
            "status", "状态"
    );

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
        if (helpType.stream().anyMatch(s -> StringUtils.isNullOrEmptyEx(type) || s.equalsIgnoreCase(type))) {
            result.add("框架状态：\n" +
                    "获取框架及系统状态信息总览。\n\n" +
                    BaniraUtils.getInsPrefixWithSpace() +
                    insConfig.get().base().status()
            );
        }
        return result;
    }

    @AnyMessageHandler
    public boolean status(BaniraBot bot, AnyMessageEvent event) {
        String message = event.getMessage();
        if (super.isCommand(message)
                && insConfig.get().base().status() != null
                && insConfig.get().base().status().contains(super.replaceCommand(message))
        ) {
            // 限制访问
            if (System.currentTimeMillis() - lastRenderTime < 1000 * 60)
                return bot.setMsgEmojiLikeNo(event.getMessageId());
            else
                bot.setMsgEmojiLikeOk(event.getMessageId());

            try {
                if (!HTML_FILE.exists()) {
                    ResourceCopyUtils.copyResources("template/status_plugin", HTML_FILE.getParent());
                }
            } catch (Exception e) {
                LOGGER.error("Failed to copy resources", e);
                return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            }

            JsonObject status = this.generateStatus(bot);

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
                this.lastRenderTime = System.currentTimeMillis();
                HtmlScreenshotResult render = HtmlScreenshotUtils.render(
                        new HtmlScreenshotConfig(new File("config/status_plugin/index.html"))
                                .setContextOptions(new Browser.NewContextOptions()
                                        .setViewportSize(1000, 800)
                                        .setIsMobile(true)
                                )
                                .setScreenshotOptions(new Page.ScreenshotOptions()
                                        .setFullPage(true)
                                )
                                .setInterval(RANDOM.nextInt(450, 1750))
                );
                String msg = MsgUtils.builder()
                        .img(render.getByte())
                        .build();
                ActionData<MsgId> msgIdData = bot.sendMsg(event, msg, false);
                return bot.isActionDataMsgIdNotEmpty(msgIdData);
            } catch (Exception e) {
                this.lastRenderTime = 0;
                LOGGER.error("Failed to render html", e);
                return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            }
        }
        return false;
    }

    private JsonObject generateStatus(BaniraBot bot) {
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
        Date systemStartDate = new Date(osInfo.getSystemBootTime() * 1000L);

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

        // background
        JsonUtils.setString(statusObject, "background", getBgUrl());

        // bot
        {
            JsonObject botObject = new JsonObject();
            JsonUtils.setString(botObject, "title", loginInfoEx.getNickname());
            JsonUtils.setString(botObject, "avatar", ShiroUtils.getUserAvatar(bot.getSelfId(), 0));
            JsonUtils.setString(botObject, "status", String.format("%s-%s | BaniraKanri v%s_commit%s:%s"
                    , version.getAppName()
                    , version.getAppVersion()
                    , baniraVersionInfo.getVersion()
                    , baniraVersionInfo.gitCommitCount()
                    , baniraVersionInfo.gitCommitShortId()
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
            JsonUtils.setString(uptimeObject, "plugin",
                    DateUtils.formatDuration(duration)
            );
            JsonUtils.setString(uptimeObject, "system",
                    DateUtils.formatDuration(DateUtils.dateOfTwo(systemStartDate, now))
            );
            JsonUtils.setString(uptimeObject, "compilation", String.format("Compiled by %s | %s %s"
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
                JsonUtils.setString(ramObject, "total",
                        StorageUnitUtils.convert(BigDecimal.valueOf(memoryInfo.getTotal())
                                , StorageUnitUtils.BYTE, 2)
                );
                JsonUtils.setString(ramObject, "used",
                        StorageUnitUtils.convert(BigDecimal.valueOf(memoryInfo.getTotal() - memoryInfo.getAvailable())
                                , StorageUnitUtils.BYTE, 2)
                );
                JsonUtils.setString(ramObject, "remaining",
                        StorageUnitUtils.convert(BigDecimal.valueOf(memoryInfo.getAvailable())
                                , StorageUnitUtils.BYTE, 2)
                );

                JsonUtils.setJsonObject(resourcesObject, "ram", ramObject);
            }

            // swap
            {
                JsonObject swapObject = new JsonObject();

                JsonUtils.setDouble(swapObject, "percentage", BigDecimal.valueOf(swapInfo.getSwapUsed())
                        .multiply(BigDecimal.valueOf(100))
                        .divide(BigDecimal.valueOf(swapInfo.getSwapTotal()), 2, RoundingMode.HALF_UP)
                        .doubleValue()
                );
                JsonUtils.setString(swapObject, "total",
                        StorageUnitUtils.convert(BigDecimal.valueOf(swapInfo.getSwapTotal())
                                , StorageUnitUtils.BYTE, 2)
                );
                JsonUtils.setString(swapObject, "used",
                        StorageUnitUtils.convert(BigDecimal.valueOf(swapInfo.getSwapUsed())
                                , StorageUnitUtils.BYTE, 2));
                JsonUtils.setString(swapObject, "remaining",
                        StorageUnitUtils.convert(BigDecimal.valueOf(swapInfo.getSwapTotal() - swapInfo.getSwapUsed())
                                , StorageUnitUtils.BYTE, 2)
                );

                JsonUtils.setJsonObject(resourcesObject, "swap", swapObject);
            }

            JsonUtils.setJsonObject(statusObject, "resources", resourcesObject);
        }

        // disks
        try {
            JsonArray disksArray = new JsonArray();
            FileSystem fileSystem = FileSystems.getDefault();
            for (Path path : fileSystem.getRootDirectories()) {
                JsonObject diskObject = new JsonObject();
                try {
                    FileStore fileStore = FileSystems.getDefault().provider().getFileStore(path);

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
                } catch (Exception e) {
                    LOGGER.error("Failed to get disk info", e);
                    JsonUtils.setString(diskObject, "label", path.toString());
                    JsonUtils.setDouble(diskObject, "percentage", 0);
                    JsonUtils.setString(diskObject, "used", "0BIT");
                    JsonUtils.setString(diskObject, "total", "0BIT");
                }

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
                    , pluginList.stream().filter(this::isPluginEnabled).count()
            ));
            JsonUtils.setString(systemSpecsObject, "memory",
                    StorageUnitUtils.convert(BigDecimal.valueOf(Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory())
                            , StorageUnitUtils.BYTE, 2)
            );

            JsonUtils.setJsonObject(statusObject, "systemSpecs", systemSpecsObject);
        }

        return statusObject;
    }

    private String getBgUrl() {
        String url = BaniraUtils.getOthersConfig().statusBgUrl();
        try {
            if (StringUtils.isNotNullOrEmpty(url)) {
                // 判断是否链接
                if (url.startsWith("http://") || url.startsWith("https://")) {
                    return url;
                }
                File file = new File(url);
                if (file.exists()) {
                    // 判断是否文件夹
                    if (file.isDirectory()) {
                        List<Path> files = getLocalPicList(url);
                        if (CollectionUtils.isNotNullOrEmpty(files)) {
                            Files.copy(files.getFirst(), TEMP_BG_FILE.toPath(), StandardCopyOption.REPLACE_EXISTING);
                            return TEMP_BG_FILE.getPath();
                        }
                    }
                    // 判断是否文件
                    else if (file.isFile()) {
                        Files.copy(file.toPath(), TEMP_BG_FILE.toPath(), StandardCopyOption.REPLACE_EXISTING);
                        return TEMP_BG_FILE.getPath();
                    }
                }
            }
        } catch (Exception e) {
            LOGGER.error("Failed to get bg url", e);
        }
        return "bg.png";
    }

    /**
     * 遍历路径及子路径下所有文件
     */
    private List<Path> getLocalPicList(String path) {
        List<Path> files = new ArrayList<>();
        try (Stream<Path> paths = Files.walk(Paths.get(path))) {
            paths.filter(Files::isRegularFile).forEach(files::add);
        } catch (IOException e) {
            LOGGER.error("Failed to get local pic list", e);
        }
        Collections.shuffle(files);
        return files;
    }

    private boolean isPluginEnabled(String className) {
        if (RecorderPlugin.class.getName().equalsIgnoreCase(className)) return true;
        if (globalConfig.get() == null || globalConfig.get().pluginConfig().capability() == null
        ) {
            return false;
        }
        Integer cap = globalConfig.get().pluginConfig().capability().get(className);
        return cap != null && cap > 0;
    }
}
