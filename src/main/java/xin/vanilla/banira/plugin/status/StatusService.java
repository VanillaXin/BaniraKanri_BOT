package xin.vanilla.banira.plugin.status;

import cn.hutool.system.oshi.CpuInfo;
import cn.hutool.system.oshi.OshiUtil;
import com.mikuac.shiro.dto.action.common.ActionList;
import com.mikuac.shiro.dto.action.response.FriendInfoResp;
import com.mikuac.shiro.dto.action.response.GroupInfoResp;
import com.mikuac.shiro.dto.action.response.LoginInfoResp;
import com.mikuac.shiro.dto.action.response.VersionInfoResp;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Service;
import oshi.hardware.CentralProcessor;
import oshi.hardware.GlobalMemory;
import oshi.software.os.OperatingSystem;
import xin.vanilla.banira.config.BaniraVersionInfo;
import xin.vanilla.banira.mapper.param.MessageRecordQueryParam;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.service.IMessageRecordManager;
import xin.vanilla.banira.util.DateUtils;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Date;

/**
 * 框架状态摘要，供插件指令与 AI 能力共用
 */
@Service
public class StatusService {

    private static final long AI_STATUS_COOLDOWN_MS = 60_000;

    @Resource
    private ApplicationContext applicationContext;
    @Resource
    private BaniraVersionInfo baniraVersionInfo;
    @Resource
    private IMessageRecordManager messageRecordManager;

    private volatile long lastAiStatusTime = 0;

    @Nonnull
    public String getStatusSummaryForAi(@Nonnull BaniraBot bot) {
        long now = System.currentTimeMillis();
        if (now - lastAiStatusTime < AI_STATUS_COOLDOWN_MS) {
            return "状态查询过于频繁，请稍后再试。";
        }
        lastAiStatusTime = now;
        return getStatusSummary(bot);
    }

    @Nonnull
    public String getStatusSummary(@Nonnull BaniraBot bot) {
        LoginInfoResp loginInfoEx = bot.getLoginInfoEx();
        Date now = new Date();
        Date startupDate = new Date(applicationContext.getStartupDate());
        VersionInfoResp version = bot.getVersionInfo().getData();

        CentralProcessor processorInfo = OshiUtil.getHardware().getProcessor();
        CpuInfo cpuInfo = OshiUtil.getCpuInfo();
        GlobalMemory memoryInfo = OshiUtil.getMemory();
        OperatingSystem osInfo = OshiUtil.getOs();

        MessageRecordQueryParam param = new MessageRecordQueryParam();
        param.setBotId(bot.getSelfId());
        param.setTimeByGt(DateUtils.getTimestamp(startupDate));
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

        double cpuUsed = BigDecimal.valueOf(cpuInfo.getUsed()).setScale(1, RoundingMode.HALF_UP).doubleValue();
        double memUsed = BigDecimal.valueOf(memoryInfo.getTotal() - memoryInfo.getAvailable())
                .multiply(BigDecimal.valueOf(100))
                .divide(BigDecimal.valueOf(memoryInfo.getTotal()), 1, RoundingMode.HALF_UP)
                .doubleValue();

        return """
                账号：%s
                版本：%s-%s | BaniraKanri v%s
                运行：%s | 系统：%s
                消息：收%s 发%s | 群%s 好友%s
                CPU：%.1f%% (%sC%sT) | 内存：%.1f%%
                """.formatted(
                loginInfoEx.getNickname(),
                version.getAppName(),
                version.getAppVersion(),
                baniraVersionInfo.getVersion(),
                DateUtils.formatDuration(DateUtils.dateOfTwo(startupDate, now)),
                DateUtils.formatDuration(DateUtils.dateOfTwo(new Date(osInfo.getSystemBootTime() * 1000L), now)),
                totalCount - sendCount,
                sendCount,
                groupCount,
                friendCount,
                cpuUsed,
                processorInfo.getPhysicalProcessorCount(),
                processorInfo.getLogicalProcessorCount(),
                memUsed
        ).trim();
    }

}
