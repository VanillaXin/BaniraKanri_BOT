package xin.vanilla.banira.plugin.mcmodcomment;

import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.quartz.*;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.extended.ModWatchInfo;
import xin.vanilla.banira.event.DatabaseInitializedEvent;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.List;
import java.util.Map;

/**
 * MCMod评论监控定时任务调度器
 */
@Slf4j
@Component
public class McModCommentScheduler {

    @Resource
    private Scheduler scheduler;

    @Resource
    private McModCommentService commentService;

    private static final String JOB_GROUP = "mcmod-comment-jobs";
    private static final String TRIGGER_GROUP = "mcmod-comment-triggers";
    private static final String JOB_NAME = "mcmod-comment-check";

    /**
     * 默认检测间隔（秒）
     */
    private static final int DEFAULT_INTERVAL_SECONDS = 60 * 5;

    /**
     * 每个mod的检测间隔基数（秒）
     */
    private static final int INTERVAL_BASE_SECONDS = 60;

    @PostConstruct
    public void init() {
        // 首次启动时加载所有缓存
        commentService.loadAllCaches();
        scheduleTask();
    }

    @EventListener
    public void onDatabaseInitialized(DatabaseInitializedEvent event) {
        // 数据库初始化后加载缓存并重新调度任务
        commentService.loadAllCaches();
        rescheduleTask();
    }

    /**
     * 调度定时任务
     */
    public void scheduleTask() {
        try {
            // 删除旧任务
            JobKey jobKey = JobKey.jobKey(JOB_NAME, JOB_GROUP);
            if (scheduler.checkExists(jobKey)) {
                scheduler.deleteJob(jobKey);
            }

            // 计算检测间隔
            int interval = calculateInterval();

            // 创建任务
            JobDataMap dataMap = new JobDataMap();
            dataMap.put("commentService", commentService);

            JobDetail jobDetail = JobBuilder.newJob(McModCommentJob.class)
                    .withIdentity(jobKey)
                    .usingJobData(dataMap)
                    .build();

            // 创建触发器
            Trigger trigger = TriggerBuilder.newTrigger()
                    .withIdentity("trigger-" + JOB_NAME, TRIGGER_GROUP)
                    .withSchedule(SimpleScheduleBuilder.simpleSchedule()
                            .withIntervalInSeconds(interval)
                            .repeatForever())
                    .startNow()
                    .build();

            scheduler.scheduleJob(jobDetail, trigger);
            LOGGER.info("Scheduled MCMod comment check task with interval {} seconds", interval);
        } catch (Exception e) {
            LOGGER.error("Failed to schedule MCMod comment check task", e);
        }
    }

    /**
     * 重新调度任务
     */
    public void rescheduleTask() {
        try {
            JobKey jobKey = JobKey.jobKey(JOB_NAME, JOB_GROUP);
            TriggerKey triggerKey = TriggerKey.triggerKey("trigger-" + JOB_NAME, TRIGGER_GROUP);

            if (scheduler.checkExists(jobKey)) {
                // 计算新的检测间隔
                int interval = calculateInterval();

                // 创建新触发器，关联到现有的JobDetail
                Trigger trigger = TriggerBuilder.newTrigger()
                        .withIdentity(triggerKey)
                        .forJob(jobKey)
                        .withSchedule(SimpleScheduleBuilder.simpleSchedule()
                                .withIntervalInSeconds(interval)
                                .repeatForever())
                        .startNow()
                        .build();

                // 重新调度触发器
                scheduler.rescheduleJob(triggerKey, trigger);
                LOGGER.info("Rescheduled MCMod comment check task with interval {} seconds", interval);
            } else {
                scheduleTask();
            }
        } catch (Exception e) {
            LOGGER.error("Failed to reschedule MCMod comment check task", e);
        }
    }

    /**
     * 计算检测间隔
     * 如果检测目标过多，动态增加检测间隔
     *
     * @return 检测间隔（秒）
     */
    private int calculateInterval() {
        Map<String, List<ModWatchInfo>> monitoredMods = commentService.getAllMonitoredMods();
        int modCount = monitoredMods.size();

        if (modCount == 0) {
            return DEFAULT_INTERVAL_SECONDS;
        }

        // 如果mod数量超过10个，每增加10个mod，增加1分钟的检测间隔
        // 最小间隔为5分钟，最大间隔为30分钟
        int additionalInterval = (modCount / 10) * INTERVAL_BASE_SECONDS;
        int interval = DEFAULT_INTERVAL_SECONDS + additionalInterval;

        // 限制在5分钟到30分钟之间
        interval = Math.max(DEFAULT_INTERVAL_SECONDS, Math.min(interval, 1800));

        return interval;
    }

    @PreDestroy
    public void shutdown() {
        try {
            JobKey jobKey = JobKey.jobKey(JOB_NAME, JOB_GROUP);
            if (scheduler.checkExists(jobKey)) {
                scheduler.deleteJob(jobKey);
                LOGGER.info("Removed MCMod comment check task");
            }
        } catch (Exception e) {
            LOGGER.error("Error removing MCMod comment check task", e);
        }
    }

    /**
     * Quartz Job实现
     */
    public static class McModCommentJob implements Job {
        @Override
        public void execute(JobExecutionContext context) throws JobExecutionException {
            JobDataMap dataMap = context.getMergedJobDataMap();
            McModCommentService commentService = (McModCommentService) dataMap.get("commentService");

            try {
                // 获取所有需要检测的mod
                Map<String, List<ModWatchInfo>> monitoredMods = commentService.getAllMonitoredMods();

                if (monitoredMods.isEmpty()) {
                    return;
                }

                // 遍历每个mod
                for (Map.Entry<String, List<ModWatchInfo>> entry : monitoredMods.entrySet()) {
                    String modId = entry.getKey();
                    List<ModWatchInfo> watchInfos = entry.getValue();

                    try {
                        // 获取评论列表
                        List<CommentInfo> comments = commentService.fetchComments(modId);
                        if (comments == null || comments.isEmpty()) {
                            continue;
                        }

                        // 检查是否有新评论
                        McModCommentService.NewCommentsResult result = commentService.getNewComments(modId, comments);

                        // 如果有新评论且不是首次加载，发送到对应的群
                        // 首次加载时（缓存文件不存在）不发送通知
                        if (!result.newComments().isEmpty() && !result.isFirstLoad()) {
                            for (CommentInfo comment : result.newComments()) {
                                for (ModWatchInfo watchInfo : watchInfos) {
                                    BaniraBot bot = BaniraUtils.getBot(watchInfo.botId());
                                    if (bot != null) {
                                        commentService.sendCommentToGroup(bot, watchInfo.groupId(), modId, comment);
                                        // 避免发送过快
                                        Thread.sleep(500);
                                    }
                                }
                            }
                        } else if (result.isFirstLoad() && !result.newComments().isEmpty()) {
                            LOGGER.info("First load for mod {}, {} new comments found but not notified", modId, result.newComments().size());
                        }
                    } catch (Exception e) {
                        LOGGER.error("Error checking comments for mod {}", modId, e);
                    }
                }
            } catch (Exception e) {
                LOGGER.error("Error executing MCMod comment check job", e);
                throw new JobExecutionException("Error executing MCMod comment check job", e);
            }
        }
    }

}
