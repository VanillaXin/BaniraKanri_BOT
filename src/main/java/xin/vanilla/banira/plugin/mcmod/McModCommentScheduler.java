package xin.vanilla.banira.plugin.mcmod;

import jakarta.annotation.PreDestroy;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.quartz.*;
import org.springframework.context.event.EventListener;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.extended.ModWatchInfo;
import xin.vanilla.banira.event.DatabaseInitializedEvent;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.mcmod.EnumCommentType;
import xin.vanilla.banira.util.mcmod.McModCommentRow;

import java.util.List;
import java.util.Map;
import java.util.Set;

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

    @Order(100)
    @EventListener
    public void init(DatabaseInitializedEvent event) {
        commentService.loadAllCaches();
        scheduleTask();
    }

    /**
     * 调度定时任务
     */
    public void scheduleTask() {
        try {
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
     * 计算检测间隔
     * 如果检测目标过多，动态增加检测间隔
     *
     * @return 检测间隔（秒）
     */
    private int calculateInterval() {
        Map<String, List<ModWatchInfo>> monitoredContainers = commentService.getAllMonitoredContainers();
        int containerCount = monitoredContainers.size();

        if (containerCount == 0) {
            return DEFAULT_INTERVAL_SECONDS;
        }

        // 如果容器数量超过10个，每增加10个容器，增加1分钟的检测间隔
        // 最小间隔为5分钟，最大间隔为30分钟
        int additionalInterval = (containerCount / 10) * INTERVAL_BASE_SECONDS;
        int interval = DEFAULT_INTERVAL_SECONDS + additionalInterval;

        // 限制在5分钟到30分钟之间
        interval = Math.max(DEFAULT_INTERVAL_SECONDS, Math.min(interval, 1800));

        return interval;
    }

    @PreDestroy
    public void shutdown() {
        try {
            LOGGER.info("Shutting down Quartz Scheduler...");
            scheduler.shutdown(true);
        } catch (SchedulerException e) {
            LOGGER.error("Error shutting down Quartz", e);
        }
    }

    /**
     * Quartz Job实现
     */
    public static class McModCommentJob implements Job {
        @Override
        public void execute(JobExecutionContext context) throws JobExecutionException {
            LOGGER.debug("Executing MCMod comment check task");
            JobDataMap dataMap = context.getMergedJobDataMap();
            McModCommentService commentService = (McModCommentService) dataMap.get("commentService");

            try {
                // 获取所有需要检测的容器
                Map<String, List<ModWatchInfo>> monitoredContainers = commentService.getAllMonitoredContainers();

                if (monitoredContainers.isEmpty()) {
                    return;
                }

                for (Map.Entry<String, List<ModWatchInfo>> entry : monitoredContainers.entrySet()) {
                    List<ModWatchInfo> watchInfos = entry.getValue();

                    if (watchInfos.isEmpty()) {
                        continue;
                    }

                    // 同一cacheKey的类型和容器ID应该相同
                    ModWatchInfo firstWatchInfo = watchInfos.getFirst();
                    EnumCommentType commentType = firstWatchInfo.commentType();
                    String containerId = firstWatchInfo.containerId();

                    try {
                        String cacheKey = McModCommentService.getCacheKey(commentType, containerId);
                        if (McModCommentService.COMMENT_CACHE.getOrDefault(cacheKey, Set.of()).isEmpty()) {
                            McModCommentService.loadCacheFromFile(cacheKey);
                        }

                        // 获取评论列表
                        Set<McModCommentRow> comments = McModCommentService.fetchComments(commentType, containerId);
                        if (CollectionUtils.isNullOrEmpty(comments)) {
                            continue;
                        }

                        // 检查是否有新评论
                        Set<McModCommentRow> result = McModCommentService.getNewComments(commentType, containerId, comments);

                        // 如果有新评论，发送到对应的群
                        if (!result.isEmpty()) {
                            for (McModCommentRow comment : result) {
                                for (ModWatchInfo watchInfo : watchInfos) {
                                    BaniraBot bot = BaniraUtils.getBot(watchInfo.botId());
                                    if (bot != null) {
                                        McModCommentService.sendCommentToGroup(bot, watchInfo.groupId(), commentType, containerId, comment);
                                        // 避免发送过快
                                        Thread.sleep(500);
                                    }
                                }
                            }
                        }
                    } catch (Exception e) {
                        LOGGER.error("Error checking comments for type {} container {}", commentType, containerId, e);
                    }
                }
            } catch (Exception e) {
                LOGGER.error("Error executing MCMod comment check job", e);
                throw new JobExecutionException("Error executing MCMod comment check job", e);
            }
        }
    }

}
