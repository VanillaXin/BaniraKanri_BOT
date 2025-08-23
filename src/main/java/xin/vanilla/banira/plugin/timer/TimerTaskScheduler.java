package xin.vanilla.banira.plugin.timer;

import jakarta.annotation.PreDestroy;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.quartz.*;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.domain.TimerRecord;
import xin.vanilla.banira.event.DatabaseInitializedEvent;
import xin.vanilla.banira.event.TimerChangedEvent;
import xin.vanilla.banira.mapper.param.TimerRecordQueryParam;
import xin.vanilla.banira.service.ITimerRecordManager;
import xin.vanilla.banira.util.CronUtils;

import java.util.List;

@Slf4j
@Component
public class TimerTaskScheduler {

    @Resource
    private Scheduler scheduler;
    @Resource
    private ITimerRecordManager timerRecordManager;
    @Resource
    private ITimerTaskExecutor timerTaskExecutor;

    @EventListener
    public void init(DatabaseInitializedEvent event) {
        this.loadTimers();
    }

    public void loadTimers() {
        try {
            LOGGER.info("Refreshing all Quartz timer tasks...");
            scheduler.clear();
            List<TimerRecord> enabled = timerRecordManager.getTimerRecordList(new TimerRecordQueryParam().setEnable(true));
            enabled.forEach(this::scheduleIfValid);
            LOGGER.info("Refreshed {} enabled timer tasks.", enabled.size());
        } catch (SchedulerException e) {
            LOGGER.error("Error refreshing timer tasks", e);
        }
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
     * 校验cron并调度
     */
    public void scheduleIfValid(TimerRecord timer) {
        if (timer == null || timer.getId() == null) return;
        String cron = timer.getCron();
        if (CronUtils.isValidCron(cron)) {
            schedule(timer);
        } else {
            LOGGER.warn("Invalid cron expression for timer id={} cron={}", timer.getId(), cron);
        }
    }

    private void schedule(TimerRecord timer) {
        try {
            JobKey jobKey = JobKey.jobKey("task-" + timer.getId(), "timer-tasks");

            if (scheduler.checkExists(jobKey)) {
                scheduler.deleteJob(jobKey);
            }

            if (!timer.getEnable()) return;

            JobDataMap dataMap = new JobDataMap();
            dataMap.put("timer", timer);
            dataMap.put("executor", timerTaskExecutor);

            JobDetail jobDetail = JobBuilder.newJob(TaskJob.class)
                    .withIdentity(jobKey)
                    .usingJobData(dataMap)
                    .build();

            CronScheduleBuilder cronSchedule = CronScheduleBuilder.cronSchedule(timer.getCron());
            Trigger trigger = TriggerBuilder.newTrigger()
                    .withIdentity("trigger-" + timer.getId(), "timer-triggers")
                    .withSchedule(cronSchedule)
                    .build();

            scheduler.scheduleJob(jobDetail, trigger);

            LOGGER.info("Scheduled task id={} cron={}", timer.getId(), timer.getCron());
        } catch (Exception e) {
            LOGGER.error("Failed to schedule task id={}", timer.getId(), e);
        }
    }

    /**
     * 取消任务
     */
    public void cancel(TimerRecord timer) {
        cancel(timer.getId());
    }

    /**
     * 取消任务
     */
    public void cancel(Long id) {
        try {
            JobKey jobKey = JobKey.jobKey("task-" + id, "timer-tasks");
            if (scheduler.checkExists(jobKey)) {
                scheduler.deleteJob(jobKey);
                LOGGER.info("Cancelled timer id={}", id);
            }
        } catch (SchedulerException e) {
            LOGGER.error("Error cancelling timer id={}", id, e);
        }
    }

    /**
     * 启用任务
     */
    public void enable(Long id) {
        TimerRecord record = timerRecordManager.getTimerRecord(id);
        if (record == null) return;
        record.setEnable(true);
        timerRecordManager.modifyTimerRecord(record);
        scheduleIfValid(record);
    }

    /**
     * 禁用任务
     */
    public void disable(Long id) {
        TimerRecord record = timerRecordManager.getTimerRecord(id);
        if (record == null) return;
        record.setEnable(false);
        timerRecordManager.modifyTimerRecord(record);
        cancel(id);
    }

    /**
     * 监听定时任务变化事件并刷新调度
     */
    @EventListener
    public void onTimerChanged(TimerChangedEvent event) {
        if (event.getOperate() == null) return;
        switch (event.getOperate()) {
            case ADD, UPDATE: {
                event.getCurrentRecord().forEach(record -> {
                    if (record.getEnable()) {
                        scheduleIfValid(record);
                    } else {
                        cancel(record.getId());
                    }
                });
            }
            break;
            case REMOVE: {
                event.getCurrentRecord().forEach(this::cancel);
            }
            break;
        }
    }

}
