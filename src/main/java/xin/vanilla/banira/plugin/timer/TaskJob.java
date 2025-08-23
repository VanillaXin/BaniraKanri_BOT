package xin.vanilla.banira.plugin.timer;

import org.quartz.Job;
import org.quartz.JobDataMap;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import xin.vanilla.banira.domain.TimerRecord;

public class TaskJob implements Job {

    @Override
    public void execute(JobExecutionContext context) throws JobExecutionException {
        JobDataMap map = context.getMergedJobDataMap();
        TimerRecord timer = (TimerRecord) map.get("timer");
        ITimerTaskExecutor executor = (ITimerTaskExecutor) map.get("executor");
        try {
            executor.execute(timer);
        } catch (Exception e) {
            throw new JobExecutionException("Error executing timer id=" + timer.getId(), e);
        }
    }
}
