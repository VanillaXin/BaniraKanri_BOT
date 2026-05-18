package xin.vanilla.banira.start;

import jakarta.annotation.PreDestroy;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.springframework.stereotype.Component;

/**
 * Quartz 生命周期统一管理
 */
@Slf4j
@Component
public class QuartzLifecycleManager {

    @Resource
    private Scheduler scheduler;

    @PreDestroy
    public void shutdown() {
        if (scheduler == null) {
            return;
        }
        try {
            if (!scheduler.isShutdown()) {
                LOGGER.info("Shutting down Quartz Scheduler...");
                scheduler.shutdown(true);
            }
        } catch (SchedulerException e) {
            LOGGER.error("Error shutting down Quartz", e);
        }
    }
}
