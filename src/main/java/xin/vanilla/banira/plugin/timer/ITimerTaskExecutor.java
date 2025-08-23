package xin.vanilla.banira.plugin.timer;

import xin.vanilla.banira.domain.TimerRecord;

public interface ITimerTaskExecutor {

    void execute(TimerRecord task);

}
