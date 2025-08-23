package xin.vanilla.banira.service;

import jakarta.annotation.Nonnull;
import xin.vanilla.banira.domain.PageResult;
import xin.vanilla.banira.domain.TimerRecord;
import xin.vanilla.banira.mapper.param.TimerRecordQueryParam;

import java.util.List;

/**
 * 定时任务记录管理服务
 */
@SuppressWarnings("unused")
public interface ITimerRecordManager {

    long addTimerRecord(TimerRecord record);

    TimerRecord getTimerRecord(long id);

    @Nonnull
    List<TimerRecord> getTimerRecordList(TimerRecordQueryParam param);

    PageResult<TimerRecord> getTimerRecordPagedList(TimerRecordQueryParam param);

    int deleteTimerRecord(long id);

    int deleteTimerRecordList(TimerRecordQueryParam param);

    void modifyTimerRecord(TimerRecord record);

}
