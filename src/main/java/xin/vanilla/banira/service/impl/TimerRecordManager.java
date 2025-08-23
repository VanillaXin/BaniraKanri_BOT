package xin.vanilla.banira.service.impl;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import org.apache.ibatis.executor.BatchResult;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import xin.vanilla.banira.domain.PageResult;
import xin.vanilla.banira.domain.TimerRecord;
import xin.vanilla.banira.enums.EnumDataOperateType;
import xin.vanilla.banira.event.TimerChangedEvent;
import xin.vanilla.banira.mapper.ITimerRecordDao;
import xin.vanilla.banira.mapper.param.TimerRecordQueryParam;
import xin.vanilla.banira.service.ITimerRecordManager;
import xin.vanilla.banira.util.CollectionUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@Transactional
@Service("timerRecordManager")
public class TimerRecordManager implements ITimerRecordManager {

    @Resource
    private ITimerRecordDao timerRecordDao;
    @Resource
    private ApplicationEventPublisher eventPublisher;

    @Override
    public long addTimerRecord(TimerRecord record) {
        int insert = timerRecordDao.insert(record);
        if (insert > 0) {
            eventPublisher.publishEvent(new TimerChangedEvent(this, record, EnumDataOperateType.ADD));
        }
        return insert;
    }

    @Override
    public TimerRecord getTimerRecord(long id) {
        return timerRecordDao.selectById(id);
    }

    @Nonnull
    @Override
    public List<TimerRecord> getTimerRecordList(TimerRecordQueryParam param) {
        if (param == null) param = new TimerRecordQueryParam().setEnable(true);
        List<TimerRecord> records = timerRecordDao.selectByParam(param);
        return CollectionUtils.isNotNullOrEmpty(records) ? records : new ArrayList<>();
    }

    @Override
    public PageResult<TimerRecord> getTimerRecordPagedList(TimerRecordQueryParam param) {
        if (param == null) param = new TimerRecordQueryParam().setEnable(true);
        PageResult<TimerRecord> result = new PageResult<>(timerRecordDao.selectByParam(param));
        if (!result.isEmpty()) {
            result.setTotal(timerRecordDao.selectCountByParam(param));
        }
        return result;
    }

    @Override
    public int deleteTimerRecord(long id) {
        int result = 0;
        TimerRecord record = timerRecordDao.selectById(id);
        if (record != null && record.getEnable()) {
            record.setEnable(false);
            result = timerRecordDao.updateById(record);
            eventPublisher.publishEvent(new TimerChangedEvent(this, record, EnumDataOperateType.REMOVE));
        }
        return result;
    }

    @Override
    public int deleteTimerRecordList(TimerRecordQueryParam param) {
        if (param == null) param = new TimerRecordQueryParam().setEnable(true);
        int result = 0;
        List<TimerRecord> records = timerRecordDao.selectByParam(param);
        if (CollectionUtils.isNotNullOrEmpty(records)) {
            List<TimerRecord> list = records.stream().filter(TimerRecord::getEnable).toList();
            list.forEach(record -> record.setEnable(false));
            List<BatchResult> batchResults = timerRecordDao.updateById(list);
            result = batchResults.stream()
                    .map(r -> Arrays.stream(r.getUpdateCounts()).reduce(0, Integer::sum))
                    .reduce(0, Integer::sum);
            eventPublisher.publishEvent(new TimerChangedEvent(this, list, EnumDataOperateType.REMOVE));
        }
        return result;
    }

    @Override
    public void modifyTimerRecord(TimerRecord record) {
        TimerRecord oldRecord = timerRecordDao.selectById(record.getId());
        if (oldRecord != null) {
            timerRecordDao.updateById(record);
            eventPublisher.publishEvent(new TimerChangedEvent(this, record, EnumDataOperateType.UPDATE));
        }
    }

}
