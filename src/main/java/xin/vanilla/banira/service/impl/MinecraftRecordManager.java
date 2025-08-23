package xin.vanilla.banira.service.impl;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import org.apache.ibatis.executor.BatchResult;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import xin.vanilla.banira.domain.MinecraftRecord;
import xin.vanilla.banira.domain.PageResult;
import xin.vanilla.banira.mapper.IMinecraftRecordDao;
import xin.vanilla.banira.mapper.param.MinecraftRecordQueryParam;
import xin.vanilla.banira.service.IMinecraftRecordManager;
import xin.vanilla.banira.util.CollectionUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@Transactional
@Service("minecraftRecordManager")
public class MinecraftRecordManager implements IMinecraftRecordManager {

    @Resource
    private IMinecraftRecordDao minecraftRecordDao;
    @Resource
    private ApplicationEventPublisher eventPublisher;

    @Override
    public long addMinecraftRecord(MinecraftRecord record) {
        return minecraftRecordDao.insert(record);
    }

    @Override
    public MinecraftRecord getMinecraftRecord(long id) {
        return minecraftRecordDao.selectById(id);
    }

    @Nonnull
    @Override
    public List<MinecraftRecord> getMinecraftRecordList(MinecraftRecordQueryParam param) {
        if (param == null) param = new MinecraftRecordQueryParam().setEnable(true);
        List<MinecraftRecord> records = minecraftRecordDao.selectByParam(param);
        return CollectionUtils.isNotNullOrEmpty(records) ? records : new ArrayList<>();
    }

    @Override
    public PageResult<MinecraftRecord> getMinecraftRecordPagedList(MinecraftRecordQueryParam param) {
        if (param == null) param = new MinecraftRecordQueryParam().setEnable(true);
        PageResult<MinecraftRecord> result = new PageResult<>(minecraftRecordDao.selectByParam(param));
        if (!result.isEmpty()) {
            result.setTotal(minecraftRecordDao.selectCountByParam(param));
        }
        return result;
    }

    @Override
    public int deleteMinecraftRecord(long id) {
        int result = 0;
        MinecraftRecord record = minecraftRecordDao.selectById(id);
        if (record != null && record.getEnable()) {
            record.setEnable(false);
            result = minecraftRecordDao.updateById(record);
        }
        return result;
    }

    @Override
    public int deleteMinecraftRecordList(MinecraftRecordQueryParam param) {
        if (param == null) param = new MinecraftRecordQueryParam().setEnable(true);
        int result = 0;
        List<MinecraftRecord> records = minecraftRecordDao.selectByParam(param);
        if (CollectionUtils.isNotNullOrEmpty(records)) {
            List<MinecraftRecord> list = records.stream().filter(MinecraftRecord::getEnable).toList();
            list.forEach(record -> record.setEnable(false));
            List<BatchResult> batchResults = minecraftRecordDao.updateById(list);
            result = batchResults.stream()
                    .map(r -> Arrays.stream(r.getUpdateCounts()).reduce(0, Integer::sum))
                    .reduce(0, Integer::sum);
        }
        return result;
    }

    @Override
    public void modifyMinecraftRecord(MinecraftRecord record) {
        MinecraftRecord oldRecord = minecraftRecordDao.selectById(record.getId());
        if (oldRecord != null) {
            minecraftRecordDao.updateById(record);
        }
    }

}
