package xin.vanilla.banira.service.impl;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import xin.vanilla.banira.domain.PageResult;
import xin.vanilla.banira.domain.WifeRecord;
import xin.vanilla.banira.mapper.IWifeRecordDao;
import xin.vanilla.banira.mapper.param.WifeRecordQueryParam;
import xin.vanilla.banira.service.IWifeRecordManager;
import xin.vanilla.banira.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;

@Transactional
@Service("wifeRecordManager")
public class WifeRecordManager implements IWifeRecordManager {

    @Resource
    private IWifeRecordDao wifeRecordDao;

    @Override
    public long addWifeRecord(WifeRecord record) {
        return wifeRecordDao.insert(record);
    }

    @Override
    public WifeRecord getWifeRecord(long id) {
        return wifeRecordDao.selectById(id);
    }

    @Nonnull
    @Override
    public List<WifeRecord> getWifeRecordList(WifeRecordQueryParam param) {
        if (param == null) param = new WifeRecordQueryParam();
        List<WifeRecord> records = wifeRecordDao.selectByParam(param);
        return CollectionUtils.isNotNullOrEmpty(records) ? records : new ArrayList<>();
    }

    @Override
    public PageResult<WifeRecord> getWifeRecordPagedList(WifeRecordQueryParam param) {
        if (param == null) param = new WifeRecordQueryParam();
        PageResult<WifeRecord> result = new PageResult<>(wifeRecordDao.selectByParam(param));
        if (!result.isEmpty()) {
            result.setTotal(wifeRecordDao.selectCountByParam(param));
        }
        return result;
    }

}
