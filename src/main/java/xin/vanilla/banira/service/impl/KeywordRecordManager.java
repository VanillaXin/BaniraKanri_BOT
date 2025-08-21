package xin.vanilla.banira.service.impl;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import xin.vanilla.banira.domain.KeywordRecord;
import xin.vanilla.banira.domain.PageResult;
import xin.vanilla.banira.enums.EnumDataOperateType;
import xin.vanilla.banira.event.KeywordChangedEvent;
import xin.vanilla.banira.mapper.IKeywordRecordDao;
import xin.vanilla.banira.mapper.param.KeywordRecordQueryParam;
import xin.vanilla.banira.service.IKeywordRecordManager;
import xin.vanilla.banira.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;

@Transactional
@Service("keywordRecordManager")
public class KeywordRecordManager implements IKeywordRecordManager {

    @Resource
    private IKeywordRecordDao keywordRecordDao;
    @Resource
    private ApplicationEventPublisher eventPublisher;

    @Override
    public long addKeywordRecord(KeywordRecord record) {
        return keywordRecordDao.insert(record);
    }

    @Override
    public KeywordRecord getKeywordRecord(long id) {
        return keywordRecordDao.selectById(id);
    }

    @Nonnull
    @Override
    public List<KeywordRecord> getKeywordRecordList(KeywordRecordQueryParam param) {
        if (param == null) param = new KeywordRecordQueryParam();
        List<KeywordRecord> records = keywordRecordDao.selectByParam(param);
        return CollectionUtils.isNotNullOrEmpty(records) ? records : new ArrayList<>();
    }

    @Override
    public PageResult<KeywordRecord> getKeywordRecordPagedList(KeywordRecordQueryParam param) {
        if (param == null) param = new KeywordRecordQueryParam();
        PageResult<KeywordRecord> result = new PageResult<>(keywordRecordDao.selectByParam(param));
        if (!result.isEmpty()) {
            result.setTotal(keywordRecordDao.selectCountByParam(param));
        }
        return result;
    }

    @Override
    public int deleteKeywordRecord(long id) {
        int result = 0;
        KeywordRecord record = keywordRecordDao.selectById(id);
        if (record != null) {
            result = keywordRecordDao.deleteById(id);
            eventPublisher.publishEvent(new KeywordChangedEvent(this, record, EnumDataOperateType.REMOVE));
        }
        return result;
    }

    @Override
    public int deleteKeywordRecordList(KeywordRecordQueryParam param) {
        if (param == null) param = new KeywordRecordQueryParam();
        int result = 0;
        List<KeywordRecord> records = keywordRecordDao.selectByParam(param);
        if (CollectionUtils.isNotNullOrEmpty(records)) {
            result = keywordRecordDao.deleteByParam(param);
            eventPublisher.publishEvent(new KeywordChangedEvent(this, records, EnumDataOperateType.REMOVE));
        }
        return result;
    }

    @Override
    public void modifyKeywordRecord(KeywordRecord record) {
        KeywordRecord oldRecord = keywordRecordDao.selectById(record.getId());
        if (oldRecord != null) {
            keywordRecordDao.updateById(record);
            eventPublisher.publishEvent(new KeywordChangedEvent(this, record, EnumDataOperateType.UPDATE));
        }
    }

}
