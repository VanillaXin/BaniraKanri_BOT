package xin.vanilla.banira.service;

import jakarta.annotation.Nonnull;
import xin.vanilla.banira.domain.KeywordRecord;
import xin.vanilla.banira.domain.PageResult;
import xin.vanilla.banira.mapper.param.KeywordRecordQueryParam;

import java.util.List;

/**
 * 关键词记录管理服务
 */
@SuppressWarnings("unused")
public interface IKeywordRecordManager {

    long addKeywordRecord(KeywordRecord record);

    KeywordRecord getKeywordRecord(long id);

    @Nonnull
    List<KeywordRecord> getKeywordRecordList(KeywordRecordQueryParam param);

    PageResult<KeywordRecord> getKeywordRecordPagedList(KeywordRecordQueryParam param);

    void deleteKeywordRecord(long id);

    void deleteKeywordRecordList(KeywordRecordQueryParam param);

    void modifyKeywordRecord(KeywordRecord record);

}
