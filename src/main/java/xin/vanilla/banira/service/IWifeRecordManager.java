package xin.vanilla.banira.service;

import jakarta.annotation.Nonnull;
import xin.vanilla.banira.domain.PageResult;
import xin.vanilla.banira.domain.WifeRecord;
import xin.vanilla.banira.mapper.param.WifeRecordQueryParam;

import java.util.List;

@SuppressWarnings("unused")
public interface IWifeRecordManager {

    long addWifeRecord(WifeRecord record);

    WifeRecord getWifeRecord(long id);

    @Nonnull
    List<WifeRecord> getWifeRecordList(WifeRecordQueryParam param);

    PageResult<WifeRecord> getWifeRecordPagedList(WifeRecordQueryParam param);

}
