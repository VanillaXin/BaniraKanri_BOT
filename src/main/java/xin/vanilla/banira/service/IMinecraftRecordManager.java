package xin.vanilla.banira.service;

import jakarta.annotation.Nonnull;
import xin.vanilla.banira.domain.MinecraftRecord;
import xin.vanilla.banira.domain.PageResult;
import xin.vanilla.banira.mapper.param.MinecraftRecordQueryParam;

import java.util.List;

/**
 * MC服务器记录管理服务
 */
@SuppressWarnings("unused")
public interface IMinecraftRecordManager {

    long addMinecraftRecord(MinecraftRecord record);

    MinecraftRecord getMinecraftRecord(long id);

    @Nonnull
    List<MinecraftRecord> getMinecraftRecordList(MinecraftRecordQueryParam param);

    PageResult<MinecraftRecord> getMinecraftRecordPagedList(MinecraftRecordQueryParam param);

    int deleteMinecraftRecord(long id);

    int deleteMinecraftRecordList(MinecraftRecordQueryParam param);

    void modifyMinecraftRecord(MinecraftRecord record);

}
