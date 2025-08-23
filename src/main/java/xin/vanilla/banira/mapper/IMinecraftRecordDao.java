package xin.vanilla.banira.mapper;

import org.apache.ibatis.annotations.Mapper;
import xin.vanilla.banira.domain.MinecraftRecord;
import xin.vanilla.banira.mapper.common.IBaniraMapper;
import xin.vanilla.banira.mapper.param.MinecraftRecordQueryParam;

import java.util.List;

@Mapper
public interface IMinecraftRecordDao extends IBaniraMapper<MinecraftRecord, MinecraftRecordQueryParam> {
    /**
     * 根据 queryParam 条件，删除记录
     *
     * @param queryParam 查询对象
     */
    int deleteByParam(MinecraftRecordQueryParam queryParam);

    /**
     * 查询（根据 queryParam 条件）
     *
     * @param queryParam 表字段 map 对象
     */
    int selectCountByParam(MinecraftRecordQueryParam queryParam);

    /**
     * 查询（根据 queryParam 条件）
     *
     * @param queryParam 表字段 map 对象
     */
    List<MinecraftRecord> selectByParam(MinecraftRecordQueryParam queryParam);
}
