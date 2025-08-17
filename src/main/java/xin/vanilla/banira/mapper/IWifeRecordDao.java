package xin.vanilla.banira.mapper;

import org.apache.ibatis.annotations.Mapper;
import xin.vanilla.banira.domain.WifeRecord;
import xin.vanilla.banira.domain.WifeRecord;
import xin.vanilla.banira.mapper.common.IBaniraMapper;
import xin.vanilla.banira.mapper.param.WifeRecordQueryParam;

import java.util.List;

@Mapper
public interface IWifeRecordDao extends IBaniraMapper<WifeRecord, WifeRecordQueryParam> {
    /**
     * 根据 queryParam 条件，删除记录
     *
     * @param queryParam 查询对象
     */
    int deleteByParam(WifeRecordQueryParam queryParam);

    /**
     * 查询（根据 queryParam 条件）
     *
     * @param queryParam 表字段 map 对象
     */
    int selectCountByParam(WifeRecordQueryParam queryParam);

    /**
     * 查询（根据 queryParam 条件）
     *
     * @param queryParam 表字段 map 对象
     */
    List<WifeRecord> selectByParam(WifeRecordQueryParam queryParam);
}
