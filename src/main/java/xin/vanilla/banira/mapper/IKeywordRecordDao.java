package xin.vanilla.banira.mapper;

import org.apache.ibatis.annotations.Mapper;
import xin.vanilla.banira.domain.KeywordRecord;
import xin.vanilla.banira.mapper.common.IBaniraMapper;
import xin.vanilla.banira.mapper.param.KeywordRecordQueryParam;

import java.util.List;

@Mapper
public interface IKeywordRecordDao extends IBaniraMapper<KeywordRecord, KeywordRecordQueryParam> {
    /**
     * 根据 queryParam 条件，删除记录
     *
     * @param queryParam 查询对象
     */
    int deleteByParam(KeywordRecordQueryParam queryParam);

    /**
     * 查询（根据 queryParam 条件）
     *
     * @param queryParam 表字段 map 对象
     */
    int selectCountByParam(KeywordRecordQueryParam queryParam);

    /**
     * 查询（根据 queryParam 条件）
     *
     * @param queryParam 表字段 map 对象
     */
    List<KeywordRecord> selectByParam(KeywordRecordQueryParam queryParam);
}
