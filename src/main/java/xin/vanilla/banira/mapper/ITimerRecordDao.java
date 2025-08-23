package xin.vanilla.banira.mapper;

import org.apache.ibatis.annotations.Mapper;
import xin.vanilla.banira.domain.TimerRecord;
import xin.vanilla.banira.mapper.common.IBaniraMapper;
import xin.vanilla.banira.mapper.param.TimerRecordQueryParam;

import java.util.List;

@Mapper
public interface ITimerRecordDao extends IBaniraMapper<TimerRecord, TimerRecordQueryParam> {
    /**
     * 根据 queryParam 条件，删除记录
     *
     * @param queryParam 查询对象
     */
    int deleteByParam(TimerRecordQueryParam queryParam);

    /**
     * 查询（根据 queryParam 条件）
     *
     * @param queryParam 表字段 map 对象
     */
    int selectCountByParam(TimerRecordQueryParam queryParam);

    /**
     * 查询（根据 queryParam 条件）
     *
     * @param queryParam 表字段 map 对象
     */
    List<TimerRecord> selectByParam(TimerRecordQueryParam queryParam);
}
