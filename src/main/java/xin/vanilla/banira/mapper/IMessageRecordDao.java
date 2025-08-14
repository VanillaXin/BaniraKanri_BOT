package xin.vanilla.banira.mapper;

import com.baomidou.mybatisplus.core.metadata.IPage;
import org.apache.ibatis.annotations.Mapper;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.mapper.common.IBaniraMapper;
import xin.vanilla.banira.mapper.param.MessageRecordQueryParam;

import java.util.List;

@Mapper
public interface IMessageRecordDao extends IBaniraMapper<MessageRecord, MessageRecordQueryParam> {
    /**
     * 根据 queryParam 条件，删除记录
     *
     * @param queryParam 查询对象
     */
    int deleteByParam(MessageRecordQueryParam queryParam);

    /**
     * 查询（根据 queryParam 条件）
     *
     * @param queryParam 表字段 map 对象
     */
    IPage<MessageRecord> selectByParam(MessageRecordQueryParam queryParam, IPage<MessageRecord> page);

    /**
     * 查询并精确控制条数（根据 queryParam 条件）
     *
     * @param queryParam 表字段 map 对象
     */
    List<MessageRecord> selectByParamLimit(MessageRecordQueryParam queryParam);

    /**
     * 查询（根据 queryParam 条件）
     *
     * @param queryParam 表字段 map 对象
     */
    int selectCountByParam(MessageRecordQueryParam queryParam);
}
