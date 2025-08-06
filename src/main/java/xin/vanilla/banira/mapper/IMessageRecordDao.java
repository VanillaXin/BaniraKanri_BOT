package xin.vanilla.banira.mapper;

import org.apache.ibatis.annotations.Mapper;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.mapper.param.MessageRecordQueryParam;

@Mapper
public interface IMessageRecordDao extends IBaniraMapper<MessageRecord, MessageRecordQueryParam> {
}
