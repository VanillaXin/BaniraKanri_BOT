package xin.vanilla.banira.mapper;

import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import xin.vanilla.banira.domain.AiMemory;
import xin.vanilla.banira.mapper.common.IBaniraMapper;
import xin.vanilla.banira.mapper.param.AiMemoryQueryParam;

import java.util.List;

@Mapper
public interface IAiMemoryDao extends IBaniraMapper<AiMemory, AiMemoryQueryParam> {

    List<AiMemory> selectByParam(AiMemoryQueryParam param);

    int selectCountByParam(AiMemoryQueryParam param);

    int updateLastUsedAt(@Param("id") long id, @Param("lastUsedAt") long lastUsedAt);

}
