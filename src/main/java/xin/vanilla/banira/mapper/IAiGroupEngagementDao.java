package xin.vanilla.banira.mapper;

import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import xin.vanilla.banira.domain.AiGroupEngagement;
import xin.vanilla.banira.mapper.common.IBaniraMapper;
import xin.vanilla.banira.mapper.param.AiGroupEngagementQueryParam;

import java.util.List;

@Mapper
public interface IAiGroupEngagementDao extends IBaniraMapper<AiGroupEngagement, AiGroupEngagementQueryParam> {

    List<AiGroupEngagement> selectByParam(AiGroupEngagementQueryParam param);

    int deleteByScope(@Param("botId") long botId, @Param("groupId") long groupId);

    Integer upsertInterest(@Param("botId") long botId,
                           @Param("groupId") long groupId,
                           @Param("interest") int interest,
                           @Param("updatedAt") long updatedAt);

    Integer adjustInterest(@Param("botId") long botId,
                           @Param("groupId") long groupId,
                           @Param("delta") int delta,
                           @Param("updatedAt") long updatedAt);
}
