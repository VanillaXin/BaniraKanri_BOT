package xin.vanilla.banira.mapper;

import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import xin.vanilla.banira.domain.AiAffinity;
import xin.vanilla.banira.mapper.common.IBaniraMapper;
import xin.vanilla.banira.mapper.param.AiAffinityQueryParam;

import java.util.List;

@Mapper
public interface IAiAffinityDao extends IBaniraMapper<AiAffinity, AiAffinityQueryParam> {

    List<AiAffinity> selectByParam(AiAffinityQueryParam param);

    int updateScore(@Param("id") long id, @Param("score") int score, @Param("updatedAt") long updatedAt);

    Integer upsertAndReturnScore(@Param("botId") long botId,
                                 @Param("groupId") long groupId,
                                 @Param("userId") long userId,
                                 @Param("initialScore") int initialScore,
                                 @Param("minScore") int minScore,
                                 @Param("maxScore") int maxScore,
                                 @Param("delta") int delta,
                                 @Param("updatedAt") long updatedAt);

}
