package xin.vanilla.banira.mapper;

import org.apache.ibatis.annotations.Mapper;
import xin.vanilla.banira.domain.AiMemoryEmbedding;
import xin.vanilla.banira.mapper.common.IBaniraMapper;
import xin.vanilla.banira.mapper.param.AiMemoryEmbeddingQueryParam;

import java.util.List;

@Mapper
public interface IAiMemoryEmbeddingDao extends IBaniraMapper<AiMemoryEmbedding, AiMemoryEmbeddingQueryParam> {

    List<AiMemoryEmbedding> selectByParam(AiMemoryEmbeddingQueryParam param);

    int upsert(AiMemoryEmbedding embedding);
}
