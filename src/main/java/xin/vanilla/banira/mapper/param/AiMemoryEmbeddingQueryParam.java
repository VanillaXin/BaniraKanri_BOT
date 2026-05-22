package xin.vanilla.banira.mapper.param;

import xin.vanilla.banira.mapper.common.BaniraQueryParam;

/**
 * AiMemoryEmbedding query params.
 */
@SuppressWarnings("unused")
public class AiMemoryEmbeddingQueryParam extends BaniraQueryParam {

    public static final String QUERY_BOT_ID = "bot_id";
    public static final String QUERY_MODEL_NAME = "model_name";

    public AiMemoryEmbeddingQueryParam() {
        super(false);
    }

    public AiMemoryEmbeddingQueryParam setBotId(Long botId) {
        return (AiMemoryEmbeddingQueryParam) addParam(QUERY_BOT_ID, botId);
    }

    public AiMemoryEmbeddingQueryParam setGroupIdInGlobal(Long groupId) {
        return (AiMemoryEmbeddingQueryParam) addParam("group_id_in_global", groupId);
    }

    public AiMemoryEmbeddingQueryParam setUserIdInGlobal(Long userId) {
        return (AiMemoryEmbeddingQueryParam) addParam("user_id_in_global", userId);
    }

    public AiMemoryEmbeddingQueryParam setModelName(String modelName) {
        return (AiMemoryEmbeddingQueryParam) addParam(QUERY_MODEL_NAME, modelName);
    }
}
