package xin.vanilla.banira.mapper.param;

import xin.vanilla.banira.domain.AiMemory;
import xin.vanilla.banira.mapper.common.BaniraQueryParam;
import xin.vanilla.banira.util.lambda.LambdaUtils;

/**
 * AiMemory 查询参数
 */
@SuppressWarnings("unused")
public class AiMemoryQueryParam extends BaniraQueryParam {

    public static final String QUERY_BOT_ID = LambdaUtils.getFiledName(AiMemory::getBotId);
    public static final String QUERY_GROUP_ID = LambdaUtils.getFiledName(AiMemory::getGroupId);
    public static final String QUERY_USER_ID = LambdaUtils.getFiledName(AiMemory::getUserId);
    public static final String QUERY_CONTENT = LambdaUtils.getFiledName(AiMemory::getContent);
    public static final String QUERY_TAGS = LambdaUtils.getFiledName(AiMemory::getTags);

    public static final String ORDER_LAST_USED_AT = LambdaUtils.getFiledName(AiMemory::getLastUsedAt);
    public static final String ORDER_CREATED_AT = LambdaUtils.getFiledName(AiMemory::getCreatedAt);

    public AiMemoryQueryParam() {
        super(false);
    }

    public AiMemoryQueryParam setBotId(Long botId) {
        return (AiMemoryQueryParam) addParam(QUERY_BOT_ID, botId);
    }

    public AiMemoryQueryParam setGroupId(Long groupId) {
        return (AiMemoryQueryParam) addParam(QUERY_GROUP_ID, groupId);
    }

    public AiMemoryQueryParam setGroupIdInGlobal(Long groupId) {
        return (AiMemoryQueryParam) addParam("group_id_in_global", groupId);
    }

    public AiMemoryQueryParam setUserId(Long userId) {
        return (AiMemoryQueryParam) addParam(QUERY_USER_ID, userId);
    }

    public AiMemoryQueryParam setUserIdInGlobal(Long userId) {
        return (AiMemoryQueryParam) addParam("user_id_in_global", userId);
    }

    public AiMemoryQueryParam setContentLike(String keyword) {
        return (AiMemoryQueryParam) addParam(QUERY_CONTENT + "_like", keyword);
    }

    public AiMemoryQueryParam setTagsLike(String keyword) {
        return (AiMemoryQueryParam) addParam(QUERY_TAGS + "_like", keyword);
    }

    public AiMemoryQueryParam addOrderByLastUsedAt(boolean asc) {
        addOrderBy(ORDER_LAST_USED_AT, asc);
        return this;
    }

    @Override
    public AiMemoryQueryParam addOrderBy(String name, boolean asc) {
        super.addOrderBy(name, asc);
        return this;
    }

    @Override
    public AiMemoryQueryParam setLimit(long limit) {
        super.setLimit(limit);
        return this;
    }

}
