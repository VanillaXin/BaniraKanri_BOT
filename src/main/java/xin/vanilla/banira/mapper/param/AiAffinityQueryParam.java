package xin.vanilla.banira.mapper.param;

import xin.vanilla.banira.domain.AiAffinity;
import xin.vanilla.banira.mapper.common.BaniraQueryParam;
import xin.vanilla.banira.util.lambda.LambdaUtils;

/**
 * AiAffinity 查询参数。
 */
@SuppressWarnings("unused")
public class AiAffinityQueryParam extends BaniraQueryParam {

    public static final String QUERY_BOT_ID = LambdaUtils.getFiledName(AiAffinity::getBotId);
    public static final String QUERY_GROUP_ID = LambdaUtils.getFiledName(AiAffinity::getGroupId);
    public static final String QUERY_USER_ID = LambdaUtils.getFiledName(AiAffinity::getUserId);

    public AiAffinityQueryParam() {
        super(false);
    }

    public AiAffinityQueryParam setBotId(Long botId) {
        return (AiAffinityQueryParam) addParam(QUERY_BOT_ID, botId);
    }

    public AiAffinityQueryParam setGroupId(Long groupId) {
        return (AiAffinityQueryParam) addParamAllowEmpty(QUERY_GROUP_ID, groupId);
    }

    public AiAffinityQueryParam setUserId(Long userId) {
        return (AiAffinityQueryParam) addParam(QUERY_USER_ID, userId);
    }

    @Override
    public AiAffinityQueryParam setLimit(long limit) {
        super.setLimit(limit);
        return this;
    }

}
