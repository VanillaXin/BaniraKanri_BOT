package xin.vanilla.banira.mapper.param;

import xin.vanilla.banira.domain.AiGroupEngagement;
import xin.vanilla.banira.mapper.common.BaniraQueryParam;
import xin.vanilla.banira.util.lambda.LambdaUtils;

@SuppressWarnings("unused")
public class AiGroupEngagementQueryParam extends BaniraQueryParam {

    public static final String QUERY_BOT_ID = LambdaUtils.getFiledName(AiGroupEngagement::getBotId);
    public static final String QUERY_GROUP_ID = LambdaUtils.getFiledName(AiGroupEngagement::getGroupId);

    public AiGroupEngagementQueryParam() {
        super(false);
    }

    public AiGroupEngagementQueryParam setBotId(Long botId) {
        return (AiGroupEngagementQueryParam) addParam(QUERY_BOT_ID, botId);
    }

    public AiGroupEngagementQueryParam setGroupId(Long groupId) {
        return (AiGroupEngagementQueryParam) addParamAllowEmpty(QUERY_GROUP_ID, groupId);
    }

    @Override
    public AiGroupEngagementQueryParam setLimit(long limit) {
        super.setLimit(limit);
        return this;
    }
}
