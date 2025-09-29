package xin.vanilla.banira.mapper.param;


import xin.vanilla.banira.domain.MinecraftRecord;
import xin.vanilla.banira.mapper.common.BaniraQueryParam;
import xin.vanilla.banira.util.lambda.LambdaUtils;

import java.util.Collection;

/**
 * MinecraftRecord 查询参数类
 */
@SuppressWarnings("unused")
public class MinecraftRecordQueryParam extends BaniraQueryParam {

    // region 查询常量

    public static final String QUERY_ID = LambdaUtils.getFiledName(MinecraftRecord::getId);
    public static final String QUERY_BOT_ID = LambdaUtils.getFiledName(MinecraftRecord::getBotId);
    public static final String QUERY_GROUP_ID = LambdaUtils.getFiledName(MinecraftRecord::getGroupId);
    public static final String QUERY_CREATOR_ID = LambdaUtils.getFiledName(MinecraftRecord::getCreatorId);
    public static final String QUERY_TIME = LambdaUtils.getFiledName(MinecraftRecord::getTime);
    public static final String QUERY_NAME = LambdaUtils.getFiledName(MinecraftRecord::getName);
    public static final String QUERY_QUERY_IP = LambdaUtils.getFiledName(MinecraftRecord::getQueryIp);
    public static final String QUERY_QUERY_PORT = LambdaUtils.getFiledName(MinecraftRecord::getQueryPort);
    public static final String QUERY_RCON_IP = LambdaUtils.getFiledName(MinecraftRecord::getRconIp);
    public static final String QUERY_RCON_PORT = LambdaUtils.getFiledName(MinecraftRecord::getRconPort);
    public static final String QUERY_RCON_PSW = LambdaUtils.getFiledName(MinecraftRecord::getRconPsw);
    public static final String QUERY_ENABLE = LambdaUtils.getFiledName(MinecraftRecord::getEnable);

    // endregion

    // region 排序常量

    public static final String ORDER_ID = LambdaUtils.getFiledName(MinecraftRecord::getId);
    public static final String ORDER_BOT_ID = LambdaUtils.getFiledName(MinecraftRecord::getBotId);
    public static final String ORDER_GROUP_ID = LambdaUtils.getFiledName(MinecraftRecord::getGroupId);
    public static final String ORDER_CREATOR_ID = LambdaUtils.getFiledName(MinecraftRecord::getCreatorId);
    public static final String ORDER_TIME = LambdaUtils.getFiledName(MinecraftRecord::getTime);
    public static final String ORDER_NAME = LambdaUtils.getFiledName(MinecraftRecord::getName);
    public static final String ORDER_QUERY_IP = LambdaUtils.getFiledName(MinecraftRecord::getQueryIp);
    public static final String ORDER_QUERY_Port = LambdaUtils.getFiledName(MinecraftRecord::getQueryPort);
    public static final String ORDER_RCON_IP = LambdaUtils.getFiledName(MinecraftRecord::getRconIp);
    public static final String ORDER_RCON_PORT = LambdaUtils.getFiledName(MinecraftRecord::getRconPort);
    public static final String ORDER_RCON_PSW = LambdaUtils.getFiledName(MinecraftRecord::getRconPsw);
    public static final String ORDER_ENABLE = LambdaUtils.getFiledName(MinecraftRecord::getEnable);

    // endregion

    // region 构造函数

    public MinecraftRecordQueryParam() {
        super(false);
    }

    /**
     * @param all 是否查询所有字段
     */
    public MinecraftRecordQueryParam(boolean all) {
        super(all);
    }

    public MinecraftRecordQueryParam(long page, long pageSize) {
        super(page, pageSize);
    }

    public MinecraftRecordQueryParam(boolean all, long page, long pageSize) {
        super(all, page, pageSize);
    }

    public MinecraftRecordQueryParam(MinecraftRecord data) {
        if (data != null) {
            setId(data.getId());
            setBotId(data.getBotId());
            setGroupId(data.getGroupId());
            setCreatorId(data.getCreatorId());
            setTime(data.getTime());
            setName(data.getName());
            setQueryIp(data.getQueryIp());
            setQueryPort(data.getQueryPort());
            setRconIp(data.getRconIp());
            setRconPort(data.getRconPort());
            setRconPsw(data.getRconPsw());
        }
    }

    // endregion

    public MinecraftRecordQueryParam setId(Long id) {
        addParam(QUERY_ID, id);
        return this;
    }

    public MinecraftRecordQueryParam setId(Long... ids) {
        addParamByArray(QUERY_ID, ids);
        return this;
    }

    public MinecraftRecordQueryParam setId(Collection<Long> ids) {
        addParamByList(QUERY_ID, ids);
        return this;
    }

    public MinecraftRecordQueryParam setIdByLt(Long max) {
        addParamByLt(QUERY_ID, max);
        return this;
    }

    public MinecraftRecordQueryParam setIdByGt(Long min) {
        addParamByGt(QUERY_ID, min);
        return this;
    }

    public MinecraftRecordQueryParam setIdByRange(Long min, Long max) {
        addParamByRangeOpen(QUERY_ID, min, max);
        return this;
    }

    public MinecraftRecordQueryParam setBotId(Long botId) {
        addParam(QUERY_BOT_ID, botId);
        return this;
    }

    public MinecraftRecordQueryParam setBotId(Long... botIds) {
        addParamByArray(QUERY_BOT_ID, botIds);
        return this;
    }

    public MinecraftRecordQueryParam setBotIdByLt(Long max) {
        addParamByLt(QUERY_BOT_ID, max);
        return this;
    }

    public MinecraftRecordQueryParam setBotIdByGt(Long min) {
        addParamByGt(QUERY_BOT_ID, min);
        return this;
    }

    public MinecraftRecordQueryParam setBotIdByRange(Long min, Long max) {
        addParamByRangeOpen(QUERY_BOT_ID, min, max);
        return this;
    }

    public MinecraftRecordQueryParam setGroupId(Long groupId) {
        addParam(QUERY_GROUP_ID, groupId);
        return this;
    }

    public MinecraftRecordQueryParam setGroupId(Long... groupIds) {
        addParamByArray(QUERY_GROUP_ID, groupIds);
        return this;
    }

    public MinecraftRecordQueryParam setGroupIdByLt(Long max) {
        addParamByLt(QUERY_GROUP_ID, max);
        return this;
    }

    public MinecraftRecordQueryParam setGroupIdByGt(Long min) {
        addParamByGt(QUERY_GROUP_ID, min);
        return this;
    }

    public MinecraftRecordQueryParam setGroupIdByRange(Long min, Long max) {
        addParamByRangeOpen(QUERY_GROUP_ID, min, max);
        return this;
    }

    public MinecraftRecordQueryParam setCreatorId(Long creatorId) {
        addParam(QUERY_CREATOR_ID, creatorId);
        return this;
    }

    public MinecraftRecordQueryParam setCreatorId(Long... creatorIds) {
        addParamByArray(QUERY_CREATOR_ID, creatorIds);
        return this;
    }

    public MinecraftRecordQueryParam setCreatorIdByLt(Long max) {
        addParamByLt(QUERY_CREATOR_ID, max);
        return this;
    }

    public MinecraftRecordQueryParam setCreatorIdByGt(Long min) {
        addParamByGt(QUERY_CREATOR_ID, min);
        return this;
    }

    public MinecraftRecordQueryParam setCreatorIdByRange(Long min, Long max) {
        addParamByRangeOpen(QUERY_CREATOR_ID, min, max);
        return this;
    }

    public MinecraftRecordQueryParam setTime(Long time) {
        addParam(QUERY_TIME, time);
        return this;
    }

    public MinecraftRecordQueryParam setTime(Long... times) {
        addParamByArray(QUERY_TIME, times);
        return this;
    }

    public MinecraftRecordQueryParam setTimeByLt(Long max) {
        addParamByLt(QUERY_TIME, max);
        return this;
    }

    public MinecraftRecordQueryParam setTimeByGt(Long min) {
        addParamByGt(QUERY_TIME, min);
        return this;
    }

    public MinecraftRecordQueryParam setTimeByRange(Long min, Long max) {
        addParamByRangeOpen(QUERY_TIME, min, max);
        return this;
    }

    public MinecraftRecordQueryParam setName(String name) {
        addParam(QUERY_NAME, name);
        return this;
    }

    public MinecraftRecordQueryParam setNameAllowEmpty(String name) {
        put(QUERY_NAME, name);
        return this;
    }

    public MinecraftRecordQueryParam setName(String... names) {
        addParamByArray(QUERY_NAME, names);
        return this;
    }

    public MinecraftRecordQueryParam setQueryIp(String queryIp) {
        addParam(QUERY_QUERY_IP, queryIp);
        return this;
    }

    public MinecraftRecordQueryParam setQueryIpAllowEmpty(String queryIp) {
        put(QUERY_QUERY_IP, queryIp);
        return this;
    }

    public MinecraftRecordQueryParam setQueryIp(String... queryIps) {
        addParamByArray(QUERY_QUERY_IP, queryIps);
        return this;
    }

    public MinecraftRecordQueryParam setQueryPort(Integer queryPort) {
        addParam(QUERY_QUERY_PORT, queryPort);
        return this;
    }

    public MinecraftRecordQueryParam setQueryPort(Integer... queryPorts) {
        addParamByArray(QUERY_QUERY_PORT, queryPorts);
        return this;
    }

    public MinecraftRecordQueryParam setQueryPortByLt(Integer max) {
        addParamByLt(QUERY_QUERY_PORT, max);
        return this;
    }

    public MinecraftRecordQueryParam setQueryPortByGt(Integer min) {
        addParamByGt(QUERY_QUERY_PORT, min);
        return this;
    }

    public MinecraftRecordQueryParam setQueryPortByRange(Integer min, Integer max) {
        addParamByRangeOpen(QUERY_QUERY_PORT, min, max);
        return this;
    }

    public MinecraftRecordQueryParam setRconIp(String rconIp) {
        addParam(QUERY_RCON_IP, rconIp);
        return this;
    }

    public MinecraftRecordQueryParam setRconIpAllowEmpty(String rconIp) {
        put(QUERY_RCON_IP, rconIp);
        return this;
    }

    public MinecraftRecordQueryParam setRconIp(String... rconIps) {
        addParamByArray(QUERY_RCON_IP, rconIps);
        return this;
    }

    public MinecraftRecordQueryParam setRconPort(Integer rconPort) {
        addParam(QUERY_RCON_PORT, rconPort);
        return this;
    }

    public MinecraftRecordQueryParam setRconPort(Integer... rconPorts) {
        addParamByArray(QUERY_RCON_PORT, rconPorts);
        return this;
    }

    public MinecraftRecordQueryParam setRconPortByLt(Integer max) {
        addParamByLt(QUERY_RCON_PORT, max);
        return this;
    }

    public MinecraftRecordQueryParam setRconPortByGt(Integer min) {
        addParamByGt(QUERY_RCON_PORT, min);
        return this;
    }

    public MinecraftRecordQueryParam setRconPortByRange(Integer min, Integer max) {
        addParamByRangeOpen(QUERY_RCON_PORT, min, max);
        return this;
    }

    public MinecraftRecordQueryParam setRconPsw(String rconPsw) {
        addParam(QUERY_RCON_PSW, rconPsw);
        return this;
    }

    public MinecraftRecordQueryParam setRconPswAllowEmpty(String rconPsw) {
        put(QUERY_RCON_PSW, rconPsw);
        return this;
    }

    public MinecraftRecordQueryParam setRconPsw(String... rconPsws) {
        addParamByArray(QUERY_RCON_PSW, rconPsws);
        return this;
    }

    public MinecraftRecordQueryParam setEnable(Boolean enable) {
        addParam(QUERY_ENABLE, enable);
        return this;
    }

    public MinecraftRecordQueryParam setParamByMinecraft(String minecraft) {
        addParam(QUERY_KEY_WORD, minecraft);
        return this;
    }

    public MinecraftRecordQueryParam addKeyWord(String keyWord) {
        addParam(QUERY_KEY_WORD, keyWord);
        return this;
    }

    @Override
    public MinecraftRecordQueryParam addOrderBy(String name, boolean asc) {
        super.addOrderBy(name, asc);
        return this;
    }
}
