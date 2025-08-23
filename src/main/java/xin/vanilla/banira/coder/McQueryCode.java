package xin.vanilla.banira.coder;

import com.google.gson.JsonObject;
import com.mikuac.shiro.common.utils.ShiroUtils;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.coder.common.BaniraCode;
import xin.vanilla.banira.coder.common.BaniraCoder;
import xin.vanilla.banira.config.entity.extended.McConfig;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.enums.EnumCodeType;
import xin.vanilla.banira.util.*;

import java.util.List;
import java.util.Random;
import java.util.Set;

import static xin.vanilla.banira.util.McQueryHelper.*;

/**
 * Minecraft 服务器查询
 */
@Component
public class McQueryCode implements BaniraCoder {

    private static final Random random = new Random();

    @Override
    public List<String> getExample() {
        return List.of(
                CODE_START + "mc" + VAL_SEPARATOR + ShiroUtils.getUserAvatar(123456789, 0) + CODE_END
                , CODE_START + "mcquery" + ARG_SEPARATOR
                        + "name" + VAL_SEPARATOR + "server" + ARG_SEPARATOR
                        + "ip" + VAL_SEPARATOR + "127.0.0.1" + ARG_SEPARATOR
                        + "port" + VAL_SEPARATOR + "25565" + CODE_END
        );
    }

    @Override
    public String getName() {
        return "MC服务器状态";
    }

    @Override
    public String getDesc() {
        return "查询MC服务器状态及玩家列表信息";
    }

    @Override
    public EnumCodeType getType() {
        return EnumCodeType.MSG;
    }

    private static final Set<String> types = BaniraUtils.mutableSetOf(
            "minecraft", "mcquery", "mcq", "mc"
    );

    @Override
    public boolean match(String msg) {
        return types.contains(msg);
    }

    @Override
    public BaniraCodeContext execute(BaniraCodeContext context, BaniraCode code, String placeholder) {
        if (notMatch(code)) return context;
        JsonObject data = code.getData();
        if (data == null) return fail(context, code, placeholder);
        String name = JsonUtils.getString(data, "name");
        String ip = JsonUtils.getString(data, "ip");
        String port = JsonUtils.getString(data, "port");
        if (StringUtils.isNullOrEmptyEx(ip)) ip = JsonUtils.getString(data, "value");
        if (StringUtils.isNullOrEmptyEx(ip)) return fail(context, code, placeholder);

        if (StringUtils.isNullOrEmptyEx(port) || StringUtils.toInt(port) == 0) port = "25565";
        String info = getQueryInfo(name, ip, StringUtils.toInt(port));
        if (StringUtils.isNullOrEmpty(info)) return fail(context, code, placeholder);
        return context.setMsg(context.getMsg().replace(placeholder, info));
    }

    public static String getQueryInfo(String name, String ip, Integer port) {
        if (port == null || port == 0) port = 25565;
        McQueryHelper mcQuery = McQueryHelper.create(name, ip + ":" + port);
        mcQuery.query();
        StringBuilder info = new StringBuilder();
        McConfig mcConfig = BaniraUtils.getOthersConfig().mcConfig();

        if (StringUtils.isNotNullOrEmpty(mcQuery.error())) {
            switch (mcQuery.error()) {
                case ERROR_MSG_CONNECT_FAILED:
                    if (CollectionUtils.isNullOrEmpty(mcConfig.getConnectFailed())) {
                        info.append(mcQuery.serverName())
                                .append(":").append(ERROR_MSG_CONNECT_FAILED);
                    } else {
                        String err = CollectionUtils.getRandomElement(mcConfig.getConnectFailed(), random);

                        if (err.contains("%s")) {
                            info.append(String.format(err, mcQuery.serverName()));
                        } else {
                            info.append(mcQuery.serverName()).append(":").append(err);
                        }
                    }
                    break;
                case ERROR_MSG_LOADING:
                    info.append(mcQuery.serverName())
                            .append(":").append(ERROR_MSG_LOADING);
                    break;
                case ERROR_MSG_UNKNOWN_HOST:
                    if (CollectionUtils.isNullOrEmpty(mcConfig.getUnknownHost())) {
                        info.append(mcQuery.serverName())
                                .append(":").append(ERROR_MSG_UNKNOWN_HOST);
                    } else {
                        String err = CollectionUtils.getRandomElement(mcConfig.getUnknownHost(), random);
                        if (err.contains("%s")) {
                            info.append(String.format(err, mcQuery.serverName()));
                        } else {
                            info.append(mcQuery.serverName()).append(":").append(err);
                        }
                    }
                    break;
                case ERROR_MSG_UNKNOWN_RESPONSE:
                    if (CollectionUtils.isNullOrEmpty(mcConfig.getUnknownResponse())) {
                        info.append(mcQuery.serverName())
                                .append(":").append(ERROR_MSG_UNKNOWN_RESPONSE);
                    } else {
                        String err = CollectionUtils.getRandomElement(mcConfig.getUnknownResponse(), random);
                        if (err.contains("%s")) {
                            info.append(String.format(err, mcQuery.serverName()));
                        } else {
                            info.append(mcQuery.serverName()).append(":").append(err);
                        }
                    }
                    break;
            }
        } else if (mcQuery.onlinePlayers() == 0) {
            String err = CollectionUtils.getRandomElement(mcConfig.getNone(), random);
            if (err.contains("%s")) {
                info.append(String.format(err, mcQuery.serverName()));
            } else {
                info.append(mcQuery.serverName()).append(":").append(err);
            }
        } else {
            String success = mcConfig.getSuccess();
            success = success.replace("[name]", mcQuery.serverName());
            success = success.replace("[motd]", mcQuery.description());
            success = success.replace("[host]", mcQuery.serverIp());
            success = success.replace("[port]", String.valueOf(mcQuery.serverPort()));
            success = success.replace("[version]", mcQuery.serverVersion());
            success = success.replace("[players]", mcQuery.playerListString());
            success = success.replace("[online]", String.valueOf(mcQuery.onlinePlayers()));
            success = success.replace("[max]", String.valueOf(mcQuery.maxPlayers()));
            info.append(success);
        }
        return info.toString();
    }
}
