package xin.vanilla.banira.plugin.mcquery;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Service;
import xin.vanilla.banira.coder.message.McQueryCode;
import xin.vanilla.banira.domain.MinecraftRecord;
import xin.vanilla.banira.mapper.param.MinecraftRecordQueryParam;
import xin.vanilla.banira.service.IMinecraftRecordManager;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.NetAddressUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.List;
import java.util.stream.Collectors;

/**
 * MC 服务器查询服务，供插件指令与 AI 能力共用
 */
@Service
public class McQueryService {

    @Resource
    private IMinecraftRecordManager minecraftRecordManager;

    @Nonnull
    public String queryServer(@Nullable String name, @Nonnull String host, int port) {
        String serverName = StringUtils.isNotNullOrEmpty(name) ? name : "Minecraft Server";
        return McQueryCode.getQueryInfo(serverName, host, port <= 0 ? 25565 : port);
    }

    @Nonnull
    public String querySavedByName(long botId, long groupId, @Nonnull String name) {
        List<MinecraftRecord> records = fetchRecordsByName(name, botId, groupId);
        if (CollectionUtils.isNullOrEmpty(records)) {
            return "未找到已保存的服务器：" + name;
        }
        return records.stream()
                .map(record -> McQueryCode.getQueryInfo(record.getName(), record.getQueryIp(), record.getQueryPort())
                        + "\n" + maintainerHint(record))
                .collect(Collectors.joining("\n\n"));
    }

    @Nonnull
    public String listSavedServers(long botId, long groupId) {
        List<MinecraftRecord> records = fetchRecordsByName(null, botId, groupId);
        if (CollectionUtils.isNullOrEmpty(records)) {
            return "当前群没有已保存的 MC 服务器。";
        }
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < records.size(); i++) {
            MinecraftRecord record = records.get(i);
            builder.append(i + 1).append(". ")
                    .append(record.getName()).append(" (")
                    .append(record.getQueryIp()).append(":").append(record.getQueryPort())
                    .append(")")
                    .append("，添加者 qq=").append(record.getCreatorId())
                    .append("\n");
        }
        return builder.toString().trim();
    }

    @Nonnull
    public String queryByAddressText(@Nonnull String addressText) {
        String trimmed = addressText.trim();
        if (trimmed.isEmpty()) {
            return "地址不能为空";
        }
        NetAddressUtils.NetAddress addr = NetAddressUtils.findAddressAndPort(trimmed.split("\\s+"), 0);
        if (addr == null) {
            return "无法解析服务器地址：" + trimmed;
        }
        String name = addr.index() > 0 ? joinTokens(trimmed.split("\\s+"), 0, addr.index()) : "Minecraft Server";
        return queryServer(name, addr.host(), addr.port());
    }

    @Nonnull
    private List<MinecraftRecord> fetchRecordsByName(@Nullable String name, long botId, long groupId) {
        MinecraftRecordQueryParam param = new MinecraftRecordQueryParam();
        param.setBotId(botId);
        param.setGroupId(groupId);
        param.setEnable(true);
        if (StringUtils.isNotNullOrEmpty(name)) {
            param.setName(name);
        }
        param.addOrderBy(MinecraftRecordQueryParam.ORDER_ID, true);
        return minecraftRecordManager.getMinecraftRecordList(param);
    }

    @Nonnull
    private static String joinTokens(@Nonnull String[] tokens, int start, int end) {
        StringBuilder builder = new StringBuilder();
        for (int i = start; i < end; i++) {
            if (!builder.isEmpty()) {
                builder.append(' ');
            }
            builder.append(tokens[i]);
        }
        return builder.toString();
    }

    @Nonnull
    private static String maintainerHint(@Nonnull MinecraftRecord record) {
        Long creatorId = record.getCreatorId();
        if (creatorId == null || creatorId <= 0) {
            return "服务器记录添加者未知；如果服务器异常，先让能处理服务器的人看。";
        }
        return "服务器记录添加者 qq=" + creatorId + "；如果服务器异常，优先让这个人看，不要默认找配置主人。";
    }

}
