package xin.vanilla.banira.plugin.mcrcon;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Service;
import xin.vanilla.banira.config.entity.extended.McConfig;
import xin.vanilla.banira.config.entity.group.McQueryGroupConfig;
import xin.vanilla.banira.domain.MinecraftRecord;
import xin.vanilla.banira.mapper.param.MinecraftRecordQueryParam;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.service.IMinecraftRecordManager;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.StringUtils;
import xin.vanilla.rcon.Rcon;

import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;

/**
 * MC RCON 执行，供插件指令与 AI 能力共用
 */
@Service
public class McRconService {

    private static final int DEFAULT_RCON_PORT = 25575;

    @Resource
    private IMinecraftRecordManager minecraftRecordManager;

    @Nonnull
    public String listRconServers(@Nonnull BaniraBot bot, long botId, long groupId, long userId) {
        List<MinecraftRecord> records = minecraftRecordManager.getMinecraftRecordList(
                new MinecraftRecordQueryParam()
                        .setBotId(botId)
                        .setGroupId(0L, groupId)
                        .setEnable(true)
        );
        List<MinecraftRecord> configured = records.stream()
                .filter(this::hasRconConfig)
                .filter(record -> canViewRecord(bot, groupId, userId, record))
                .toList();
        if (configured.isEmpty()) {
            return "当前没有你有权查看的 RCON 服务器";
        }
        return configured.stream()
                .map(record -> "#" + record.getId() + " "
                        + record.getName() + " ("
                        + resolveRconHost(record) + ":" + resolveRconPort(record) + ")")
                .collect(Collectors.joining("\n"));
    }

    @Nonnull
    public String executeCommand(@Nonnull BaniraBot bot, long botId, long groupId, long userId
            , @Nonnull String key, @Nonnull String command, boolean confirmed
    ) {
        MinecraftRecord record = resolveRecord(bot, botId, groupId, userId, key);
        if (record == null) {
            return "未找到服务器：" + key;
        }
        if (!canExecute(userId, record)) {
            return "你没有该服务器的 RCON 执行权限";
        }
        if (!confirmed) {
            return "RCON 待确认：服务器 #" + record.getId() + " " + record.getName()
                    + "，命令：" + command.trim()
                    + "。请向用户说明风险并获明确同意后，再次调用 execute_rcon 并设置 confirm=true。";
        }
        return executeRcon(record, command);
    }

    @Nullable
    public MinecraftRecord resolveRecord(@Nullable BaniraBot bot, long botId, long groupId, long userId, @Nonnull String key) {
        long id = StringUtils.toLong(key, 0);
        if (id > 0) {
            MinecraftRecord record = minecraftRecordManager.getMinecraftRecord(id);
            if (record == null || !record.getEnable() || !Objects.equals(record.getBotId(), botId)) {
                return null;
            }
            if (!isRecordInScope(record, groupId) || !canViewRecord(bot, groupId, userId, record)) {
                return null;
            }
            return record;
        }
        List<MinecraftRecord> records = minecraftRecordManager.getMinecraftRecordList(
                new MinecraftRecordQueryParam()
                        .setBotId(botId)
                        .setGroupId(0L, groupId)
                        .setName(String.format("%%%s%%", key))
                        .setEnable(true)
        );
        List<MinecraftRecord> matched = records.stream()
                .filter(record -> canViewRecord(bot, groupId, userId, record))
                .toList();
        if (matched.size() == 1) {
            return matched.getFirst();
        }
        return null;
    }

    public boolean canViewRecord(@Nullable BaniraBot bot, long groupId, long userId, @Nonnull MinecraftRecord record) {
        if (Objects.equals(userId, record.getCreatorId())) {
            return true;
        }
        if (bot == null) {
            return BaniraUtils.isButler(userId);
        }
        return BaniraUtils.isGroupAdmin(bot, groupId, userId)
                || BaniraUtils.isGroupOwner(bot, groupId, userId)
                || BaniraUtils.isMaid(groupId, userId)
                || BaniraUtils.isButler(userId);
    }

    public boolean canExecute(long userId, @Nonnull MinecraftRecord record) {
        if (Objects.equals(userId, record.getCreatorId())) {
            return true;
        }
        return parseOperators(record).contains(userId);
    }

    private boolean isRecordInScope(@Nonnull MinecraftRecord record, long groupId) {
        if (record.getGroupId() == null || record.getGroupId() == 0L) {
            return true;
        }
        return Objects.equals(record.getGroupId(), groupId);
    }

    @Nonnull
    private String executeRcon(@Nonnull MinecraftRecord record, @Nonnull String command) {
        String host = resolveRconHost(record);
        int port = resolveRconPort(record);
        String password = record.getRconPsw();
        if (StringUtils.isNullOrEmptyEx(password)) {
            return "RCON 密码未设置，请私聊发送密码设置指令";
        }
        try (Rcon rcon = Rcon.open(host, port)) {
            rcon.tryAuthenticate(password);
            String response = rcon.sendCommand(command);
            return StringUtils.isNullOrEmptyEx(response) ? "(无返回内容)" : response.strip();
        } catch (IOException e) {
            return formatRconError(record, e);
        }
    }

    @Nonnull
    private String formatRconError(@Nonnull MinecraftRecord record, @Nonnull IOException e) {
        String msg = e.getMessage() == null ? "" : e.getMessage().toLowerCase(Locale.ROOT);
        McConfig mcConfig = BaniraUtils.getGroupConfigOrGlobal(McQueryGroupConfig.class, 0L).mcConfig();
        if (msg.contains("auth")) {
            if (CollectionUtils.isNotNullOrEmpty(mcConfig.pswError())) {
                return CollectionUtils.getRandomElement(mcConfig.pswError(), new Random())
                        .replace("%s", record.getName());
            }
            return record.getName() + " RCON 密码错误";
        }
        return record.getName() + " RCON 执行失败：" + e.getMessage();
    }

    private boolean hasRconConfig(@Nonnull MinecraftRecord record) {
        return StringUtils.isNotNullOrEmpty(resolveRconHost(record)) && resolveRconPort(record) > 0;
    }

    @Nonnull
    private static String resolveRconHost(@Nonnull MinecraftRecord record) {
        if (StringUtils.isNotNullOrEmpty(record.getRconIp())) {
            return record.getRconIp();
        }
        return record.getQueryIp();
    }

    private static int resolveRconPort(@Nonnull MinecraftRecord record) {
        if (record.getRconPort() > 0) {
            return record.getRconPort();
        }
        return DEFAULT_RCON_PORT;
    }

    @Nonnull
    private static Set<Long> parseOperators(@Nonnull MinecraftRecord record) {
        if (StringUtils.isNullOrEmptyEx(record.getRconOperators())) {
            return BaniraUtils.mutableSetOf();
        }
        Set<Long> result = BaniraUtils.mutableSetOf();
        for (String part : record.getRconOperators().split(",")) {
            long id = StringUtils.toLong(part.trim(), 0);
            if (id > 0) {
                result.add(id);
            }
        }
        return result;
    }

}
