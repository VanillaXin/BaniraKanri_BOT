package xin.vanilla.banira.service;

import jakarta.annotation.Nonnull;
import xin.vanilla.banira.domain.AiMemory;
import xin.vanilla.banira.mapper.param.AiMemoryQueryParam;

import java.util.List;

public interface IAiMemoryManager {

    long addMemory(AiMemory memory);

    AiMemory getMemory(long id);

    boolean updateMemory(AiMemory memory);

    @Nonnull
    List<AiMemory> getMemoryList(AiMemoryQueryParam param);

    void touchMemory(long id, long lastUsedAt);

    boolean deleteMemory(long id);

    boolean existsSimilar(long botId, long groupId, long userId, String content);

}
