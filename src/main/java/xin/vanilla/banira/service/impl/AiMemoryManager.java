package xin.vanilla.banira.service.impl;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import xin.vanilla.banira.domain.AiMemory;
import xin.vanilla.banira.mapper.IAiMemoryDao;
import xin.vanilla.banira.mapper.param.AiMemoryQueryParam;
import xin.vanilla.banira.service.IAiMemoryManager;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.List;

@Transactional
@Service("aiMemoryManager")
public class AiMemoryManager implements IAiMemoryManager {

    @Resource
    private IAiMemoryDao aiMemoryDao;

    @Override
    public long addMemory(AiMemory memory) {
        return aiMemoryDao.insert(memory);
    }

    @Override
    public AiMemory getMemory(long id) {
        return aiMemoryDao.selectById(id);
    }

    @Nonnull
    @Override
    public List<AiMemory> getMemoryList(AiMemoryQueryParam param) {
        if (param == null) {
            param = new AiMemoryQueryParam();
        }
        List<AiMemory> memories = aiMemoryDao.selectByParam(param);
        return CollectionUtils.isNotNullOrEmpty(memories) ? memories : new ArrayList<>();
    }

    @Override
    public void touchMemory(long id, long lastUsedAt) {
        aiMemoryDao.updateLastUsedAt(id, lastUsedAt);
    }

    @Override
    public boolean existsSimilar(long botId, long groupId, long userId, String content) {
        if (StringUtils.isNullOrEmptyEx(content)) {
            return true;
        }
        AiMemoryQueryParam param = new AiMemoryQueryParam()
                .setBotId(botId)
                .setGroupIdInGlobal(groupId)
                .setUserIdInGlobal(userId)
                .setContentLike(content.trim())
                .setLimit(1);
        return !getMemoryList(param).isEmpty();
    }

}
