package xin.vanilla.banira.service.impl;

import jakarta.annotation.Resource;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import xin.vanilla.banira.domain.AiAffinity;
import xin.vanilla.banira.mapper.IAiAffinityDao;
import xin.vanilla.banira.mapper.param.AiAffinityQueryParam;
import xin.vanilla.banira.service.IAiAffinityManager;
import xin.vanilla.banira.util.DateUtils;

import java.util.Date;
import java.util.List;

@Transactional
@Service("aiAffinityManager")
public class AiAffinityManager implements IAiAffinityManager {

    @Resource
    private IAiAffinityDao aiAffinityDao;

    @Override
    public int getScore(long botId, long groupId, long userId, int initialScore) {
        AiAffinity affinity = getRecord(botId, groupId, userId);
        return affinity != null && affinity.getScore() != null ? affinity.getScore() : initialScore;
    }

    @Override
    public int adjustScore(long botId, long groupId, long userId, int initialScore, int minScore, int maxScore, int delta) {
        long now = DateUtils.getTimestamp(new Date());
        Integer score = aiAffinityDao.upsertAndReturnScore(
                botId, groupId, userId, initialScore, minScore, maxScore, delta, now);
        return score != null ? score : Math.clamp(initialScore + delta, minScore, maxScore);
    }

    private AiAffinity getRecord(long botId, long groupId, long userId) {
        List<AiAffinity> records = aiAffinityDao.selectByParam(new AiAffinityQueryParam()
                .setBotId(botId)
                .setGroupId(groupId)
                .setUserId(userId)
                .setLimit(1));
        return records.isEmpty() ? null : records.getFirst();
    }

}
