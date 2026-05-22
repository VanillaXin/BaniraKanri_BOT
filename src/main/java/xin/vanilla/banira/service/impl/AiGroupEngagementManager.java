package xin.vanilla.banira.service.impl;

import jakarta.annotation.Nullable;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import xin.vanilla.banira.domain.AiGroupEngagement;
import xin.vanilla.banira.mapper.IAiGroupEngagementDao;
import xin.vanilla.banira.mapper.param.AiGroupEngagementQueryParam;
import xin.vanilla.banira.service.IAiGroupEngagementManager;
import xin.vanilla.banira.util.DateUtils;

import java.util.Date;
import java.util.List;

@Transactional
@Service("aiGroupEngagementManager")
public class AiGroupEngagementManager implements IAiGroupEngagementManager {

    @Resource
    private IAiGroupEngagementDao aiGroupEngagementDao;

    @Override
    @Nullable
    public AiGroupEngagement get(long botId, long groupId) {
        List<AiGroupEngagement> records = aiGroupEngagementDao.selectByParam(new AiGroupEngagementQueryParam()
                .setBotId(botId)
                .setGroupId(groupId)
                .setLimit(1));
        return records.isEmpty() ? null : records.getFirst();
    }

    @Override
    public int saveInterest(long botId, long groupId, int interest) {
        long now = DateUtils.getTimestamp(new Date());
        Integer saved = aiGroupEngagementDao.upsertInterest(botId, groupId, interest, now);
        return saved != null ? saved : Math.clamp(interest, 0, 100);
    }

    @Override
    public int adjustInterest(long botId, long groupId, int delta) {
        long now = DateUtils.getTimestamp(new Date());
        Integer saved = aiGroupEngagementDao.adjustInterest(botId, groupId, delta, now);
        if (saved != null) {
            if (saved <= 0) {
                aiGroupEngagementDao.deleteByScope(botId, groupId);
                return 0;
            }
            return saved;
        }
        return Math.clamp(delta, 0, 100);
    }

    @Override
    public void clear(long botId, long groupId) {
        aiGroupEngagementDao.deleteByScope(botId, groupId);
    }
}
