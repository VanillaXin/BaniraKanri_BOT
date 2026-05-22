package xin.vanilla.banira.service;

import jakarta.annotation.Nullable;
import xin.vanilla.banira.domain.AiGroupEngagement;

public interface IAiGroupEngagementManager {

    @Nullable
    AiGroupEngagement get(long botId, long groupId);

    int saveInterest(long botId, long groupId, int interest);

    int adjustInterest(long botId, long groupId, int delta);

    void clear(long botId, long groupId);
}
