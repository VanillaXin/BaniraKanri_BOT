package xin.vanilla.banira.service;

public interface IAiAffinityManager {

    int getScore(long botId, long groupId, long userId, int initialScore);

    int adjustScore(long botId, long groupId, long userId, int initialScore, int minScore, int maxScore, int delta);

}
