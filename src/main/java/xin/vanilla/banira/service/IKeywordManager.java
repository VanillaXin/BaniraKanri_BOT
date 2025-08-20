package xin.vanilla.banira.service;

import xin.vanilla.banira.domain.KeywordRecord;

/**
 * 关键词管理服务
 */
@SuppressWarnings("unused")
public interface IKeywordManager {

    KeywordRecord findMatchReply(String input, Long botId, Long groupId);

}
