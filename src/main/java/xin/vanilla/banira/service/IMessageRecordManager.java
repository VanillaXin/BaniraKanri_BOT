package xin.vanilla.banira.service;

import jakarta.annotation.Nonnull;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.domain.PageResult;
import xin.vanilla.banira.mapper.param.MessageRecordQueryParam;

import java.util.List;

@SuppressWarnings("unused")
public interface IMessageRecordManager {

    long addMessageRecord(MessageRecord record);

    MessageRecord getMessageRecord(long id);

    @Nonnull
    List<MessageRecord> getMessageRecordList(MessageRecordQueryParam param);

    PageResult<MessageRecord> getMessageRecordPagedList(MessageRecordQueryParam param);

    MessageRecord getMessageRecord(long groupId, int msgId);

}
