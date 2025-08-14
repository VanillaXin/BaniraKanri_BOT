package xin.vanilla.banira.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import jakarta.annotation.Nonnull;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.mapper.param.MessageRecordQueryParam;

import java.util.List;

@SuppressWarnings("unused")
public interface IMessageRecordManager {

    long addMessageRecord(MessageRecord record);

    MessageRecord getMessageRecord(long id);

    @Nonnull
    List<MessageRecord> getMessageRecordList(MessageRecordQueryParam param);

    IPage<MessageRecord> getMessageRecordPagedList(MessageRecordQueryParam param);

    @Nonnull
    List<MessageRecord> getMessageRecordLimitList(MessageRecordQueryParam param);

    MessageRecord getMessageRecord(long groupId, int msgId);

}
