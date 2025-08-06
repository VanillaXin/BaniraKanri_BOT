package xin.vanilla.banira.service;

import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.mapper.param.MessageRecordQueryParam;

import java.util.List;

@SuppressWarnings("unused")
public interface IMessageRecordManager {

    long addMessageRecord(MessageRecord record);

    MessageRecord getMessageRecord(long id);

    List<MessageRecord> getMessageRecordList(MessageRecordQueryParam param);

}
