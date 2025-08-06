package xin.vanilla.banira.service.impl;

import jakarta.annotation.Resource;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.mapper.IMessageRecordDao;
import xin.vanilla.banira.mapper.param.MessageRecordQueryParam;
import xin.vanilla.banira.service.IMessageRecordManager;

import java.util.List;

@Transactional
@Service("messageRecordManager")
public class MessageRecordManager implements IMessageRecordManager {

    @Resource
    private IMessageRecordDao messageRecordDao;

    @Override
    public long addMessageRecord(MessageRecord record) {
        return messageRecordDao.insert(record);
    }

    @Override
    public MessageRecord getMessageRecord(long id) {
        return messageRecordDao.selectById(id);
    }

    @Override
    public List<MessageRecord> getMessageRecordList(MessageRecordQueryParam param) {
        return messageRecordDao.selectByParam(param);
    }
}
