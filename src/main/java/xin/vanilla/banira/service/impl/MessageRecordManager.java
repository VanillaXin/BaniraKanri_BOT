package xin.vanilla.banira.service.impl;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.mapper.IMessageRecordDao;
import xin.vanilla.banira.mapper.param.MessageRecordQueryParam;
import xin.vanilla.banira.service.IMessageRecordManager;
import xin.vanilla.banira.util.CollectionUtils;

import java.util.ArrayList;
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

    @Nonnull
    @Override
    public List<MessageRecord> getMessageRecordList(MessageRecordQueryParam param) {
        List<MessageRecord> records = getMessageRecordPagedList(param).getRecords();
        return CollectionUtils.isNotNullOrEmpty(records) ? records : new ArrayList<>();
    }

    @Override
    public IPage<MessageRecord> getMessageRecordPagedList(MessageRecordQueryParam param) {
        if (param == null) param = new MessageRecordQueryParam();
        return messageRecordDao.selectByParam(param, new Page<>(param.getStartIndex(), param.getPageSize()));
    }

    @Nonnull
    @Override
    public List<MessageRecord> getMessageRecordLimitList(MessageRecordQueryParam param) {
        if (param == null) param = new MessageRecordQueryParam();
        List<MessageRecord> records = messageRecordDao.selectByParamLimit(param);
        return CollectionUtils.isNotNullOrEmpty(records) ? records : new ArrayList<>();
    }

    @Override
    public MessageRecord getMessageRecord(long groupId, int msgId) {
        MessageRecordQueryParam param = new MessageRecordQueryParam();
        param.setGroupId(groupId);
        param.setMsgId(String.valueOf(msgId));
        IPage<MessageRecord> messageRecords = messageRecordDao.selectByParam(param, new Page<>(0, 1));
        return CollectionUtils.isNotNullOrEmpty(messageRecords.getRecords()) ? messageRecords.getRecords().getFirst() : null;
    }
}
