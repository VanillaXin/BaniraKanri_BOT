package xin.vanilla.banira.service.impl;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.domain.PageResult;
import xin.vanilla.banira.enums.EnumMessageType;
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
        if (param == null) param = new MessageRecordQueryParam();
        List<MessageRecord> records = messageRecordDao.selectByParam(param);
        return CollectionUtils.isNotNullOrEmpty(records) ? records : new ArrayList<>();
    }

    @Override
    public PageResult<MessageRecord> getMessageRecordPagedList(MessageRecordQueryParam param) {
        if (param == null) param = new MessageRecordQueryParam();
        PageResult<MessageRecord> result = new PageResult<>(messageRecordDao.selectByParam(param));
        if (!result.isEmpty()) {
            result.setTotal(messageRecordDao.selectCountByParam(param));
        }
        return result;
    }

    @Override
    public long getMessageRecordCount(MessageRecordQueryParam param) {
        if (param == null) param = new MessageRecordQueryParam();
        return messageRecordDao.selectCountByParam(param);
    }

    @Override
    public MessageRecord getGroupMessageRecord(long groupId, int msgId) {
        MessageRecordQueryParam param = new MessageRecordQueryParam(true);
        param.setGroupId(groupId);
        param.setMsgId(String.valueOf(msgId));
        List<MessageRecord> messageRecords = messageRecordDao.selectByParam(param);
        return CollectionUtils.isNotNullOrEmpty(messageRecords) ? messageRecords.getFirst() : null;
    }

    @Override
    public MessageRecord getPrivateMessageRecord(long friendId, int msgId) {
        MessageRecordQueryParam param = new MessageRecordQueryParam(true);
        param.setSenderId(friendId);
        param.setMsgId(String.valueOf(msgId));
        param.setMsgType(EnumMessageType.FRIEND.name(), EnumMessageType.MEMBER.name(), EnumMessageType.STRANGER.name());
        List<MessageRecord> messageRecords = messageRecordDao.selectByParam(param);
        return CollectionUtils.isNotNullOrEmpty(messageRecords) ? messageRecords.getFirst() : null;
    }
}
