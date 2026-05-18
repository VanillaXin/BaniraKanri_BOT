package xin.vanilla.banira.service.impl;

import jakarta.annotation.Nonnull;
import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
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
import java.util.Objects;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

@Transactional
@Service("messageRecordManager")
public class MessageRecordManager implements IMessageRecordManager {
    private static final int BATCH_SIZE = 100;
    private static final long FLUSH_INTERVAL_MS = 200L;

    @Resource
    private IMessageRecordDao messageRecordDao;
    private final ConcurrentLinkedQueue<MessageRecord> pendingRecords = new ConcurrentLinkedQueue<>();
    private ScheduledExecutorService writerExecutor;

    @PostConstruct
    public void initAsyncWriter() {
        this.writerExecutor = Executors.newSingleThreadScheduledExecutor(r -> {
            Thread thread = new Thread(r, "message-record-writer");
            thread.setDaemon(true);
            return thread;
        });
        this.writerExecutor.scheduleWithFixedDelay(this::flushPendingRecords, FLUSH_INTERVAL_MS, FLUSH_INTERVAL_MS, TimeUnit.MILLISECONDS);
    }

    @Override
    public long addMessageRecord(MessageRecord record) {
        return messageRecordDao.insert(record);
    }

    @Override
    public void addMessageRecordAsync(MessageRecord record) {
        if (record == null) {
            return;
        }
        pendingRecords.offer(record);
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

    private void flushPendingRecords() {
        if (pendingRecords.isEmpty()) {
            return;
        }
        List<MessageRecord> batch = new ArrayList<>(BATCH_SIZE);
        for (int i = 0; i < BATCH_SIZE; i++) {
            MessageRecord record = pendingRecords.poll();
            if (record == null) {
                break;
            }
            batch.add(record);
        }
        if (batch.isEmpty()) {
            return;
        }
        for (MessageRecord record : batch) {
            if (Objects.nonNull(record)) {
                messageRecordDao.insert(record);
            }
        }
    }

    @PreDestroy
    public void shutdownAsyncWriter() {
        flushPendingRecords();
        if (writerExecutor == null) {
            return;
        }
        writerExecutor.shutdown();
        try {
            if (!writerExecutor.awaitTermination(3, TimeUnit.SECONDS)) {
                writerExecutor.shutdownNow();
            }
        } catch (InterruptedException e) {
            writerExecutor.shutdownNow();
            Thread.currentThread().interrupt();
        }
        while (!pendingRecords.isEmpty()) {
            flushPendingRecords();
        }
    }
}
