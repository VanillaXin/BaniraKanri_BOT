package xin.vanilla.banira.service.impl;

import jakarta.annotation.Resource;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import xin.vanilla.banira.mapper.IMessageRecordDao;
import xin.vanilla.banira.service.IMessageRecordManager;

@Transactional
@Service("messageRecordManager")
public class MessageRecordManager implements IMessageRecordManager {

    @Resource
    private IMessageRecordDao messageRecordDao;

}
