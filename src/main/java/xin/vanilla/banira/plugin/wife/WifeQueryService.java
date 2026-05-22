package xin.vanilla.banira.plugin.wife;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Service;
import xin.vanilla.banira.domain.WifeRecord;
import xin.vanilla.banira.mapper.param.WifeRecordQueryParam;
import xin.vanilla.banira.service.IWifeRecordManager;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.DateUtils;

import java.util.Date;
import java.util.List;

/**
 * 抽老婆记录查询，供插件指令与 AI 能力共用
 */
@Service
public class WifeQueryService {

    @Resource
    private IWifeRecordManager wifeRecordManager;

    @Nullable
    public WifeRecord getTodayRecord(long groupId, long senderId) {
        Date current = new Date();
        long theDayStart = DateUtils.toTheDayStart(current).getTime() / 1000;
        long theDayEnd = DateUtils.toTheDayEnd(current).getTime() / 1000;
        WifeRecordQueryParam param = new WifeRecordQueryParam();
        param.setGroupId(groupId);
        param.setSenderId(senderId);
        param.setTimeByRange(theDayStart, theDayEnd);
        List<WifeRecord> records = wifeRecordManager.getWifeRecordList(param);
        return CollectionUtils.isNotNullOrEmpty(records) ? records.getFirst() : null;
    }

    @Nonnull
    public String describeTodayWife(long groupId, long senderId) {
        WifeRecord record = getTodayRecord(groupId, senderId);
        if (record == null) {
            return "今天还没有抽老婆记录";
        }
        return "今天的老婆：" + record.getWifeNick() + "（" + record.getWifeName() + "，QQ " + record.getWifeId() + "）";
    }

}
