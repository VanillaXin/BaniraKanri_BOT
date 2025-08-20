package xin.vanilla.banira.event;

import lombok.Getter;
import org.springframework.context.ApplicationEvent;
import xin.vanilla.banira.domain.KeywordRecord;
import xin.vanilla.banira.enums.EnumDataOperateType;

import java.util.List;

@Getter
public class KeywordChangedEvent extends ApplicationEvent {
    private final List<KeywordRecord> currentRecord;
    private final EnumDataOperateType operate;


    public KeywordChangedEvent(Object source, KeywordRecord currentRecord, EnumDataOperateType operate) {
        super(source);
        this.currentRecord = List.of(currentRecord);
        this.operate = operate;
    }

    public KeywordChangedEvent(Object source, List<KeywordRecord> currentRecord, EnumDataOperateType operate) {
        super(source);
        this.currentRecord = currentRecord;
        this.operate = operate;
    }

}
