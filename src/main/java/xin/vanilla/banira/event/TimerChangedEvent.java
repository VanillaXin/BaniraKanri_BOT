package xin.vanilla.banira.event;

import lombok.Getter;
import org.springframework.context.ApplicationEvent;
import xin.vanilla.banira.domain.TimerRecord;
import xin.vanilla.banira.enums.EnumDataOperateType;

import java.util.List;

@Getter
public class TimerChangedEvent extends ApplicationEvent {
    private final List<TimerRecord> currentRecord;
    private final EnumDataOperateType operate;


    public TimerChangedEvent(Object source, TimerRecord currentRecord, EnumDataOperateType operate) {
        super(source);
        this.currentRecord = List.of(currentRecord);
        this.operate = operate;
    }

    public TimerChangedEvent(Object source, List<TimerRecord> currentRecord, EnumDataOperateType operate) {
        super(source);
        this.currentRecord = currentRecord;
        this.operate = operate;
    }

}
