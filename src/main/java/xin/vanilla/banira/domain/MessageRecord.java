package xin.vanilla.banira.domain;

import lombok.Data;
import xin.vanilla.banira.enums.EnumMessageType;

@Data
public class MessageRecord {
    private Long id;
    private String nos;
    private Long bot;
    private Long sender;
    private Long target;
    private Long time;
    private String msgJson;
    private String msgText;
    private EnumMessageType msgType;
}
