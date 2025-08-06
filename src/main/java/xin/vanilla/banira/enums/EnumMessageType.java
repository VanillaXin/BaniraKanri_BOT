package xin.vanilla.banira.enums;

import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import com.mikuac.shiro.dto.event.message.MessageEvent;
import com.mikuac.shiro.dto.event.message.PrivateMessageEvent;

public enum EnumMessageType {
    FRIEND(),
    GROUP(),
    MEMBER(),
    STRANGER(),
    ;


    public static EnumMessageType getType(MessageEvent event) {
        return switch (event) {
            case GroupMessageEvent ignored -> GROUP;
            case PrivateMessageEvent friend when friend.getSubType().equalsIgnoreCase("friend") -> FRIEND;
            case PrivateMessageEvent friend when friend.getSubType().equalsIgnoreCase("group") -> MEMBER;
            default -> STRANGER;
        };
    }
}
