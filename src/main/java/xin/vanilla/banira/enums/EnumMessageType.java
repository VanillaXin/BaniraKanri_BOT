package xin.vanilla.banira.enums;

import com.mikuac.shiro.constant.ActionParams;
import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import com.mikuac.shiro.dto.event.message.MessageEvent;
import xin.vanilla.banira.util.BaniraUtils;

public enum EnumMessageType {
    FRIEND(),
    GROUP(),
    MEMBER(),
    STRANGER(),
    ;


    public static EnumMessageType getType(MessageEvent event) {
        if (ActionParams.GROUP.equals(event.getMessageType())) {
            return EnumMessageType.GROUP;
        } else if (ActionParams.PRIVATE.equals(event.getMessageType())) {
            if (event instanceof GroupMessageEvent groupEvent && BaniraUtils.isGroupIdValid(groupEvent.getGroupId())) {
                return EnumMessageType.MEMBER;
            } else {
                return EnumMessageType.FRIEND;
            }
        } else {
            return EnumMessageType.STRANGER;
        }
    }
}
