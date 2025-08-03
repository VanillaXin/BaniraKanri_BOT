package xin.vanilla.banira.enums;

import lombok.Getter;

@Getter
public enum EnumPermission {
    MUTE("设置群员禁言"),
    LOUD("解除群员禁言"),
    MALL("设置全体禁言"),
    ATAL("艾特全体成员"),
    CARD("修改群名片"),
    TAG("修改群头衔"),
    KICK("移除群员"),
    REC("撤回消息"),
    GARE("处理加群请求"),
    ESSE("设置群精华"),
    GNAM("修改群名称"),
    ADMI("增删群管理"),
    ;

    private final String desc;

    EnumPermission(String desc) {
        this.desc = desc;
    }

}
