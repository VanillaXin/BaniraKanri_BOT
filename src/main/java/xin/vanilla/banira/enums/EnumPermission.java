package xin.vanilla.banira.enums;

import lombok.Getter;

@Getter
public enum EnumPermission {
    MUTE("设置群员禁言", "mute"),
    LOUD("解除群员禁言", "loud"),
    MALL("设置全体禁言", "mute all"),
    ATAL("艾特全体成员", "at all"),
    CARD("修改群名片", "card"),
    TAG("修改群头衔", "tag"),
    KICK("移除群员", "kick"),
    RECA("撤回消息", "recall"),
    GARE("处理加群请求", "group add request"),
    ESSE("设置群精华", "essence"),
    GNAM("修改群名称", "group name"),
    ADMI("增删群管理", "admin"),
    ;

    private final String desc;
    private final String word;

    EnumPermission(String desc, String word) {
        this.desc = desc;
        this.word = word;
    }

}
