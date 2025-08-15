package xin.vanilla.banira.enums;

import lombok.Getter;

import java.util.EnumSet;
import java.util.Set;
import java.util.stream.Collectors;

@Getter
public enum EnumPermission {
    MUTE("设置群员禁言", "mute", 1),
    LOUD("解除群员禁言", "loud", 1),
    MALL("设置全体禁言", "mute all", 1),
    ATAL("艾特全体成员", "at all", 1),
    CARD("修改群名片", "card", 1),
    TAG("修改群头衔", "tag", 1),
    KICK("移除群员", "kick", 1),
    RECA("撤回消息", "recall", 1),
    GARE("处理加群请求", "group add request", 1),
    AESS("设置群精华", "add essence", 1),
    RESS("设置群精华", "remove essence", 1),
    GNAM("修改群名称", "group name", 1),
    AADM("增加群管理", "add admin", 2),
    RADM("移除群管理", "remove admin", 2),
    ;

    private final String desc;
    private final String word;
    private final int level;

    EnumPermission(String desc, String word, int level) {
        this.desc = desc;
        this.word = word;
        this.level = level;
    }

    /**
     * 获取所有权限
     */
    public static Set<EnumPermission> getAll() {
        return EnumSet.allOf(EnumPermission.class);
    }

    /**
     * 获取群主拥有的权限
     */
    public static Set<EnumPermission> getGroupOwner() {
        return getAll().stream()
                .filter(p -> p.level <= 2)
                .collect(Collectors.toSet());
    }

    /**
     * 获取群管理员拥有的权限
     */
    public static Set<EnumPermission> getGroupAdmin() {
        return getAll().stream()
                .filter(p -> p.level <= 1)
                .collect(Collectors.toSet());
    }

}
