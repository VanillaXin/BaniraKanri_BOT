package xin.vanilla.banira.enums;

import lombok.Getter;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.*;

@Getter
public enum EnumPermission {
    APER("增加权限", "add permission", 1),
    RPER("移除权限", "remove permission", 1),
    MUTE("设置群员禁言", "mute", 1),
    LOUD("解除群员禁言", "loud", 1),
    MALL("设置全体禁言", "mute all", 1),
    LALL("解除全体禁言", "loud all", 1),
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
    AMAI("增加女仆", "add maid", 3),
    RMAI("移除女仆", "remove maid", 3),
    ABUT("增加主管", "add butler", 4),
    RBUT("移除主管", "remove butler", 4),
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
     * 获取主管拥有的权限
     */
    public static List<EnumPermission> getButler() {
        return getAll().stream()
                .filter(p -> p.level <= 3)
                .toList();
    }

    /**
     * 获取女仆拥有的权限
     */
    public static List<EnumPermission> getMaid() {
        return getGroupAdmin();
    }

    /**
     * 获取群主拥有的权限
     */
    public static List<EnumPermission> getGroupOwner() {
        return getAll().stream()
                .filter(p -> p.level <= 2)
                .toList();
    }

    /**
     * 获取群管理员拥有的权限
     */
    public static List<EnumPermission> getGroupAdmin() {
        return getAll().stream()
                .filter(p -> p.level <= 1)
                .toList();
    }

    public static EnumPermission valueFrom(String s) {
        for (EnumPermission permission : EnumPermission.values()) {
            if (permission.name().equalsIgnoreCase(s)
                    || permission.word.equalsIgnoreCase(s)
                    || permission.desc.equalsIgnoreCase(s)
                    || BaniraUtils.getPermissionNames(permission).contains(s)
            ) {
                return permission;
            }
        }
        return null;
    }

    public static List<EnumPermission> valueFrom(Collection<String> collection) {
        return collection.stream()
                .map(EnumPermission::valueFrom)
                .filter(Objects::nonNull)
                .toList();
    }

    public static List<EnumPermission> valueFrom(String... strings) {
        return valueFrom(List.of(strings));
    }

}
