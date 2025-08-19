package xin.vanilla.banira.config.entity.basic;

import lombok.experimental.Accessors;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.Set;

/**
 * 管理指令配置
 *
 * @param prefix    前缀
 * @param tag       头衔
 * @param card      群名片
 * @param tap       戳一戳
 * @param mute      禁言
 * @param loud      解除禁言
 * @param withdraw  撤回
 * @param kick      踢出群聊
 * @param essence   精华
 * @param admin     群管理
 * @param butler    主管
 * @param maid      女仆
 * @param op        权限
 * @param groupName 群名称
 */
@Accessors(chain = true)
public record KanriInstructionsConfig(
        Set<String> prefix,
        Set<String> tag,
        Set<String> card,
        Set<String> tap,
        Set<String> mute,
        Set<String> loud,
        Set<String> withdraw,
        Set<String> kick,
        Set<String> essence,
        Set<String> admin,
        Set<String> butler,
        Set<String> maid,
        Set<String> op,
        Set<String> groupName,
        Set<String> approve
) {

    public static KanriInstructionsConfig preset() {
        return new KanriInstructionsConfig(
                BaniraUtils.mutableSetOf(),
                BaniraUtils.mutableSetOf("头衔", "tag"),
                BaniraUtils.mutableSetOf("群名片", "card"),
                BaniraUtils.mutableSetOf("戳一戳", "戳", "tap", "slap"),
                BaniraUtils.mutableSetOf("禁言", "mute", "ban"),
                BaniraUtils.mutableSetOf("解除禁言", "解禁", "loud", "unmute", "unban"),
                BaniraUtils.mutableSetOf("撤回", "withdraw", "recall", "rec"),
                BaniraUtils.mutableSetOf("踢出群聊", "kick"),
                BaniraUtils.mutableSetOf("精华", "essence"),
                BaniraUtils.mutableSetOf("群管理", "admin", "gad", "ad"),
                BaniraUtils.mutableSetOf("主管", "管家", "女仆长", "butler", "chief"),
                BaniraUtils.mutableSetOf("仆人", "女仆", "妹抖", "servant", "maid"),
                BaniraUtils.mutableSetOf("权限", "op", "permission"),
                BaniraUtils.mutableSetOf("群名", "群名称", "groupname", "gname", "gn"),
                BaniraUtils.mutableSetOf("审批", "审核", "approve")
        );
    }

}
