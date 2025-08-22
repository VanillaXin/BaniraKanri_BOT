package xin.vanilla.banira.config.entity.basic;

import lombok.experimental.Accessors;

import java.util.Arrays;
import java.util.List;

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
        List<String> prefix,
        List<String> tag,
        List<String> card,
        List<String> tap,
        List<String> mute,
        List<String> loud,
        List<String> withdraw,
        List<String> kick,
        List<String> essence,
        List<String> admin,
        List<String> butler,
        List<String> maid,
        List<String> op,
        List<String> groupName,
        List<String> approve
) {

    public static KanriInstructionsConfig preset() {
        return new KanriInstructionsConfig(
                Arrays.asList(),
                Arrays.asList("头衔", "tag"),
                Arrays.asList("群名片", "card"),
                Arrays.asList("戳一戳", "戳", "tap", "slap"),
                Arrays.asList("禁言", "mute", "ban"),
                Arrays.asList("解除禁言", "解禁", "loud", "unmute", "unban"),
                Arrays.asList("撤回", "withdraw", "recall", "rec"),
                Arrays.asList("踢出群聊", "kick"),
                Arrays.asList("精华", "essence"),
                Arrays.asList("群管理", "admin", "gad", "ad"),
                Arrays.asList("主管", "管家", "女仆长", "butler", "chief"),
                Arrays.asList("仆人", "女仆", "妹抖", "servant", "maid"),
                Arrays.asList("权限", "op", "permission"),
                Arrays.asList("群名", "群名称", "groupname", "gname", "gn"),
                Arrays.asList("审批", "审核", "approve")
        );
    }

}
