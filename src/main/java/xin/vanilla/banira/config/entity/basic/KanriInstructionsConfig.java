package xin.vanilla.banira.config.entity.basic;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * 管理指令配置
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class KanriInstructionsConfig {

    /**
     * 前缀
     */
    private List<String> prefix;
    /**
     * 头衔
     */
    private List<String> tag;
    /**
     * 群名片
     */
    private List<String> card;
    /**
     * 戳一戳
     */
    private List<String> tap;
    /**
     * 禁言
     */
    private List<String> mute;
    /**
     * 解除禁言
     */
    private List<String> loud;
    /**
     * 撤回
     */
    private List<String> withdraw;
    /**
     * 踢出群聊
     */
    private List<String> kick;
    /**
     * 精华
     */
    private List<String> essence;
    /**
     * 群管理
     */
    private List<String> admin;
    /**
     * 主管
     */
    private List<String> butler;
    /**
     * 女仆
     */
    private List<String> maid;
    /**
     * 权限
     */
    private List<String> op;
    /**
     * 群名称
     */
    private List<String> groupName;
    /**
     *
     */
    private List<String> approve;


    {
        this.prefix = new ArrayList<>();
        this.tag = BaniraUtils.mutableListOf("头衔", "tag");
        this.card = BaniraUtils.mutableListOf("群名片", "card");
        this.tap = BaniraUtils.mutableListOf("戳一戳", "戳", "tap", "slap");
        this.mute = BaniraUtils.mutableListOf("禁言", "mute", "ban");
        this.loud = BaniraUtils.mutableListOf("解除禁言", "解禁", "loud", "unmute", "unban");
        this.withdraw = BaniraUtils.mutableListOf("撤回", "withdraw", "recall", "rec");
        this.kick = BaniraUtils.mutableListOf("踢出群聊", "kick");
        this.essence = BaniraUtils.mutableListOf("精华", "essence");
        this.admin = BaniraUtils.mutableListOf("群管理", "admin", "gad", "ad");
        this.butler = BaniraUtils.mutableListOf("主管", "管家", "女仆长", "butler", "chief");
        this.maid = BaniraUtils.mutableListOf("仆人", "女仆", "妹抖", "servant", "maid");
        this.op = BaniraUtils.mutableListOf("权限", "op", "permission");
        this.groupName = BaniraUtils.mutableListOf("群名", "群名称", "groupname", "gname", "gn");
        this.approve = BaniraUtils.mutableListOf("审批", "审核", "approve");

    }

}
