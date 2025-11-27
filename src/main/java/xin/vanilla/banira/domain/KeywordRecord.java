package xin.vanilla.banira.domain;


import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import lombok.Data;
import lombok.experimental.Accessors;
import xin.vanilla.banira.enums.EnumKeywordType;

/**
 * 关键词记录
 */
@Data
@Accessors(chain = true)
public class KeywordRecord {
    /**
     * 关键词记录ID
     */
    @TableId(type = IdType.AUTO)
    private Long id;
    /**
     * 机器人ID
     */
    private Long botId;
    /**
     * 群组ID
     */
    private Long groupId = 0L;
    /**
     * 添加人
     */
    private Long creatorId;
    /**
     * 添加时间
     */
    private Long time;
    /**
     * 关键词类型
     */
    private EnumKeywordType keywordType;
    /**
     * 关键词
     */
    private String keyword;
    /**
     * 回复消息
     */
    private String replyMsg;
    /**
     * 启用状态
     */
    private Boolean enable = true;
    /**
     * 审核状态
     */
    private Boolean audited = false;

    /**
     * 权重
     */
    private Integer priority = 1;


    public KeywordRecord setGroupId(Long groupId) {
        this.groupId = groupId != null ? groupId : 0L;
        return this;
    }

    public KeywordRecord setEnable(Boolean enable) {
        this.enable = enable != null ? enable : false;
        return this;
    }

    public Boolean getEnable() {
        return enable != null && enable;
    }

}
