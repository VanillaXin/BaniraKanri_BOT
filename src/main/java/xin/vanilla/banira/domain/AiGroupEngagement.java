package xin.vanilla.banira.domain;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * AI 按群对话兴趣值。
 */
@Data
@Accessors(chain = true)
@TableName("ai_group_engagement")
public class AiGroupEngagement {
    @TableId(type = IdType.AUTO)
    private Long id;
    private Long botId;
    private Long groupId = 0L;
    private Integer interest = 0;
    private Long updatedAt;

    public AiGroupEngagement setGroupId(Long groupId) {
        this.groupId = groupId != null ? groupId : 0L;
        return this;
    }

    public AiGroupEngagement setInterest(Integer interest) {
        this.interest = interest != null ? interest : 0;
        return this;
    }
}
