package xin.vanilla.banira.domain;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * AI 对用户的好感度。
 */
@Data
@Accessors(chain = true)
@TableName("ai_affinity")
public class AiAffinity {
    @TableId(type = IdType.AUTO)
    private Long id;
    private Long botId;
    private Long groupId = 0L;
    private Long userId = 0L;
    private Integer score = 50;
    private Long updatedAt;

    public AiAffinity setGroupId(Long groupId) {
        this.groupId = groupId != null ? groupId : 0L;
        return this;
    }

    public AiAffinity setUserId(Long userId) {
        this.userId = userId != null ? userId : 0L;
        return this;
    }

    public AiAffinity setScore(Integer score) {
        this.score = score != null ? score : 50;
        return this;
    }
}
