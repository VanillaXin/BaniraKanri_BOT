package xin.vanilla.banira.domain;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * AI 长期记忆
 */
@Data
@Accessors(chain = true)
@TableName("ai_memory")
public class AiMemory {
    @TableId(type = IdType.AUTO)
    private Long id;
    private Long botId;
    private Long groupId = 0L;
    private Long userId = 0L;
    private String content = "";
    private String tags = "";
    private String sourceMsgId = "";
    private Long createdAt;
    private Long lastUsedAt;

    public AiMemory setGroupId(Long groupId) {
        this.groupId = groupId != null ? groupId : 0L;
        return this;
    }

    public AiMemory setUserId(Long userId) {
        this.userId = userId != null ? userId : 0L;
        return this;
    }

    public AiMemory setContent(String content) {
        this.content = content != null ? content : "";
        return this;
    }

    public AiMemory setTags(String tags) {
        this.tags = tags != null ? tags : "";
        return this;
    }

    public AiMemory setSourceMsgId(String sourceMsgId) {
        this.sourceMsgId = sourceMsgId != null ? sourceMsgId : "";
        return this;
    }

}
