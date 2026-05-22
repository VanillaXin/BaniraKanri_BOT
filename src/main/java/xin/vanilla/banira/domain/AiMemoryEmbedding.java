package xin.vanilla.banira.domain;

import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * AI memory embedding index.
 */
@Data
@Accessors(chain = true)
@TableName("ai_memory_embedding")
public class AiMemoryEmbedding {
    @TableId
    private Long memoryId;
    private Long botId;
    private Long groupId = 0L;
    private Long userId = 0L;
    private String modelName = "";
    private Integer dimension = 0;
    private String vectorJson = "";
    private Long updatedAt;

    public AiMemoryEmbedding setGroupId(Long groupId) {
        this.groupId = groupId != null ? groupId : 0L;
        return this;
    }

    public AiMemoryEmbedding setUserId(Long userId) {
        this.userId = userId != null ? userId : 0L;
        return this;
    }

    public AiMemoryEmbedding setModelName(String modelName) {
        this.modelName = modelName != null ? modelName : "";
        return this;
    }

    public AiMemoryEmbedding setVectorJson(String vectorJson) {
        this.vectorJson = vectorJson != null ? vectorJson : "";
        return this;
    }
}
