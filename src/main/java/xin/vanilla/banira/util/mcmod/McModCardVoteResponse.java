package xin.vanilla.banira.util.mcmod;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * MCMod 卡片投票操作响应
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
public class McModCardVoteResponse {
    /**
     * 状态码：0 表示成功
     */
    private Integer state;
    /**
     * 投票数量
     */
    private McModCardVoteNum num;
    /**
     * 投票占比
     */
    private McModCardVotePer per;

    public boolean isSuccess() {
        return state != null && state == 0;
    }
}
