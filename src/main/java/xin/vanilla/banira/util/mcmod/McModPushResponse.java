package xin.vanilla.banira.util.mcmod;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * MCMod 推荐操作响应
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
public class McModPushResponse {
    /**
     * 状态码：0 表示成功
     */
    private Integer state;
    /**
     * 推荐结果
     */
    private Integer todo;

    public boolean isSuccess() {
        return state != null && state == 0;
    }
}
