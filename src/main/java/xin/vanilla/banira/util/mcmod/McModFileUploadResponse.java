package xin.vanilla.banira.util.mcmod;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * MCMod 文件上传响应
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
public class McModFileUploadResponse {
    /**
     * 状态码：0 表示成功
     */
    private Integer state;
    /**
     * 成功上传的文件信息
     */
    private McModFileUploadSuccess success;
    /**
     * 失败的文件名列表
     */
    private List<String> failed;

    /**
     * 是否成功
     *
     * @return true 如果 state 为 0
     */
    public boolean isSuccess() {
        return state != null && state == 0;
    }
}

