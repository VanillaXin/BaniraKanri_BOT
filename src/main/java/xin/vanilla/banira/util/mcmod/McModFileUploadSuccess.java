package xin.vanilla.banira.util.mcmod;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * MCMod 文件上传成功信息
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
public class McModFileUploadSuccess {
    /**
     * 新上传的文件名列表
     */
    private List<String> upload;
    /**
     * 更新的文件名列表
     */
    private List<String> update;
}

