package xin.vanilla.banira.config.entity.extended;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * 单个 LLM 接口配置
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class ChatModelEndpoint {

    /**
     * 接口名称（唯一标识，供配额查询使用）
     */
    private String name = "";
    /**
     * API Key
     */
    private String apiKey = "";
    /**
     * OpenAI 兼容 API 地址，留空则继承 model.baseUrl
     */
    private String baseUrl = "";
    /**
     * 模型名称，留空则继承 model.modelName
     */
    private String modelName = "";
    /**
     * 配额/余额查询地址（GET，Bearer 鉴权），留空则尝试自动探测
     */
    private String quotaCheckUrl = "";
    /**
     * 是否启用
     */
    private boolean enabled = true;
    /**
     * 备注（如供应商、套餐说明）
     */
    private String remark = "";

}
