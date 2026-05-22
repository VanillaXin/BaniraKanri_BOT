package xin.vanilla.banira.config.entity.extended;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class ChatModelSettings {
    /**
     * 多接口配置（优先使用；为空时回退到下方单接口字段）
     */
    private List<ChatModelEndpoint> endpoints = new ArrayList<>();
    /**
     * 默认优先使用的接口名称
     */
    private String defaultEndpoint = "";
    /**
     * 请求失败时是否轮询其他接口
     */
    private boolean rotateOnFailure = true;
    /**
     * 是否向模型发送图片输入。仅在所选模型明确支持 vision/image input 时开启。
     */
    private boolean imageInputEnabled = false;
    /**
     * 模型 API Key（单接口兼容字段）
     */
    private String apiKey = "";
    /**
     * 模型名称
     */
    private String modelName = "gpt-4o-mini";
    /**
     * OpenAI 兼容 API 地址
     */
    private String baseUrl = "https://api.openai.com/v1/";
    /**
     * 采样温度，角色化聊天建议 0.75~0.9
     */
    private double temperature = 0.8;
    /**
     * 请求超时（秒），启用 Agent 时建议 ≥ 60
     */
    private long timeout = 60;
    /**
     * 失败重试次数
     */
    private int maxRetries = 2;
}
