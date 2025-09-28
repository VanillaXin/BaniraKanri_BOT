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
public class ChatConfig {
    /**
     * 是否开启自动回复
     */
    private boolean enabled;
    /**
     * 基本回复概率
     */
    private double baseReplyProbability = 0.35;
    /**
     * 群聊内的最小触发概率加成
     */
    private double mentionBoost = 0.45;
    /**
     * 每个群或用户的最小间隔（秒）用于去抖动
     */
    private long perTargetCooldownSeconds = 6;
    /**
     * 每分钟的最大回复数
     */
    private int perTargetRateLimitPerMinute = 10;
    /**
     * 历史消息读取条数
     */
    private int historyLimit = 20;
    /**
     * 拆分的最大片段数
     */
    private int maxSplitParts = 3;
    /**
     * 单条消息最大字符（粗略）限制，用于拆分
     */
    private int maxCharsPerPart = 200;
    /**
     * 当回复内容长度超过阈值时使用合并转发进行回复
     */
    private int maxForwardLength = 600;
    /**
     * 模拟输入的最快打字速度(字/ms)
     */
    private int minTypingSpeed = 100;
    /**
     * 模拟输入的最慢打字速度(字/ms)
     */
    private int maxTypingSpeed = 200;
    /**
     * 短时间内的最大回复数
     */
    private int globalRateLimitPerMinute = 120;
    /**
     * 模型温度
     * <p>
     * 0.0：近乎确定性（通常不是严格 deterministic，但非常保守），适合事实性/工具型回复或需要稳定输出的场景。
     * <p>
     * ~0.2–0.5：偏保守，适合需要准确性/一致性的问答、摘要、检索增强的回答。
     * <p>
     * ~0.6–0.9：比较自然、带个性（适合角色化的聊天）。
     * <p>
     * 1.0：默认的随机性（较平衡）。
     * <p>
     * >1.0：更“发散”，适合创意写作，但更容易出现不相关或不准确的内容。
     */
    private double temperature = 0.75;
    /**
     * 模型请求超时（秒）
     */
    private long timeout = 30;
    /**
     * 模型请求重试次数
     */
    private int maxRetries = 2;
    /**
     * 模型API Key
     */
    private String apiKey = "123456789";
    /**
     * 模型名称
     */
    private String modelName = "gpt-4o";
    /**
     * 模型地址
     */
    private String baseUrl = "https://api.openai.com/v1/";
    /**
     * 系统提示
     */
    private List<String> systemPrompt = new ArrayList<>();
}
