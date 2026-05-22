package xin.vanilla.banira.config.entity.extended;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class ChatEngagementSettings {
    /**
     * 是否启用 LLM 参与「是否回复」与兴趣值跟进，替代纯概率冒泡。
     */
    private boolean enabled = true;
    /**
     * 兴趣值达到此阈值时，下一条群消息（同群隔离）会直接调用 LLM 继续判断。
     */
    private int followInterestThreshold = 50;
    /**
     * 兴趣值有效时长（秒）；超时未续则归零。
     */
    private long followTtlSeconds = 180;
    /**
     * 是否允许未 @、未跟进时的随机冒泡（建议关闭，由 LLM 决策替代）。
     */
    private boolean randomBubbleEnabled = false;
    /**
     * 主人发言时是否总是调用 LLM 判断（即使未 @）。
     */
    private boolean ownerAlwaysInvoke = true;
    /**
     * 是否将兴趣值持久化到数据库（重启/刷新配置后仍可恢复跟进）。
     */
    private boolean persistenceEnabled = true;
    /**
     * 未触发 LLM 的群消息是否对已有兴趣值做被动衰减。
     */
    private boolean passiveDecayEnabled = true;
    /**
     * 每条无关群消息扣减的兴趣值。
     */
    private int passiveDecayDelta = 3;
    /**
     * 是否在主 Agent 前启用轻量预检模型，过滤明显不必回复的消息。
     */
    private boolean preflightEnabled = true;
    /**
     * 预检范围：SEMIFULL 表示除纯噪声外的群消息都可进入轻量预检。
     */
    private String preflightScope = "SEMIFULL";
    /**
     * 半全量预检时是否跳过纯媒体/CQ 码消息。
     */
    private boolean preflightSkipPureMedia = true;
    /**
     * 半全量预检时是否跳过明显无语义短噪声。
     */
    private boolean preflightSkipShortNoise = true;
    /**
     * Skip bare group messages like "you..." when the bot was not addressed.
     */
    private boolean preflightSkipAmbiguousSecondPerson = true;
    /**
     * 短噪声最大文本长度。
     */
    private int preflightShortNoiseMaxChars = 2;
    /**
     * 预检使用的 endpoint 名称；留空则使用默认主 endpoint。
     */
    private String preflightEndpointName = "";
    /**
     * 预检注入的历史条数上限。
     */
    private int preflightHistoryLimit = 10;
    /**
     * 预检采样温度。
     */
    private double preflightTemperature = 0.2;
    /**
     * 同群进行中的 LLM 回合是否合并后续触发，避免并行各答各的。
     */
    private boolean turnCoalescingEnabled = true;
    /**
     * 合并重跑前短暂等待（毫秒），便于异步消息入库。
     */
    private long coalesceRetryDelayMillis = 250;
}
