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
public class ChatReplySettings {
    /**
     * 基础回复概率（未 @ 时）
     */
    private double baseReplyProbability = 0.08;
    /**
     * 被 @ 时的概率加成
     */
    private double mentionBoost = 0.45;
    /**
     * 直接 @ 机器人时的目标回复概率。
     */
    private double directMentionReplyProbability = 0.90;
    /**
     * 消息内容包含机器人名字时的目标回复概率。
     */
    private double nameMentionReplyProbability = 0.90;
    /**
     * 额外名字触发词，除机器人昵称外也会匹配。
     */
    private List<String> botNameAliases = new ArrayList<>();
    /**
     * 同一发送者的最小回复间隔（秒）
     */
    private long perTargetCooldownSeconds = 8;
    /**
     * 同一发送者每分钟最大回复数
     */
    private int perTargetRateLimitPerMinute = 8;
    /**
     * 注入 Prompt 的历史消息条数
     */
    private int historyLimit = 30;
    /**
     * 台词按句拆分的最多条数
     */
    private int maxSplitParts = 6;
    /**
     * 多条台词之间的基础发送延迟（毫秒）。0 表示不延迟。
     */
    private long splitPartDelayMillis = 900;
    /**
     * 多条台词之间的随机附加延迟上限（毫秒）。0 表示无随机抖动。
     */
    private long splitPartDelayJitterMillis = 500;
    /**
     * Send a short acknowledgement when a directly addressed turn takes long.
     */
    private boolean thinkingFeedbackEnabled = true;
    /**
     * Delay before sending the acknowledgement.
     */
    private long thinkingFeedbackDelayMillis = 2500;
    /**
     * 单条消息最大字符数（拆分用）
     */
    private int maxCharsPerPart = 100;
    /**
     * 超过此长度改用合并转发
     */
    private int maxForwardLength = 500;
    /**
     * 合并转发中单个节点的最大字符数。应明显大于普通消息拆分长度，避免代码被拆碎。
     */
    private int maxForwardChunkChars = 1800;
    /**
     * 回复硬上限（字符），0 表示不截断
     */
    private int maxReplyChars = 260;
    /**
     * 允许 AI 用 [AT:QQ] 在回复中 @ 群成员
     */
    private boolean allowStructuredMentions = true;
    /**
     * 允许 AI 用 [REPLY:消息ID] 引用回复消息
     */
    private boolean allowStructuredReply = true;
    /**
     * 群聊消息密集时，若 AI 没有主动引用，自动引用当前消息。
     */
    private boolean autoReplyInBusyGroup = true;
    /**
     * 判断群聊是否密集的时间窗口（秒）。
     */
    private long busyGroupWindowSeconds = 45;
    /**
     * 时间窗口内达到多少条历史消息视为群聊密集。0 表示关闭。
     */
    private int busyGroupMessageThreshold = 6;
    /**
     * 时间窗口内达到多少个不同发送者视为群聊密集。0 表示不检查发送者数量。
     */
    private int busyGroupDistinctSenderThreshold = 3;
    /**
     * 单条回复最多 @ 的人数
     */
    private int maxAtTargets = 3;
    /**
     * 单次普通回复最多保留的 emoji 数量
     */
    private int maxEmojiPerReply = 0;
    /**
     * 是否去掉每条消息末尾刻意补上的句号
     */
    private boolean stripFinalSentencePeriod = true;
}
