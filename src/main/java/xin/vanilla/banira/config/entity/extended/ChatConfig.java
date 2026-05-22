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
     * LLM 连接参数
     */
    private ChatModelSettings model = new ChatModelSettings();
    /**
     * 回复概率与长度控制
     */
    private ChatReplySettings reply = new ChatReplySettings();
    /**
     * Agent 工具调用
     */
    private ChatAgentSettings agent = new ChatAgentSettings();
    /**
     * 长期记忆
     */
    private ChatMemorySettings memory = new ChatMemorySettings();
    /**
     * 用户好感度
     */
    private ChatAffinitySettings affinity = new ChatAffinitySettings();
    /**
     * LLM 参与回复决策与兴趣值跟进
     */
    private ChatEngagementSettings engagement = new ChatEngagementSettings();
    /**
     * 安全、身份、自我介绍、记忆等轻量守卫规则。
     */
    private ChatGuardSettings guard = new ChatGuardSettings();
    /**
     * 是否注入内置默认人格提示（建议在 systemPrompt 中自行编写后设为 false）
     */
    private boolean useDefaultPersonaPrompt = true;
    /**
     * 系统提示（人格与行为规则）
     */
    private List<String> systemPrompt = new ArrayList<>();
}
