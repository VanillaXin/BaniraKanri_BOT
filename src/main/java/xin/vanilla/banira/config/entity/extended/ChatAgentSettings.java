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
public class ChatAgentSettings {
    /**
     * 是否启用 Agent 工具调用循环
     */
    private boolean enabled = true;
    /**
     * 单次对话最大工具调用轮数
     */
    private int maxIterations = 8;
    /**
     * AI 能力允许列表。为空表示允许所有已启用且权限满足的能力。
     */
    private List<String> allowedCapabilities = new ArrayList<>();
    /**
     * AI 能力禁用列表，优先级高于允许列表。
     */
    private List<String> blockedCapabilities = new ArrayList<>();
    /**
     * 额外限制为仅主人可调用的能力名。
     */
    private List<String> ownerOnlyCapabilities = new ArrayList<>();
    /**
     * 额外限制为主人或本群管理员可调用的能力名。
     */
    private List<String> adminOnlyCapabilities = new ArrayList<>();
    /**
     * 是否在 LLM 之前本地预执行禁言（如 @禁我10分钟）。默认 false，由 Agent 调工具执行。
     */
    private boolean localKanriPreflight = false;
}
