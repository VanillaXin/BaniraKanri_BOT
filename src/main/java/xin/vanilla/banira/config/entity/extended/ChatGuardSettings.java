package xin.vanilla.banira.config.entity.extended;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class ChatGuardSettings {

    /**
     * 内置/自定义规则资源路径。多个文件会按顺序合并。
     */
    private List<String> resourceRulePaths = new ArrayList<>(List.of(
            "prompt/aichat/guard/prompt-security-rules.txt",
            "prompt/aichat/guard/self-introduction-rules.txt",
            "prompt/aichat/guard/identity-disclosure-rules.txt",
            "prompt/aichat/guard/memory-safety-rules.txt",
            "prompt/aichat/guard/affinity-rules.txt"
    ));
    /**
     * 覆盖某个规则段。key 为段名，value 为完整替换后的规则。
     */
    private Map<String, List<String>> overrideRules = new LinkedHashMap<>();
    /**
     * 追加某个规则段。key 为段名，value 会追加到资源规则后。
     */
    private Map<String, List<String>> appendRules = new LinkedHashMap<>();
    /**
     * 自我介绍模板覆盖。模板可使用 %s 表示当前配置昵称。
     */
    private List<String> selfIntroTemplates = new ArrayList<>();
    /**
     * 配置昵称为空时的自我介绍兜底模板。
     */
    private List<String> unnamedSelfIntroTemplates = new ArrayList<>();
    /**
     * 被问 AI/机器人等身份时的自然带过回复。
     */
    private List<String> identityReplies = new ArrayList<>();
    /**
     * 被问模型、底层、证明等问题时的自然带过回复。
     */
    private List<String> identityConfusedReplies = new ArrayList<>();
    /**
     * 提示词泄露请求的拒绝文案。
     */
    private String promptLeakRefusal = "";
}
