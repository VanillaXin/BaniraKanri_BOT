package xin.vanilla.banira.config.entity.group;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import xin.vanilla.banira.config.contract.GroupConfig;
import xin.vanilla.banira.config.entity.extended.ChatConfig;

/**
 * AI 聊天群配置。
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class AIChatGroupConfig implements GroupConfig {

    /**
     * AI聊天配置。
     */
    private ChatConfig chatConfig;

    {
        this.chatConfig = new ChatConfig();
    }
}
