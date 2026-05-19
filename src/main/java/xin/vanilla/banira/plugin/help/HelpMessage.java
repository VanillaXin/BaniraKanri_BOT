package xin.vanilla.banira.plugin.help;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * 帮助合并转发中的一条消息
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
public class HelpMessage {

    private String content = "";
    /**
     * 合并转发显示的发送人名称，为空时使用机器人默认昵称
     */
    private String senderName = "";

    @Nonnull
    public static HelpMessage of(@Nonnull String content) {
        return new HelpMessage().content(content);
    }

    @Nonnull
    public static HelpMessage of(@Nonnull String content, @Nullable String senderName) {
        return new HelpMessage().content(content).senderName(senderName);
    }

}
