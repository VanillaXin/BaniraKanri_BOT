package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.util.List;

/**
 * 结构化 AI 回复：台词 + 参考资料
 */
public record StructuredReply(@Nonnull String speech
        , @Nonnull List<String> references
        , @Nullable Integer replyToMessageId
        , @Nonnull List<Long> atTargets
        , boolean directHandled
) {

    public StructuredReply {
        speech = speech != null ? speech : "";
        references = references != null ? List.copyOf(references) : List.of();
        atTargets = atTargets != null ? List.copyOf(atTargets) : List.of();
    }

    public StructuredReply(@Nonnull String speech, @Nonnull List<String> references) {
        this(speech, references, null, List.of(), false);
    }

    public StructuredReply(@Nonnull String speech, @Nonnull List<String> references, boolean directHandled) {
        this(speech, references, null, List.of(), directHandled);
    }

    public static StructuredReply handledDirectly() {
        return new StructuredReply("", List.of(), true);
    }
}
