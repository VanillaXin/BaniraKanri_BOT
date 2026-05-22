package xin.vanilla.banira.plugin.chat.agent;

import com.mikuac.shiro.dto.action.response.LoginInfoResp;
import dev.langchain4j.data.message.ChatMessage;
import dev.langchain4j.data.message.SystemMessage;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.domain.AiMemory;
import xin.vanilla.banira.plugin.chat.memory.MemoryRetriever;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.DateUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.*;

/**
 * 组装 Agent 使用的 Prompt 消息
 */
public class PromptBuilder {

    private static final String DEFAULT_PERSONA = "prompt/aichat/default-persona.txt";
    private static final String RUNTIME_CONTEXT = "prompt/aichat/runtime-context.txt";
    private static final String SAFETY_RULE = "prompt/aichat/safety-rule.txt";
    private static final String MEMORY_BLOCK = "prompt/aichat/memory-block.txt";
    private static final String CURRENT_MESSAGE_FOCUS = "prompt/aichat/current-message-focus.txt";
    private static final String OWNER_LINE = "prompt/aichat/owner-line.txt";
    private static final String ENGAGEMENT_META = "prompt/aichat/engagement-meta.txt";

    private PromptBuilder() {
    }

    @Nonnull
    public static List<ChatMessage> buildSystemMessages(@Nonnull BaniraBot bot
            , @Nonnull ChatConfig cfg
            , @Nonnull MemoryRetriever memoryRetriever
            , @Nonnull AgentContext ctx
            , @Nonnull String currentUserText
    ) {
        List<ChatMessage> messages = new ArrayList<>();
        for (String sysPrompt : cfg.systemPrompt()) {
            if (StringUtils.isNotNullOrEmpty(sysPrompt)) {
                messages.add(SystemMessage.from(sysPrompt));
            }
        }
        if (cfg.useDefaultPersonaPrompt()) {
            PromptTemplateLoader.loadSections(DEFAULT_PERSONA).forEach(text -> messages.add(SystemMessage.from(text)));
        }
        LoginInfoResp loginInfoEx = bot.getLoginInfoEx();
        String botNickname = loginInfoEx != null ? StringUtils.toString(loginInfoEx.getNickname(), "") : "";
        messages.add(SystemMessage.from(PromptTemplateLoader.render(RUNTIME_CONTEXT, Map.of(
                "currentTime", DateUtils.toDateTimeString(new Date()),
                "botNickname", StringUtils.isNotNullOrEmpty(botNickname) ? botNickname : BaniraUtils.getBotNick(),
                "botNamesLine", buildBotNamesLine(bot, cfg, botNickname),
                "ownerNick", BaniraUtils.getOwnerNick(),
                "ownerDisplay", BaniraUtils.getOwnerDisplayName(),
                "ownerLine", buildOwnerLine()
        ))));
        addTemplateMessage(messages, SAFETY_RULE, Map.of());
        List<AiMemory> memories = memoryRetriever.retrieve(ctx, cfg, currentUserText);
        String memoryText = memoryRetriever.format(ctx, memories);
        if (StringUtils.isNotNullOrEmpty(memoryText)) {
            addTemplateMessage(messages, MEMORY_BLOCK, Map.of("memoryText", memoryText));
        }
        return messages;
    }

    @Nonnull
    public static ChatMessage buildCurrentMessageFocusMessage() {
        String text = PromptTemplateLoader.render(CURRENT_MESSAGE_FOCUS, Map.of());
        return SystemMessage.from(text);
    }

    @Nonnull
    public static ChatMessage buildEngagementMetaMessage() {
        String text = PromptTemplateLoader.render(ENGAGEMENT_META, Map.of());
        return SystemMessage.from(text);
    }

    @Nonnull
    private static String buildBotNamesLine(@Nonnull BaniraBot bot, @Nonnull ChatConfig cfg, @Nullable String botNickname) {
        Set<String> names = new LinkedHashSet<>();
        if (StringUtils.isNotNullOrEmpty(botNickname)) {
            names.add(botNickname.trim());
        }
        for (String nick : BaniraUtils.getBotNicks()) {
            if (StringUtils.isNotNullOrEmpty(nick)) {
                names.add(nick.trim());
            }
        }
        if (cfg.reply() != null && cfg.reply().botNameAliases() != null) {
            for (String alias : cfg.reply().botNameAliases()) {
                if (StringUtils.isNotNullOrEmpty(alias)) {
                    names.add(alias.trim());
                }
            }
        }
        if (names.isEmpty()) {
            return "";
        }
        if (names.size() == 1) {
            return "";
        }
        return "群友也可能这样叫你：" + String.join("、", names);
    }

    @Nonnull
    private static String buildOwnerLine() {
        Long owner = BaniraUtils.getOwner();
        if (owner == null || owner <= 0) {
            return "";
        }
        return PromptTemplateLoader.render(OWNER_LINE, Map.of(
                "owner", String.valueOf(owner),
                "ownerNick", BaniraUtils.getOwnerNick(),
                "ownerDisplay", BaniraUtils.getOwnerDisplayName()
        ));
    }

    private static void addTemplateMessage(@Nonnull List<ChatMessage> messages
            , @Nonnull String resourcePath
            , @Nonnull Map<String, String> variables
    ) {
        String text = PromptTemplateLoader.render(resourcePath, variables);
        if (StringUtils.isNotNullOrEmpty(text)) {
            messages.add(SystemMessage.from(text));
        }
    }

}
