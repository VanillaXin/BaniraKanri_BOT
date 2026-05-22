package xin.vanilla.banira.plugin.chat;

import com.mikuac.shiro.common.utils.MsgUtils;
import dev.langchain4j.data.message.ChatMessage;
import dev.langchain4j.data.message.SystemMessage;
import dev.langchain4j.data.message.UserMessage;
import dev.langchain4j.model.chat.response.ChatResponse;
import jakarta.annotation.Nonnull;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.config.entity.extended.ChatReplySettings;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.enums.EnumMessageType;
import xin.vanilla.banira.plugin.chat.agent.PromptTemplateLoader;
import xin.vanilla.banira.plugin.chat.model.ChatModelRouter;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

public final class ChatThinkingFeedbackService {

    private static final String FEEDBACK_PROMPT = "prompt/aichat/thinking-feedback.txt";
    private static final ScheduledExecutorService EXECUTOR = Executors.newSingleThreadScheduledExecutor(r -> {
        Thread thread = new Thread(r, "ai-thinking-feedback");
        thread.setDaemon(true);
        return thread;
    });

    private ChatThinkingFeedbackService() {
    }

    @Nonnull
    public static FeedbackHandle arm(@Nonnull BaniraBot bot
            , @Nonnull BaniraCodeContext ctx
            , @Nonnull ChatConfig cfg
            , @Nonnull ChatModelRouter modelRouter
    ) {
        AtomicBoolean done = new AtomicBoolean(false);
        ChatReplySettings settings = cfg.reply();
        if (!shouldArm(bot, ctx, settings) || !modelRouter.isReady()) {
            return new FeedbackHandle(done);
        }
        long delay = Math.max(300L, settings.thinkingFeedbackDelayMillis());
        EXECUTOR.schedule(() -> {
            if (done.get()) {
                return;
            }
            String text = generateFeedback(modelRouter, ctx);
            if (done.get()) {
                return;
            }
            text = sanitizeFeedback(text, settings);
            if (StringUtils.isNullOrEmptyEx(text)) {
                return;
            }
            send(bot, ctx, settings, text);
        }, delay, TimeUnit.MILLISECONDS);
        return new FeedbackHandle(done);
    }

    private static boolean shouldArm(@Nonnull BaniraBot bot
            , @Nonnull BaniraCodeContext ctx
            , @Nonnull ChatReplySettings settings
    ) {
        if (!settings.thinkingFeedbackEnabled() || settings.thinkingFeedbackDelayMillis() <= 0) {
            return false;
        }
        if (ctx.msgType() != EnumMessageType.GROUP) {
            return true;
        }
        if (bot.isMentioned(ctx.originalMsg())) {
            return true;
        }
        String message = StringUtils.nullToEmpty(ctx.msg()).trim().toLowerCase(Locale.ROOT);
        for (String nick : BaniraUtils.getBotNicks()) {
            if (StringUtils.isNotNullOrEmpty(nick) && message.contains(nick.trim().toLowerCase(Locale.ROOT))) {
                return true;
            }
        }
        for (String alias : settings.botNameAliases()) {
            if (StringUtils.isNotNullOrEmpty(alias) && message.contains(alias.trim().toLowerCase(Locale.ROOT))) {
                return true;
            }
        }
        return false;
    }

    @Nonnull
    private static String generateFeedback(@Nonnull ChatModelRouter modelRouter, @Nonnull BaniraCodeContext ctx) {
        try {
            List<ChatMessage> prompt = new ArrayList<>();
            PromptTemplateLoader.loadSections(FEEDBACK_PROMPT).forEach(text -> prompt.add(SystemMessage.from(text)));
            prompt.add(UserMessage.from("Current message:\n" + ChatInputSanitizer.sanitizeUserText(ctx.msg())));
            ChatResponse response = modelRouter.chat(prompt);
            return response.aiMessage() != null ? StringUtils.nullToEmpty(response.aiMessage().text()) : "";
        } catch (Exception ignored) {
            return "";
        }
    }

    @Nonnull
    private static String sanitizeFeedback(@Nonnull String text, @Nonnull ChatReplySettings settings) {
        String cleaned = StringUtils.nullToEmpty(text)
                .replaceAll("(?i)\\[ENGAGE[^]]*]", "")
                .replaceAll("(?i)\\[PREFLIGHT[^]]*]", "")
                .replaceAll("(?i)\\[/?REF]", "")
                .replaceAll("(?i)\\[REPLY:[^]]+]", "")
                .replaceAll("(?i)\\[AT:[^]]+]", "")
                .replaceAll("[\"'`]+", "")
                .trim();
        if (cleaned.equalsIgnoreCase("null")
                || cleaned.equals("\"\"")
                || cleaned.equals("''")
                || cleaned.equalsIgnoreCase("empty")) {
            return "";
        }
        String[] lines = cleaned.split("\\R+");
        cleaned = "";
        for (String line : lines) {
            if (StringUtils.isNotNullOrEmpty(line)) {
                cleaned = line.trim();
                break;
            }
        }
        if (cleaned.length() > 24) {
            cleaned = cleaned.substring(0, 24).trim();
        }
        return ReplyPostProcessor.processPart(cleaned, settings);
    }

    private static void send(@Nonnull BaniraBot bot
            , @Nonnull BaniraCodeContext ctx
            , @Nonnull ChatReplySettings settings
            , @Nonnull String text
    ) {
        try {
            if (ctx.msgType() == EnumMessageType.GROUP && BaniraUtils.isGroupIdValid(ctx.group())) {
                MsgUtils builder = MsgUtils.builder();
                if (settings.allowStructuredReply() && ctx.msgId() != null && ctx.msgId() > 0) {
                    builder.reply(ctx.msgId());
                }
                builder.text(text);
                bot.sendGroupMsg(ctx.group(), builder.build(), false);
            } else if (BaniraUtils.isUserIdValid(ctx.sender())) {
                bot.sendPrivateMsg(ctx.sender(), text, false);
            }
        } catch (Exception ignored) {
        }
    }

    public record FeedbackHandle(AtomicBoolean done) implements AutoCloseable {
        @Override
        public void close() {
            done.set(true);
        }
    }
}
