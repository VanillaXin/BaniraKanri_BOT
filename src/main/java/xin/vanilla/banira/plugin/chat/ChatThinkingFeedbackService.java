package xin.vanilla.banira.plugin.chat;

import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.enums.MsgTypeEnum;
import com.mikuac.shiro.model.ArrayMsg;
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
import java.util.regex.Pattern;

public final class ChatThinkingFeedbackService {

    private static final String FEEDBACK_PROMPT = "prompt/aichat/thinking-feedback.txt";
    private static final Pattern SENSITIVE_TOPIC = Pattern.compile(
            "(政治|政权|政府|官僚|体制|意识形态|共产党|中共|国家领导|领导人|独裁|民主|革命|红色话题|新疆|西藏|台湾|港独|台独|俄乌|巴以|中东|制裁|战争)"
    );
    private static final Pattern QUICK_CONTEXT_QUESTION = Pattern.compile(
            "(为什么不理|为啥不理|怎么不理|谁说的|谁问的|谁提的|你刚刚|你前面|是不是|在不在|人呢|干嘛|什么意思|啥意思)"
    );
    private static final Pattern LONG_RUNNING_REQUEST = Pattern.compile(
            "(搜|搜索|查查|查询|检索|查资料|找资料|网页|联网|百度|Google|Bing|看看这张|看图|识别图片|图片里|分析图片|读图|总结|整理|归纳|长文|日志|报错|堆栈|代码|模板|脚本|程序|实现|重构|修复|生成|写一段|写个|MC百科|MCMod|模组详情|作者信息|合并转发|上传文件)"
    );
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
        if (!looksWorthFeedback(ctx)) {
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

    private static boolean looksWorthFeedback(@Nonnull BaniraCodeContext ctx) {
        String message = StringUtils.nullToEmpty(ctx.msg()).trim();
        String compact = message.replaceAll("\\s+", "");
        if (StringUtils.isNullOrEmptyEx(compact)) {
            return hasFeedbackMedia(ctx.originalMsg());
        }
        if (SENSITIVE_TOPIC.matcher(compact).find()) {
            return false;
        }
        if (QUICK_CONTEXT_QUESTION.matcher(compact).find() && !LONG_RUNNING_REQUEST.matcher(compact).find()) {
            return false;
        }
        if (hasFeedbackMedia(ctx.originalMsg())) {
            return true;
        }
        if (LONG_RUNNING_REQUEST.matcher(compact).find()) {
            return true;
        }
        return compact.length() >= 80 && (compact.contains("分析") || compact.contains("看看") || compact.contains("怎么"));
    }

    private static boolean hasFeedbackMedia(List<ArrayMsg> messages) {
        if (messages == null || messages.isEmpty()) {
            return false;
        }
        for (ArrayMsg message : messages) {
            if (message == null || message.getType() == null) {
                continue;
            }
            MsgTypeEnum type = message.getType();
            if (type == MsgTypeEnum.image
                    || type == MsgTypeEnum.video
                    || type == MsgTypeEnum.record
                    || type == MsgTypeEnum.forward) {
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
        if (looksMechanicalFeedback(cleaned)) {
            return "";
        }
        if (cleaned.length() > 24) {
            cleaned = cleaned.substring(0, 24).trim();
        }
        return ReplyPostProcessor.processPart(cleaned, settings);
    }

    private static boolean looksMechanicalFeedback(@Nonnull String text) {
        String compact = text.replaceAll("\\s+", "");
        return compact.contains("收到")
                || compact.contains("处理")
                || compact.contains("已接收")
                || compact.contains("正在思考")
                || compact.contains("接住")
                || compact.contains("任务")
                || compact.contains("流程")
                || compact.contains("马上")
                || compact.contains("请稍等")
                || compact.contains("稍等一下")
                || compact.contains("稍等")
                || compact.contains("等一下");
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
