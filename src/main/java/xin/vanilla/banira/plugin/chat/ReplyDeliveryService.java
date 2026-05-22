package xin.vanilla.banira.plugin.chat;

import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.common.utils.ShiroUtils;
import com.mikuac.shiro.dto.action.common.ActionData;
import com.mikuac.shiro.dto.action.common.MsgId;
import com.mikuac.shiro.dto.action.response.LoginInfoResp;
import jakarta.annotation.Nonnull;
import xin.vanilla.banira.config.entity.extended.ChatReplySettings;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.enums.EnumMessageType;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ThreadLocalRandom;

/**
 * AI 回复投递：台词按句分条，参考资料合并转发
 */
public final class ReplyDeliveryService {

    private ReplyDeliveryService() {
    }

    public static boolean deliver(@Nonnull BaniraBot bot
            , @Nonnull BaniraCodeContext ctx
            , @Nonnull StructuredReply reply
            , @Nonnull ChatReplySettings settings
    ) {
        boolean sent = false;
        List<Integer> sentMessageIds = new ArrayList<>();
        if (StringUtils.isNotNullOrEmpty(reply.speech())) {
            List<String> speechParts = MessageSplitter.split(
                    reply.speech(),
                    settings.maxCharsPerPart(),
                    settings.maxSplitParts()
            );
            boolean firstPart = true;
            for (String part : speechParts) {
                part = ReplyPostProcessor.processPart(part, settings);
                if (StringUtils.isNullOrEmptyEx(part)) {
                    continue;
                }
                if (sent) {
                    sleepBetweenSplitParts(settings);
                }
                if (ctx.msgType() == EnumMessageType.GROUP) {
                    String outgoing = firstPart ? decorateGroupSpeechPart(bot, ctx, reply, part, settings) : part;
                    collectSentMessageId(bot.sendGroupMsg(ctx.group(), outgoing, false), bot, sentMessageIds);
                } else {
                    collectSentMessageId(bot.sendPrivateMsg(ctx.sender(), part, false), bot, sentMessageIds);
                }
                firstPart = false;
                sent = true;
            }
        }
        if (!reply.references().isEmpty()) {
            if (sent) {
                sleepBetweenSplitParts(settings);
            }
            sentMessageIds.addAll(sendReferencesForward(bot, ctx, reply.references(), settings));
            sent = true;
        }
        if (!sentMessageIds.isEmpty()) {
            RecentAiReplyTracker.remember(bot, ctx, sentMessageIds);
        }
        return sent;
    }

    @Nonnull
    private static String decorateGroupSpeechPart(@Nonnull BaniraBot bot
            , @Nonnull BaniraCodeContext ctx
            , @Nonnull StructuredReply reply
            , @Nonnull String part
            , @Nonnull ChatReplySettings settings
    ) {
        MsgUtils builder = MsgUtils.builder();
        if (settings.allowStructuredReply()
                && reply.replyToMessageId() != null
                && reply.replyToMessageId() > 0) {
            builder.reply(reply.replyToMessageId());
        }
        if (settings.allowStructuredMentions() && settings.maxAtTargets() > 0) {
            int count = 0;
            for (Long target : reply.atTargets()) {
                if (target == null || !BaniraUtils.isUserIdValid(target) || target == bot.getSelfId()) {
                    continue;
                }
                if (count >= settings.maxAtTargets()) {
                    break;
                }
                builder.at(target);
                count++;
            }
            if (count > 0 && !part.startsWith(" ")) {
                builder.text(" ");
            }
        }
        builder.text(part);
        return builder.build();
    }

    @Nonnull
    private static List<Integer> sendReferencesForward(@Nonnull BaniraBot bot
            , @Nonnull BaniraCodeContext ctx
            , @Nonnull List<String> references
            , @Nonnull ChatReplySettings settings
    ) {
        LoginInfoResp loginInfoEx = bot.getLoginInfoEx();
        List<Map<String, Object>> forwardMsg = new ArrayList<>();
        forwardMsg.add(ShiroUtils.generateSingleMsg(
                ctx.sender(),
                bot.getUserNameEx(ctx.group(), ctx.sender()),
                ctx.msg()
        ));
        for (String reference : references) {
            String cleanedReference = StructuredReplyPipeline.cleanReferenceText(reference);
            if (StructuredReplyPipeline.isUnsuitableForwardReferenceText(cleanedReference)) {
                continue;
            }
            for (String chunk : splitReference(cleanedReference, settings.maxForwardChunkChars())) {
                if (StringUtils.isNullOrEmptyEx(chunk)) {
                    continue;
                }
                forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname(), chunk));
            }
        }
        if (forwardMsg.size() <= 1) {
            return List.of();
        }
        List<Integer> sentMessageIds = new ArrayList<>();
        if (ctx.msgType() == EnumMessageType.GROUP) {
            collectSentMessageId(bot.sendGroupForwardMsg(ctx.group(), forwardMsg), bot, sentMessageIds);
        } else {
            collectSentMessageId(bot.sendPrivateForwardMsg(ctx.sender(), forwardMsg), bot, sentMessageIds);
        }
        return sentMessageIds;
    }

    private static void collectSentMessageId(ActionData<MsgId> data, @Nonnull BaniraBot bot, @Nonnull List<Integer> sentMessageIds) {
        if (bot.isActionDataMsgIdNotEmpty(data)) {
            sentMessageIds.add(bot.getActionDataMsgId(data));
        }
    }

    @Nonnull
    private static List<String> splitReference(@Nonnull String text, int maxChars) {
        int limit = Math.max(300, maxChars);
        if (text.length() <= limit) {
            return List.of(text);
        }
        List<String> chunks = new ArrayList<>();
        int offset = 0;
        while (offset < text.length()) {
            int end = Math.min(text.length(), offset + limit);
            if (end < text.length()) {
                int newline = text.lastIndexOf('\n', end);
                if (newline > offset + limit / 2) {
                    end = newline;
                }
            }
            String chunk = text.substring(offset, end).trim();
            if (StringUtils.isNotNullOrEmpty(chunk)) {
                chunks.add(chunk);
            }
            offset = end;
        }
        return chunks;
    }

    private static void sleepBetweenSplitParts(@Nonnull ChatReplySettings settings) {
        long base = Math.max(0L, settings.splitPartDelayMillis());
        long jitter = Math.max(0L, settings.splitPartDelayJitterMillis());
        if (base <= 0 && jitter <= 0) {
            return;
        }
        long delay = base + (jitter > 0 ? ThreadLocalRandom.current().nextLong(jitter + 1) : 0L);
        try {
            Thread.sleep(delay);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

}
