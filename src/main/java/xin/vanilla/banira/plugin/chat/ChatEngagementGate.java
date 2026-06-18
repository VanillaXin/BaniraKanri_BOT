package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;
import xin.vanilla.banira.config.entity.extended.ChatEngagementSettings;
import xin.vanilla.banira.config.entity.extended.ChatReplySettings;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.enums.EnumMessageType;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.Locale;
import java.util.Objects;
import java.util.concurrent.ThreadLocalRandom;
import java.util.regex.Pattern;

/**
 * 决定是否调用 LLM 分析本轮消息（替代纯概率冒泡）。
 */
@Slf4j
public class ChatEngagementGate {

    private static final Pattern CQ_CODE = Pattern.compile("\\[CQ:[^]]+]");
    private static final Pattern AMBIGUOUS_UNTARGETED_IDENTITY_LABEL = Pattern.compile(
            "(?i)^(?:ai|aigc|gpt|chatgpt|llm|bot|robot|\\u4eba\\u5de5\\u667a\\u80fd|\\u673a\\u5668\\u4eba|\\u6a5f\\u5668\\u4eba|\\u5927\\u6a21\\u578b|\\u8bed\\u8a00\\u6a21\\u578b|\\u8a9e\\u8a00\\u6a21\\u578b|\\u6a21\\u578b)[?？!！。.,，、~～]*$"
    );
    private static final Pattern EMOJI_AND_SYMBOLS = Pattern.compile("[\\p{So}\\p{Sk}\\x{1F000}-\\x{1FAFF}\\x{2600}-\\x{27BF}\\s~～!！?？。,.，、…]+");

    private final ChatEngagementSettings engagementCfg;
    private final ChatReplySettings replyCfg;
    private final ChatEngagementService engagementService;
    private final ReplyDecisionMaker legacyDecisionMaker;

    public ChatEngagementGate(@Nonnull ChatEngagementSettings engagementCfg
            , @Nonnull ChatReplySettings replyCfg
            , @Nonnull ChatEngagementService engagementService
            , @Nonnull ReplyDecisionMaker legacyDecisionMaker
    ) {
        this.engagementCfg = Objects.requireNonNull(engagementCfg, "engagementCfg");
        this.replyCfg = Objects.requireNonNull(replyCfg, "replyCfg");
        this.engagementService = Objects.requireNonNull(engagementService, "engagementService");
        this.legacyDecisionMaker = Objects.requireNonNull(legacyDecisionMaker, "legacyDecisionMaker");
    }

    public boolean shouldInvokeModel(long botId
            , @Nonnull BaniraCodeContext ctx
            , boolean directMentioned
            , boolean nameMentioned
            , boolean botNamePrefixMentioned
            , int affinityScore
    ) {
        if (!engagementCfg.enabled()) {
            return legacyDecisionMaker.shouldReply(
                    ctx,
                    directMentioned || botNamePrefixMentioned || ctx.msgType() != EnumMessageType.GROUP,
                    nameMentioned,
                    affinityScore
            );
        }

        long groupId = ctx.group() != null ? ctx.group() : 0L;
        engagementService.decayIfExpired(botId, groupId, engagementCfg);

        if (ctx.msgType() != EnumMessageType.GROUP) {
            return true;
        }
        if (directMentioned || botNamePrefixMentioned) {
            return true;
        }
        if (engagementService.hasActiveFollow(botId, groupId, engagementCfg)) {
            LOGGER.debug("engagement follow invoke bot={} group={} interest={}",
                    botId, groupId, engagementService.currentInterest(botId, groupId, engagementCfg));
            return true;
        }
        if (!directMentioned && !nameMentioned && !botNamePrefixMentioned
                && isAmbiguousUntargetedIdentityLabel(ctx.msg())) {
            LOGGER.debug("engagement skip ambiguous untargeted identity label group={} sender={}", groupId, ctx.sender());
            return false;
        }
        if (nameMentioned) {
            return true;
        }
        if (engagementCfg.ownerAlwaysInvoke() && BaniraUtils.isOwner(ctx.sender())) {
            return true;
        }
        if (shouldInvokeForSemiFullPreflight(ctx)) {
            LOGGER.debug("engagement semifull preflight invoke group={} sender={}", groupId, ctx.sender());
            return true;
        }
        if (engagementCfg.randomBubbleEnabled()) {
            double roll = ThreadLocalRandom.current().nextDouble();
            boolean hit = roll <= replyCfg.baseReplyProbability();
            LOGGER.debug("engagement random bubble group={} p={} roll={} => {}",
                    groupId, replyCfg.baseReplyProbability(), roll, hit);
            return hit;
        }
        return false;
    }

    public void onSkippedMessage(long botId
            , @Nonnull BaniraCodeContext ctx
            , boolean directMentioned
            , boolean nameMentioned
            , boolean botNamePrefixMentioned
    ) {
        if (!engagementCfg.enabled() || ctx.msgType() != EnumMessageType.GROUP) {
            return;
        }
        long groupId = ctx.group() != null ? ctx.group() : 0L;
        boolean strongTrigger = directMentioned
                || botNamePrefixMentioned
                || nameMentioned
                || (engagementCfg.ownerAlwaysInvoke() && BaniraUtils.isOwner(ctx.sender()));
        engagementService.applyPassiveDecay(botId, groupId, strongTrigger, engagementCfg);
    }

    private boolean shouldInvokeForSemiFullPreflight(@Nonnull BaniraCodeContext ctx) {
        if (!engagementCfg.preflightEnabled()) {
            return false;
        }
        String scope = StringUtils.nullToEmpty(engagementCfg.preflightScope()).trim().toUpperCase(Locale.ROOT);
        if (!"SEMIFULL".equals(scope)) {
            return false;
        }
        String raw = StringUtils.nullToEmpty(ctx.msg());
        if (StringUtils.isNullOrEmptyEx(raw)) {
            return false;
        }
        String withoutCq = CQ_CODE.matcher(raw).replaceAll(" ").replaceAll("\\s+", " ").trim();
        if (engagementCfg.preflightSkipPureMedia()
                && StringUtils.isNullOrEmptyEx(withoutCq)
                && CQ_CODE.matcher(raw).find()) {
            return false;
        }
        if (engagementCfg.preflightSkipShortNoise() && isShortNoise(withoutCq, engagementCfg.preflightShortNoiseMaxChars())) {
            return false;
        }
        if (engagementCfg.preflightSkipAmbiguousSecondPerson() && isAmbiguousSecondPerson(withoutCq)) {
            return false;
        }
        return true;
    }

    private static boolean isAmbiguousUntargetedIdentityLabel(String text) {
        if (StringUtils.isNullOrEmptyEx(text)) {
            return false;
        }
        String compact = CQ_CODE.matcher(text)
                .replaceAll(" ")
                .replaceAll("\\s+", "")
                .trim();
        return AMBIGUOUS_UNTARGETED_IDENTITY_LABEL.matcher(compact).matches();
    }

    private static boolean isShortNoise(@Nonnull String text, int maxChars) {
        String compact = text.replaceAll("\\s+", "");
        if (StringUtils.isNullOrEmptyEx(compact)) {
            return true;
        }
        int limit = Math.max(1, maxChars);
        if (compact.length() > limit) {
            return false;
        }
        if (containsUsefulShortIntent(compact)) {
            return false;
        }
        return EMOJI_AND_SYMBOLS.matcher(compact).matches()
                || !compact.matches(".*[\\p{IsHan}A-Za-z0-9].*");
    }

    private static boolean isAmbiguousSecondPerson(@Nonnull String text) {
        String compact = text.replaceAll("\\s+", "");
        if (StringUtils.isNullOrEmptyEx(compact)) {
            return false;
        }
        String lower = compact.toLowerCase(Locale.ROOT);
        boolean hasSecondPerson = compact.contains("你") || compact.contains("妳") || compact.contains("你们")
                || lower.contains("you");
        if (!hasSecondPerson) {
            return false;
        }
        boolean looksLikeOpenQuestion = compact.startsWith("有没有")
                || compact.startsWith("谁")
                || compact.startsWith("哪")
                || compact.startsWith("什么")
                || compact.startsWith("怎么")
                || compact.startsWith("为什么")
                || compact.contains("大家")
                || compact.contains("有人")
                || compact.contains("谁知道");
        return !looksLikeOpenQuestion;
    }

    private static boolean containsUsefulShortIntent(@Nonnull String compact) {
        return compact.contains("?")
                || compact.contains("？")
                || compact.contains("谁")
                || compact.contains("啥")
                || compact.contains("查")
                || compact.contains("搜")
                || compact.contains("禁")
                || compact.contains("撤")
                || compact.contains("在");
    }
}
