package xin.vanilla.banira.plugin.chat.capability;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import xin.vanilla.banira.plugin.chat.agent.AgentContext;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Central policy that prevents the model from executing tools with stale history parameters.
 */
public final class CapabilityInvocationPolicy {

    private CapabilityInvocationPolicy() {
    }

    @Nonnull
    public static Decision evaluate(@Nonnull AgentContext ctx
            , @Nullable AiCapability capability
            , @Nonnull String capabilityName
            , @Nonnull Map<String, String> args
    ) {
        String current = currentText(ctx, capability);
        String normalizedName = normalizeName(capabilityName);
        boolean mutating = capability != null && (capability.mutating()
                || capability.mutationPolicy() != null && capability.mutationPolicy() != AiMutationPolicy.NONE)
                || isKnownMutating(normalizedName);
        if (!mutating && !requiresFreshCurrentMessage(normalizedName)) {
            return Decision.allow();
        }
        if (StringUtils.isNullOrEmptyEx(current)) {
            return Decision.block("当前最新消息为空，已阻止工具调用。");
        }
        return switch (normalizedName) {
            case "search_mcmod" -> allowsMcmodSearch(current, args);
            case "vote_mcmod" -> allowsMcmodVote(current, args);
            case "push_mcmod" -> allowsMcmodPush(current, args);
            case "comment_mcmod" -> allowsMcmodComment(current, args);
            case "reply_mcmod_comment" -> allowsMcmodCommentReply(current, args);
            case "delete_mcmod_comment" -> allowsMcmodCommentDelete(current, args);
            case "create_timer" -> allowsTimerCreate(current, args);
            case "recall_last_ai_reply" -> allowsRecallLastAiReply(current);
            case "execute_kanri" -> allowsKanri(ctx, current, args);
            case "execute_rcon" -> allowsRcon(current, args);
            case "web_search" -> allowsWebSearch(current, args);
            case "get_weather" -> allowsWeather(current, args);
            default -> mutating ? allowsGenericMutating(normalizedName, current, args) : Decision.allow();
        };
    }

    @Nonnull
    private static Decision allowsMcmodSearch(@Nonnull String current, @Nonnull Map<String, String> args) {
        String lower = current.toLowerCase(Locale.ROOT);
        String keyword = arg(args, "keyword", "keywordOrId");
        boolean searchIntent = containsAny(current,
                "搜索", "查询", "查找", "搜", "查", "看看", "看一下", "详情", "信息", "怎么样", "是什么", "确认", "核实", "再查")
                || lower.contains("mc百科")
                || lower.contains("mcmod")
                || containsAny(current, "模组", "整合包", "作者", "用户", "资料", "教程");
        if (!searchIntent) {
            return Decision.block("当前最新消息没有明确要求 MC百科查询，不能根据历史话题继续搜索。");
        }
        boolean targetMatched = StringUtils.isNotNullOrEmpty(keyword) && current.contains(keyword);
        return targetMatched ? Decision.allow() : Decision.block("当前最新消息没有明确 MC百科查询目标，已阻止沿用旧参数。");
    }

    @Nonnull
    private static Decision allowsMcmodVote(@Nonnull String current, @Nonnull Map<String, String> args) {
        if (!containsAny(current, "红票", "黑票", "投票", "点票", "vote")) {
            return Decision.block("当前最新消息没有明确要求给 MC百科目标投红票/黑票，不能沿用历史话题执行投票。");
        }
        return targetInCurrent(current, arg(args, "keywordOrId", "keyword"))
                ? Decision.allow()
                : Decision.block("当前最新消息没有包含本次 MC百科投票目标，已阻止沿用旧参数。");
    }

    @Nonnull
    private static Decision allowsMcmodPush(@Nonnull String current, @Nonnull Map<String, String> args) {
        boolean pushVerb = current.contains("推荐") && containsAny(current, "点", "投", "给", "推荐一下", "推荐下");
        if (!pushVerb) {
            return Decision.block("当前最新消息没有明确要求点推荐，不能沿用历史话题执行推荐。");
        }
        return targetInCurrent(current, arg(args, "keywordOrId", "keyword"))
                ? Decision.allow()
                : Decision.block("当前最新消息没有包含本次 MC百科推荐目标，已阻止沿用旧参数。");
    }

    @Nonnull
    private static Decision allowsMcmodComment(@Nonnull String current, @Nonnull Map<String, String> args) {
        if (!containsAny(current, "留言", "评论")) {
            return Decision.block("当前最新消息没有明确要求发布 MC百科留言。");
        }
        String keyword = arg(args, "keywordOrId", "keyword");
        String content = arg(args, "content");
        return targetInCurrent(current, keyword) && targetInCurrent(current, content)
                ? Decision.allow()
                : Decision.block("当前最新消息没有同时包含 MC百科留言目标和内容，已阻止沿用旧参数。");
    }

    @Nonnull
    private static Decision allowsMcmodCommentReply(@Nonnull String current, @Nonnull Map<String, String> args) {
        if (!current.contains("回复")) {
            return Decision.block("当前最新消息没有明确要求回复评论。");
        }
        String content = arg(args, "content");
        String commentId = arg(args, "commentId");
        String containerId = arg(args, "containerId");
        boolean idMatched = targetInCurrent(current, commentId) || targetInCurrent(current, containerId);
        return targetInCurrent(current, content) && idMatched
                ? Decision.allow()
                : Decision.block("当前最新消息没有包含本次评论回复内容和 ID，已阻止沿用旧参数。");
    }

    @Nonnull
    private static Decision allowsMcmodCommentDelete(@Nonnull String current, @Nonnull Map<String, String> args) {
        if (!containsAny(current, "删除", "删掉", "移除") || !containsAny(current, "评论", "留言")) {
            return Decision.block("当前最新消息没有明确要求删除 MC百科评论。");
        }
        return targetInCurrent(current, arg(args, "commentId"))
                ? Decision.allow()
                : Decision.block("当前最新消息没有包含要删除的评论 ID，已阻止沿用旧参数。");
    }

    @Nonnull
    private static Decision allowsTimerCreate(@Nonnull String current, @Nonnull Map<String, String> args) {
        if (!current.contains("提醒")) {
            return Decision.block("当前最新消息没有明确要求创建提醒，已阻止沿用历史参数重复创建。");
        }
        boolean timeMentioned = current.matches(".*([\\d一二两俩三四五六七八九十百半]+\\s*个?\\s*(分钟|分|小时|时|天|日)后).*")
                || current.matches(".*(明天|后天|今天|今晚|早上|上午|中午|下午|晚上|\\d{1,2}[:：点时]).*");
        if (!timeMentioned) {
            return Decision.block("当前最新消息没有明确提醒时间，已阻止创建提醒。");
        }
        String message = arg(args, "message");
        String plainCurrent = current.replaceAll("\\[CQ:[^]]+]", "");
        boolean contentMatched = targetInCurrent(plainCurrent, message)
                || plainCurrent.contains(message.replaceFirst("^提醒[：:，,\\s]*", ""))
                || message.contains("起床") && plainCurrent.contains("起床")
                || message.contains("吃饭") && plainCurrent.contains("吃饭")
                || message.contains("喝水") && plainCurrent.contains("喝水");
        return contentMatched ? Decision.allow() : Decision.block("当前最新消息没有包含本次提醒内容，已阻止沿用旧参数。");
    }

    @Nonnull
    private static Decision allowsRecallLastAiReply(@Nonnull String current) {
        if (!containsAny(current, "撤回", "撤掉", "删掉", "收回")) {
            return Decision.block("当前最新消息没有明确要求撤回我刚发的回复，已阻止工具调用。");
        }
        if (containsAny(current, "你", "妳", "刚刚", "刚才", "上一条", "上条", "消息", "回复")) {
            return Decision.allow();
        }
        return Decision.block("当前最新消息没有明确撤回对象，已阻止工具调用。");
    }

    @Nonnull
    private static Decision allowsKanri(@Nonnull AgentContext ctx, @Nonnull String current, @Nonnull Map<String, String> args) {
        String action = arg(args, "action").toLowerCase(Locale.ROOT);
        String actionText = arg(args, "args");
        boolean confirm = "true".equalsIgnoreCase(arg(args, "confirm"));
        boolean selfTarget = "true".equalsIgnoreCase(arg(args, "selfTarget"));
        if (ctx.kanriMuteSucceeded() && isMuteOrLoudAction(action)) {
            return Decision.block("本回合禁言已由系统执行，勿重复调用群管工具。");
        }
        if ("loud".equals(action) && selfTarget) {
            return Decision.block("不允许自助解禁。");
        }
        if ("mute".equals(action) && selfTarget) {
            return allowsCurrentSenderSelfTarget(ctx, actionText);
        }
        if ("card".equals(action) && isBotGroupCardAction(ctx, current, actionText)) {
            return Decision.allow();
        }
        if ("card".equals(action) && isSelfGroupCardAction(ctx, current, actionText)) {
            return allowsCurrentSenderCardTarget(ctx, actionText);
        }
        if ("card".equals(action) && confirm && hasNumericTargets(actionText)
                && hasGroupCardValue(actionText) && isDirectlyAddressedToBot(ctx, current)) {
            return Decision.allow();
        }
        if ("card".equals(action) && confirm && "true".equalsIgnoreCase(arg(args, "followupResolved"))
                && hasNumericTargets(actionText) && hasGroupCardValue(actionText)) {
            return Decision.allow();
        }
        if (isMuteOrLoudAction(action) && isWholeGroupTarget(actionText)) {
            if (confirm) {
                return Decision.block("全员禁言/解禁需要先保存待确认，再由同一用户在下一轮确认执行。");
            }
            return mentionsWholeGroup(current)
                    ? Decision.allow()
                    : Decision.block("当前最新消息没有明确要求全员禁言/解禁，已阻止沿用旧参数。");
        }
        if (isMuteOrLoudAction(action) && confirm && hasNumericTargets(actionText)) {
            return Decision.allow();
        }
        boolean verbMatched = containsAny(current, "禁言", "解禁", "解除禁言", "名片", "头衔", "精华", "群名", "群名称")
                || containsAny(current.toLowerCase(Locale.ROOT), "mute", "loud", "card", "tag", "essence", action);
        if (!verbMatched) {
            return Decision.block("当前最新消息没有明确要求执行这个群管动作，已阻止沿用旧参数。");
        }
        if (argsContainCurrentTarget(current, actionText) || (isMuteOrLoudAction(action) && hasNumericTargets(actionText))) {
            return Decision.allow();
        }
        return Decision.block("当前最新消息没有包含本次群管目标或参数，已阻止沿用旧参数。");
    }

    @Nonnull
    private static Decision allowsCurrentSenderSelfTarget(@Nonnull AgentContext ctx, @Nonnull String actionText) {
        Long senderId = ctx.senderId();
        if (senderId == null || senderId <= 0) {
            return Decision.block("自我群管缺少当前发送者身份。");
        }
        List<Long> targets = parseMuteTargetIds(actionText);
        if (targets.size() != 1 || !targets.getFirst().equals(senderId)) {
            return Decision.block("自我群管只能作用于当前发消息的人本人。");
        }
        return Decision.allow();
    }

    @Nonnull
    private static Decision allowsCurrentSenderCardTarget(@Nonnull AgentContext ctx, @Nonnull String actionText) {
        Long senderId = ctx.senderId();
        if (senderId == null || senderId <= 0) {
            return Decision.block("自我群名片修改缺少当前发送者身份。");
        }
        String first = firstToken(actionText);
        if (!String.valueOf(senderId).equals(first)) {
            return Decision.block("自我群名片修改只能作用于当前发消息的人本人。");
        }
        return Decision.allow();
    }

    private static boolean isMuteOrLoudAction(@Nonnull String action) {
        return "mute".equals(action) || "loud".equals(action);
    }

    private static boolean hasNumericTargets(@Nullable String actionText) {
        if (StringUtils.isNullOrEmptyEx(actionText)) {
            return false;
        }
        return actionText.matches(".*\\d{5,}.*");
    }

    private static boolean hasGroupCardValue(@Nullable String actionText) {
        if (StringUtils.isNullOrEmptyEx(actionText)) {
            return false;
        }
        String[] parts = actionText.trim().split("\\s+");
        return parts.length >= 2 && StringUtils.isNotNullOrEmpty(parts[parts.length - 1]);
    }

    private static boolean isDirectlyAddressedToBot(@Nonnull AgentContext ctx, @Nonnull String current) {
        return ctx.bot() != null && current.contains("[CQ:at,qq=" + ctx.botId() + "]");
    }

    private static boolean isSelfGroupCardAction(@Nonnull AgentContext ctx, @Nonnull String current, @Nonnull String actionText) {
        CardTargetHint hint = inferGroupCardTargetFromCard(current, groupCardValue(actionText));
        if (hint == CardTargetHint.SENDER) {
            return true;
        }
        return hint == CardTargetHint.UNKNOWN && isSelfGroupCardRequest(current);
    }

    private static boolean isBotGroupCardAction(@Nonnull AgentContext ctx, @Nonnull String current, @Nonnull String actionText) {
        if (ctx.bot() == null || ctx.botId() <= 0) {
            return false;
        }
        if (!String.valueOf(ctx.botId()).equals(firstToken(actionText))) {
            return false;
        }
        CardTargetHint hint = inferGroupCardTargetFromCard(current, groupCardValue(actionText));
        return hint == CardTargetHint.BOT || hint == CardTargetHint.UNKNOWN && isBotGroupCardRequest(current);
    }

    private static boolean isWholeGroupTarget(@Nullable String actionText) {
        String normalized = StringUtils.nullToEmpty(actionText)
                .replaceAll("\\s+", "")
                .toLowerCase(Locale.ROOT);
        return normalized.equals("all")
                || normalized.equals("@all")
                || normalized.equals("全体")
                || normalized.equals("全员")
                || normalized.equals("@全体")
                || normalized.equals("@全员")
                || normalized.equals("@全体成员");
    }

    @Nonnull
    private static String firstToken(@Nullable String value) {
        if (StringUtils.isNullOrEmptyEx(value)) {
            return "";
        }
        String[] parts = value.trim().split("\\s+");
        return parts.length > 0 ? parts[0] : "";
    }

    private static boolean isSelfGroupCardRequest(@Nonnull String current) {
        String compact = current.replaceAll("\\[CQ:[^]]+]", " ")
                .replaceAll("\\s+", "");
        return containsAny(compact, "群名片", "名片", "群昵称")
                && containsAny(compact, "我的", "把我", "给我", "帮我把我");
    }

    private static boolean isBotGroupCardRequest(@Nonnull String current) {
        String compact = current.replaceAll("\\[CQ:[^]]+]", " ")
                .replaceAll("\\s+", "");
        return containsAny(compact, "群名片", "名片", "群昵称")
                && containsAny(compact, "你的", "你自己", "把你", "给你", "将你", "替你");
    }

    private enum CardTargetHint {
        BOT,
        SENDER,
        UNKNOWN
    }

    @Nonnull
    private static CardTargetHint inferGroupCardTargetFromCard(@Nonnull String current, @Nonnull String card) {
        String normalizedCard = card.replaceAll("[\\r\\n\\t]+", " ")
                .replaceAll("\\s+", "")
                .trim();
        if (StringUtils.isNullOrEmptyEx(normalizedCard)) {
            return CardTargetHint.UNKNOWN;
        }
        String compact = current.replaceAll("\\[CQ:[^]]+]", " ")
                .replaceAll("\\s+", "");
        int cardIndex = compact.indexOf(normalizedCard);
        if (cardIndex < 0) {
            return CardTargetHint.UNKNOWN;
        }
        String segment = sentenceSegmentAround(compact, cardIndex);
        if (!containsAny(segment, "群名片", "名片", "群昵称")) {
            return CardTargetHint.UNKNOWN;
        }
        if (containsAny(segment, "我的", "把我", "给我", "将我", "替我", "帮我把我")) {
            return CardTargetHint.SENDER;
        }
        if (containsAny(segment, "你的", "你自己", "把你", "给你", "将你", "替你", "帮你把你")) {
            return CardTargetHint.BOT;
        }
        return CardTargetHint.UNKNOWN;
    }

    @Nonnull
    private static String sentenceSegmentAround(@Nonnull String text, int index) {
        int start = 0;
        int end = text.length();
        for (int i = index - 1; i >= 0; i--) {
            if (isSegmentSeparator(text.charAt(i))) {
                start = i + 1;
                break;
            }
        }
        for (int i = index; i < text.length(); i++) {
            if (isSegmentSeparator(text.charAt(i))) {
                end = i;
                break;
            }
        }
        return text.substring(start, end);
    }

    private static boolean isSegmentSeparator(char c) {
        return c == '，' || c == ',' || c == '。' || c == '；' || c == ';'
                || c == '！' || c == '!' || c == '？' || c == '?';
    }

    @Nonnull
    private static String groupCardValue(@Nullable String actionText) {
        if (StringUtils.isNullOrEmptyEx(actionText)) {
            return "";
        }
        String trimmed = actionText.trim();
        String first = firstToken(trimmed);
        if (StringUtils.isNullOrEmptyEx(first)) {
            return "";
        }
        return trimmed.substring(Math.min(first.length(), trimmed.length())).trim();
    }

    private static boolean mentionsWholeGroup(@Nonnull String current) {
        return containsAny(current, "全员", "全体", "@全体", "@全员")
                || current.toLowerCase(Locale.ROOT).contains("@all");
    }

    @Nonnull
    private static java.util.List<Long> parseMuteTargetIds(@Nullable String actionText) {
        if (StringUtils.isNullOrEmptyEx(actionText)) {
            return List.of();
        }
        String[] parts = actionText.trim().split("\\s+");
        java.util.List<Long> ids = new java.util.ArrayList<>();
        for (int i = 0; i < parts.length - 1; i++) {
            String part = parts[i].trim();
            if (part.matches("\\d{5,}")) {
                ids.add(Long.parseLong(part));
            }
        }
        if (ids.isEmpty() && parts.length > 0) {
            String last = parts[parts.length - 1].trim();
            if (last.matches("\\d{5,}")) {
                ids.add(Long.parseLong(last));
            }
        }
        return ids;
    }

    @Nonnull
    private static Decision allowsRcon(@Nonnull String current, @Nonnull Map<String, String> args) {
        String lower = current.toLowerCase(Locale.ROOT);
        if (!(lower.contains("rcon") || current.contains("服务器命令") || current.contains("执行命令"))) {
            return Decision.block("当前最新消息没有明确要求执行 RCON，已阻止沿用旧参数。");
        }
        return argsContainCurrentTarget(current, arg(args, "server") + " " + arg(args, "command"))
                ? Decision.allow()
                : Decision.block("当前最新消息没有包含本次 RCON 服务器或命令，已阻止沿用旧参数。");
    }

    @Nonnull
    private static Decision allowsWebSearch(@Nonnull String current, @Nonnull Map<String, String> args) {
        String query = arg(args, "query", "keyword");
        String lower = current.toLowerCase(Locale.ROOT);
        boolean explicitSearchIntent = containsAny(current,
                "搜索", "查询", "查找", "检索", "资料", "网页", "最新", "最近", "新闻", "来源", "出处",
                "帮我查", "帮我搜", "你查", "你搜", "查一下", "查下", "查查", "搜一下", "搜下", "搜搜", "再搜", "再查")
                || containsAny(lower, "search", "google", "bing", "web");
        boolean publicFactIntent = containsAny(current,
                "学历", "履历", "简历", "哪一年", "什么时候", "在哪里", "是谁", "谁", "百科")
                || containsAny(lower, "wiki", "wikipedia");
        if (looksLikeResolvedSearchChatter(current) && !isSearchFollowUp(current)) {
            return Decision.block("当前消息只是在讨论已搜索过的结果，不是新的网页搜索请求。");
        }
        if (!explicitSearchIntent && looksLikeGeneralAdviceQuestion(current)) {
            return Decision.block("当前消息是普通建议/推荐问题，不需要网页搜索。");
        }
        boolean searchIntent = explicitSearchIntent
                || publicFactIntent
                || containsAny(lower, "search", "google", "bing", "web");
        if (!searchIntent) {
            return Decision.block("当前最新消息没有明确要求网页搜索，已阻止沿用旧搜索目标。");
        }
        if (StringUtils.isNullOrEmptyEx(query)) {
            return Decision.block("网页搜索关键词为空，已阻止工具调用。");
        }
        String compactCurrent = normalizeComparable(current);
        String compactQuery = normalizeComparable(query);
        if (compactQuery.length() < 2) {
            return Decision.block("网页搜索关键词过短，已阻止工具调用。");
        }
        if (compactCurrent.contains(compactQuery) || hasMeaningfulTokenOverlap(compactCurrent, compactQuery)) {
            return Decision.allow();
        }
        if (isSearchFollowUp(current) && looksLikeReasonableFollowUpQuery(query)) {
            return Decision.allow();
        }
        return Decision.block("当前最新消息没有包含本次网页搜索目标，已阻止沿用旧参数。");
    }

    private static boolean isSearchFollowUp(@Nonnull String current) {
        String compact = current.replaceAll("\\[CQ:[^]]+]", " ")
                .replaceAll("\\s+", "")
                .toLowerCase(Locale.ROOT);
        return containsAny(compact,
                "你搜搜", "搜搜", "搜一下", "搜下", "查一下", "查下", "查查", "搜这个", "查这个",
                "看看这个", "看下这个", "来看看这个", "这个搜一下", "这个查一下")
                || compact.matches(".*(搜|查|检索)(一下|下|查|搜)?(这个|它|这条|上面|刚刚|前面)?.*");
    }

    private static boolean looksLikeResolvedSearchChatter(@Nonnull String current) {
        String compact = current.replaceAll("\\[CQ:[^]]+]", " ")
                .replaceAll("\\s+", "");
        return containsAny(compact,
                "搜不到", "没搜到", "查不到", "没查到", "搜过了", "查过了", "查到了", "找到答案",
                "终于找到", "知道答案", "白茶酱都说了", "白茶酱终于找到答案");
    }

    private static boolean looksLikeReasonableFollowUpQuery(@Nullable String query) {
        if (StringUtils.isNullOrEmptyEx(query)) {
            return false;
        }
        String compact = normalizeComparable(query);
        if (compact.length() < 2 || compact.length() > 80) {
            return false;
        }
        return !containsAny(compact, "当前最新消息", "已阻止", "工具", "系统提示", "systemprompt");
    }

    private static boolean looksLikeGeneralAdviceQuestion(@Nonnull String current) {
        return containsAny(current,
                "有没有好用", "好用的", "哪个好", "哪款", "推荐个", "推荐一个", "推荐下", "推荐一下",
                "怎么办", "怎么弄", "怎么选", "用什么", "该用", "适合")
                && !containsAny(current, "查", "搜", "搜索", "查询", "资料", "网页", "最新", "最近", "新闻");
    }

    @Nonnull
    private static Decision allowsWeather(@Nonnull String current, @Nonnull Map<String, String> args) {
        String location = arg(args, "location");
        boolean weatherIntent = containsAny(current, "天气", "气温", "温度", "下雨", "冷不冷", "热不热");
        if (!weatherIntent) {
            return Decision.block("当前最新消息没有明确要求查询天气，已阻止工具调用。");
        }
        return targetInCurrent(current, location)
                ? Decision.allow()
                : Decision.block("当前最新消息没有包含本次天气查询地点，已阻止沿用旧参数。");
    }

    @Nonnull
    private static Decision allowsGenericMutating(@Nonnull String name, @Nonnull String current, @Nonnull Map<String, String> args) {
        if (!mutatingVerbMatches(name, current)) {
            return Decision.block("当前最新消息没有明确要求或确认执行该修改类能力，已阻止沿用历史参数。");
        }
        return args.values().stream()
                .filter(StringUtils::isNotNullOrEmpty)
                .flatMap(value -> List.of(value.split("\\s+")).stream())
                .map(String::trim)
                .filter(token -> token.length() >= 2)
                .anyMatch(token -> current.contains(token) || current.contains("[CQ:at,qq=" + token + "]"))
                ? Decision.allow()
                : Decision.block("当前最新消息没有包含本次修改类能力的关键参数，已阻止沿用旧参数。");
    }

    private static boolean requiresFreshCurrentMessage(@Nonnull String normalizedName) {
        return normalizedName.startsWith("search_mcmod")
                || normalizedName.equals("web_search")
                || normalizedName.equals("get_weather");
    }

    @Nonnull
    private static String currentText(@Nonnull AgentContext ctx, @Nullable AiCapability capability) {
        String current = ctx.userMessage() != null ? ctx.userMessage() : "";
        if (capability == null || !capability.allowQuotedContext()
                || ctx.bot() == null || ctx.messageContext() == null || ctx.messageContext().originalMsg() == null) {
            return current;
        }
        String quoted = BaniraUtils.getReplyContentString(ctx.bot(), ctx.messageContext().originalMsg());
        if (StringUtils.isNullOrEmptyEx(quoted)) {
            return current;
        }
        return current + "\n引用内容：" + quoted;
    }

    private static boolean isKnownMutating(@Nonnull String normalizedName) {
        return switch (normalizedName) {
            case "vote_mcmod", "push_mcmod", "comment_mcmod", "reply_mcmod_comment", "delete_mcmod_comment",
                 "create_timer", "recall_last_ai_reply", "draw_today_wife", "execute_kanri", "execute_rcon" -> true;
            default -> false;
        };
    }

    private static boolean mutatingVerbMatches(@Nonnull String capabilityName, @Nonnull String current) {
        String lower = current.toLowerCase(Locale.ROOT);
        if (capabilityName.contains("mcmod")) {
            return containsAny(current, "留言", "评论", "红票", "黑票", "推荐")
                    || lower.contains("mcmod") || lower.contains("mc百科");
        }
        if (capabilityName.contains("kanri")) {
            return containsAny(current, "禁言", "解禁", "解除禁言", "名片", "头衔", "精华", "群名", "群名称")
                    || containsAny(lower, "mute", "loud", "card", "tag", "essence");
        }
        if (capabilityName.contains("rcon")) {
            return lower.contains("rcon") || current.contains("服务器命令") || current.contains("执行命令");
        }
        if (capabilityName.contains("wife")) {
            return containsAny(current, "抽", "老婆", "今日老婆") || lower.contains("wife");
        }
        return containsAny(current, "帮我", "请", "设置", "添加", "删除", "执行", "撤回", "修改", "改成");
    }

    private static boolean targetInCurrent(@Nonnull String current, @Nullable String target) {
        return StringUtils.isNotNullOrEmpty(target) && current.contains(target.trim());
    }

    private static boolean argsContainCurrentTarget(@Nonnull String current, @Nullable String args) {
        if (StringUtils.isNullOrEmptyEx(args)) {
            return false;
        }
        for (String token : args.split("\\s+")) {
            String trimmed = token.trim();
            if (trimmed.length() >= 2 && (current.contains(trimmed) || current.contains("[CQ:at,qq=" + trimmed + "]"))) {
                return true;
            }
        }
        return false;
    }

    @Nonnull
    private static String normalizeComparable(@Nullable String value) {
        if (value == null) {
            return "";
        }
        return value.replaceAll("\\[CQ:[^]]+]", " ")
                .replaceAll("[-+][a-zA-Z]{2,16}", " ")
                .replaceAll("[\\p{Punct}！？。。，、~～…·\\s]", "")
                .toLowerCase(Locale.ROOT)
                .trim();
    }

    private static boolean hasMeaningfulTokenOverlap(@Nonnull String current, @Nonnull String query) {
        for (String token : query.split("[\\s，,。！？!?；;：:/\\\\]+")) {
            String normalized = normalizeComparable(token);
            if (normalized.length() >= 2 && current.contains(normalized)) {
                return true;
            }
        }
        if (query.length() >= 4) {
            for (int i = 0; i + 2 <= query.length(); i++) {
                String gram = query.substring(i, i + 2);
                if (current.contains(gram)) {
                    return true;
                }
            }
        }
        return false;
    }

    private static String arg(@Nonnull Map<String, String> args, String... keys) {
        for (String key : keys) {
            String value = args.get(key);
            if (StringUtils.isNotNullOrEmpty(value)) {
                return value.trim();
            }
        }
        return "";
    }

    private static boolean containsAny(@Nonnull String text, String... needles) {
        for (String needle : needles) {
            if (StringUtils.isNotNullOrEmpty(needle) && text.contains(needle)) {
                return true;
            }
        }
        return false;
    }

    @Nonnull
    private static String normalizeName(@Nullable String value) {
        return value == null ? "" : value.trim().toLowerCase(Locale.ROOT);
    }

    public record Decision(boolean allowed, @Nonnull String reason) {
        @Nonnull
        public static Decision allow() {
            return new Decision(true, "");
        }

        @Nonnull
        public static Decision block(@Nonnull String reason) {
            return new Decision(false, reason);
        }
    }
}
