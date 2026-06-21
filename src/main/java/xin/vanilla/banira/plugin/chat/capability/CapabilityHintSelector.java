package xin.vanilla.banira.plugin.chat.capability;

import jakarta.annotation.Nonnull;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.plugin.chat.agent.AgentContext;
import xin.vanilla.banira.util.StringUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Selects a small capability hint list for the current turn.
 */
public final class CapabilityHintSelector {

    private static final int MAX_SELECTED = 10;
    private static final int FALLBACK_SELECTED = 8;
    private static final List<String> CORE_CAPABILITIES = List.of(
            "web_search",
            "search_mcmod",
            "create_timer",
            "draw_today_wife",
            "recall_last_ai_reply",
            "get_group_owner",
            "get_group_summary",
            "get_weather"
    );

    private CapabilityHintSelector() {
    }

    @Nonnull
    public static String describe(@Nonnull AiCapabilityRegistry registry
            , @Nonnull AgentContext ctx
            , @Nonnull ChatConfig chatConfig
            , @Nonnull String currentText
    ) {
        List<AiCapability> available = registry.available(ctx, chatConfig);
        if (available.isEmpty()) {
            return "当前没有可用能力。";
        }
        List<AiCapability> selected = wantsFullCapabilityList(currentText)
                ? available
                : selectRelevant(available, currentText);
        return selected.stream()
                .map(CapabilityHintSelector::format)
                .collect(Collectors.joining("\n"));
    }

    @Nonnull
    static List<AiCapability> selectRelevant(@Nonnull List<AiCapability> capabilities, @Nonnull String currentText) {
        String normalized = normalize(currentText);
        Map<AiCapability, Integer> scores = new LinkedHashMap<>();
        for (AiCapability capability : capabilities) {
            int score = score(capability, normalized);
            if (score > 0) {
                scores.put(capability, score);
            }
        }
        List<AiCapability> selected = scores.entrySet().stream()
                .sorted(Map.Entry.<AiCapability, Integer>comparingByValue(Comparator.reverseOrder())
                        .thenComparing(entry -> entry.getKey().name()))
                .limit(MAX_SELECTED)
                .map(Map.Entry::getKey)
                .collect(Collectors.toCollection(ArrayList::new));
        if (!selected.isEmpty()) {
            return selected;
        }
        for (String name : CORE_CAPABILITIES) {
            capabilities.stream()
                    .filter(cap -> name.equalsIgnoreCase(cap.name()))
                    .findFirst()
                    .ifPresent(selected::add);
            if (selected.size() >= FALLBACK_SELECTED) {
                break;
            }
        }
        return selected.isEmpty()
                ? capabilities.stream().limit(FALLBACK_SELECTED).toList()
                : selected;
    }

    private static int score(@Nonnull AiCapability capability, @Nonnull String text) {
        String name = normalize(capability.name());
        String description = normalize(capability.description());
        int score = 0;
        if (StringUtils.isNotNullOrEmpty(name) && text.contains(name)) {
            score += 10;
        }
        score += keywordScore(capability.name(), text);
        for (String token : splitCapabilityTokens(name + " " + description)) {
            if (token.length() >= 2 && text.contains(token)) {
                score += 1;
            }
        }
        return score;
    }

    private static int keywordScore(String capabilityName, String text) {
        String name = capabilityName == null ? "" : capabilityName.toLowerCase(Locale.ROOT);
        if (name.contains("mcmod")) {
            return containsAny(text, "mc百科", "mcmod", "模组", "整合包", "作者", "水仙辞", "红票", "黑票", "推荐", "留言") ? 8 : 0;
        }
        if (name.contains("timer")) {
            return containsAny(text, "提醒", "定时", "分钟后", "小时后", "明天", "后天") ? 8 : 0;
        }
        if (name.contains("wife")) {
            return containsAny(text, "抽老婆", "老婆", "今日老婆") ? 8 : 0;
        }
        if (name.contains("weather")) {
            return containsAny(text, "天气", "气温", "温度", "下雨", "冷不冷", "热不热") ? 8 : 0;
        }
        if (name.contains("group")) {
            return containsAny(text, "群主", "群名", "群号", "管理员", "成员数", "群信息") ? 8 : 0;
        }
        if (name.contains("web")) {
            return containsAny(text, "搜索", "查询", "查", "学历", "新闻", "资料", "网页") ? 8 : 0;
        }
        if (name.contains("kanri") || name.contains("mute")) {
            return containsAny(text, "禁言", "解禁", "名片", "头衔", "精华", "群名", "群名称", "群管") ? 8 : 0;
        }
        if (name.contains("rcon")) {
            return containsAny(text, "rcon", "服务器命令", "执行命令") ? 8 : 0;
        }
        if (name.contains("recall")) {
            return containsAny(text, "撤回", "撤掉", "删掉", "收回") ? 8 : 0;
        }
        return 0;
    }

    private static boolean wantsFullCapabilityList(@Nonnull String text) {
        String normalized = normalize(text);
        return containsAny(normalized, "你能做什么", "你会什么", "有哪些功能", "列出功能", "能力列表", "所有能力", "listcapabilities");
    }

    @Nonnull
    private static String format(@Nonnull AiCapability capability) {
        String params = StringUtils.isNotNullOrEmpty(capability.parameterHint())
                ? " 参数：" + capability.parameterHint()
                : "";
        return "- " + capability.name() + ": " + capability.description() + params;
    }

    @Nonnull
    private static List<String> splitCapabilityTokens(@Nonnull String text) {
        String[] parts = text.split("[_\\s，,。！？!?；;：:/\\\\()（）\\[\\]【】]+");
        List<String> tokens = new ArrayList<>();
        for (String part : parts) {
            String token = normalize(part);
            if (token.length() >= 2) {
                tokens.add(token);
            }
        }
        return tokens;
    }

    @Nonnull
    private static String normalize(String text) {
        if (text == null) {
            return "";
        }
        return text.replaceAll("\\[CQ:[^]]+]", " ")
                .replaceAll("\\s+", " ")
                .trim()
                .toLowerCase(Locale.ROOT);
    }

    private static boolean containsAny(@Nonnull String text, String... needles) {
        for (String needle : needles) {
            if (StringUtils.isNotNullOrEmpty(needle) && text.contains(needle.toLowerCase(Locale.ROOT))) {
                return true;
            }
        }
        return false;
    }
}
