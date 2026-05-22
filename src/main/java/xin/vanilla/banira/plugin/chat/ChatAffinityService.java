package xin.vanilla.banira.plugin.chat;

import jakarta.annotation.Nonnull;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.extended.ChatAffinitySettings;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.plugin.chat.agent.AgentContext;
import xin.vanilla.banira.service.IAiAffinityManager;
import xin.vanilla.banira.util.StringUtils;

import java.util.List;
import java.util.Locale;
import java.util.Objects;

/**
 * 好感度读写与轻量消息打分。
 */
@Component
public class ChatAffinityService {

    private static final List<String> STRONG_NEGATIVE_WORDS = List.of(
            "滚", "闭嘴", "爬", "死开", "傻逼", "煞笔", "弱智", "废物", "垃圾", "别说话", "吵死了",
            "shut up", "fuck off", "stupid", "idiot"
    );
    private static final List<String> NEGATIVE_WORDS = List.of(
            "烦你", "讨厌你", "不喜欢你", "没用", "真笨", "笨蛋", "蠢", "错了", "坏", "拉倒",
            "annoying", "useless", "bad bot"
    );
    private static final List<String> POSITIVE_WORDS = List.of(
            "谢谢", "谢啦", "辛苦", "喜欢你", "可爱", "乖", "真好", "厉害", "好棒", "靠谱", "爱你",
            "thanks", "thank you", "good bot", "nice", "cute"
    );

    private final IAiAffinityManager affinityManager;

    public ChatAffinityService(@Nonnull IAiAffinityManager affinityManager) {
        this.affinityManager = Objects.requireNonNull(affinityManager, "affinityManager");
    }

    public int scoreAfterMessage(@Nonnull ChatConfig cfg, @Nonnull AgentContext ctx, @Nonnull String message) {
        ChatAffinitySettings settings = cfg.affinity();
        if (settings == null || !settings.enabled()) {
            return settings != null ? settings.initialScore() : 50;
        }
        int delta = classifyDelta(cfg, settings, message);
        if (delta == 0) {
            return affinityManager.getScore(ctx.botId(), ctx.scopeGroupId(), ctx.senderId(), settings.initialScore());
        }
        return affinityManager.adjustScore(
                ctx.botId(),
                ctx.scopeGroupId(),
                ctx.senderId(),
                settings.initialScore(),
                settings.minScore(),
                settings.maxScore(),
                delta
        );
    }

    private static int classifyDelta(@Nonnull ChatConfig cfg, @Nonnull ChatAffinitySettings settings, @Nonnull String message) {
        if (StringUtils.isNullOrEmptyEx(message)) {
            return 0;
        }
        String normalized = message.toLowerCase(Locale.ROOT);
        GuardRuleSet rules = GuardRuleSet.from(cfg.guard());
        if (containsAny(normalized, rules.lines("strongNegativeWords", STRONG_NEGATIVE_WORDS))) {
            return settings.strongNegativeDelta();
        }
        if (containsAny(normalized, rules.lines("negativeWords", NEGATIVE_WORDS))) {
            return settings.negativeDelta();
        }
        if (containsAny(normalized, rules.lines("positiveWords", POSITIVE_WORDS))) {
            return settings.positiveDelta();
        }
        return 0;
    }

    private static boolean containsAny(@Nonnull String text, @Nonnull List<String> words) {
        for (String word : words) {
            if (text.contains(word)) {
                return true;
            }
        }
        return false;
    }

}
