package xin.vanilla.banira.config.entity.extended;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * AI 对单个用户的好感度配置。
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class ChatAffinitySettings {

    /**
     * 是否启用好感度对回复概率的影响。
     */
    private boolean enabled = true;
    /**
     * 首次遇到用户时的默认好感度。
     */
    private int initialScore = 50;
    /**
     * 好感度最小值。
     */
    private int minScore = 0;
    /**
     * 好感度最大值。
     */
    private int maxScore = 100;
    /**
     * 低于该值时降低回复概率。
     */
    private int lowThreshold = 25;
    /**
     * 低于该值时明显降低回复概率。
     */
    private int veryLowThreshold = 10;
    /**
     * 低好感度概率倍率。
     */
    private double lowReplyMultiplier = 0.60;
    /**
     * 极低好感度概率倍率。
     */
    private double veryLowReplyMultiplier = 0.35;
    /**
     * 友好消息增加的好感度。
     */
    private int positiveDelta = 2;
    /**
     * 不友好消息扣除的好感度。
     */
    private int negativeDelta = -4;
    /**
     * 明显恶意消息扣除的好感度。
     */
    private int strongNegativeDelta = -8;

}
