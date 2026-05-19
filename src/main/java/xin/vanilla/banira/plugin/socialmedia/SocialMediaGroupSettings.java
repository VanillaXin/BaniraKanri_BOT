package xin.vanilla.banira.plugin.socialmedia;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.List;

/**
 * 社交媒体群组级别配置
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class SocialMediaGroupSettings {

    /**
     * 识别到可解析内容后直接触发
     */
    private boolean triggerDirect;
    /**
     * at机器人或解析指令 + 回复消息 时触发（解析被回复内容）
     */
    private boolean triggerReplyInvoke;
    /**
     * 消息表情回调触发（解析被点赞消息）
     */
    private boolean triggerEmojiLikeNotice;
    /**
     * 允许触发的 emojiId 列表
     */
    private List<Long> emojiIds;
    /**
     * 回复方式：forward | detail | video
     */
    private String replyMode;
    /**
     * 识别到可解析内容时，是否自动回复 emojiIds[0]
     */
    private boolean replyEmojiOnRecognize;

    {
        this.triggerDirect = false;
        this.triggerReplyInvoke = true;
        this.triggerEmojiLikeNotice = true;
        this.emojiIds = BaniraUtils.mutableListOf(10068L);
        this.replyMode = "forward";
        this.replyEmojiOnRecognize = true;
    }
}
