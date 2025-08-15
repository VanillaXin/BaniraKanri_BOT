package xin.vanilla.banira.domain;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import lombok.Data;
import lombok.experimental.Accessors;
import xin.vanilla.banira.enums.EnumMessageType;

/**
 * 消息记录
 */
@Data
@Accessors(chain = true)
public class MessageRecord {
    /**
     * 消息记录ID
     */
    @TableId(type = IdType.AUTO)
    private Long id;
    /**
     * 消息ID
     */
    private String msgId;
    /**
     * 机器人ID
     */
    private Long botId;
    /**
     * 发送者ID
     */
    private Long senderId;
    /**
     * 好友ID (仅私聊消息)
     */
    private Long targetId = 0L;
    /**
     * 群组ID
     */
    private Long groupId = 0L;
    /**
     * 发送时间
     */
    private Long time;
    /**
     * 消息内容
     */
    private String msgRaw;
    /**
     * 消息类型
     */
    private EnumMessageType msgType;
    /**
     * 消息内容
     */
    private String msgRecode = "";


    public MessageRecord setMsgId(String msgId) {
        this.msgId = msgId;
        return this;
    }

    public MessageRecord setMsgId(Number msgId) {
        this.msgId = String.valueOf(msgId.longValue());
        return this;
    }

    public MessageRecord setTargetId(Long targetId) {
        this.targetId = targetId != null ? targetId : 0L;
        return this;
    }

    public MessageRecord setGroupId(Long groupId) {
        this.groupId = groupId != null ? groupId : 0L;
        return this;
    }

}
