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
     * 消息记录NOS
     */
    private String nos;
    /**
     * 机器人ID
     */
    private Long bot;
    /**
     * 发送者ID
     */
    private Long sender;
    /**
     * 群组ID
     */
    private Long target;
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
}
