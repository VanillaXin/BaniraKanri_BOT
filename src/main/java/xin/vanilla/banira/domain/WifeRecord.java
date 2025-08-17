package xin.vanilla.banira.domain;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * 抽老婆记录
 */
@Data
@Accessors(chain = true)
public class WifeRecord {
    /**
     * 抽老婆记录ID
     */
    @TableId(type = IdType.AUTO)
    private Long id;
    /**
     * 消息ID
     */
    private String msgId;
    /**
     * 群组ID
     */
    private Long groupId;
    /**
     * 发送者ID
     */
    private Long senderId;
    /**
     * 抽取时间
     */
    private Long time;
    /**
     * 老婆ID
     */
    private Long wifeId;
    /**
     * 老婆名称
     */
    private String wifeName;
    /**
     * 昵称
     */
    private String wifeNick;

}
