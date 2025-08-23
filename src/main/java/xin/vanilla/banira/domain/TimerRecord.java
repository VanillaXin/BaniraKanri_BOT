package xin.vanilla.banira.domain;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * 定时任务记录
 */
@Data
@Accessors(chain = true)
public class TimerRecord {
    /**
     * 定时任务记录ID
     */
    @TableId(type = IdType.AUTO)
    private Long id;
    /**
     * 机器人ID
     */
    private Long botId;
    /**
     * 群组ID
     */
    private Long groupId = 0L;
    /**
     * 添加人
     */
    private Long creatorId;
    /**
     * 添加时间
     */
    private Long time;
    /**
     * cron表达式
     */
    private String cron;
    /**
     * 回复消息
     */
    private String replyMsg;
    /**
     * 启用状态
     */
    private Boolean enable = true;


    public TimerRecord setGroupId(Long groupId) {
        this.groupId = groupId != null ? groupId : 0L;
        return this;
    }

    public TimerRecord setEnable(Boolean enable) {
        this.enable = enable != null ? enable : false;
        return this;
    }

    public Boolean getEnable() {
        return enable != null && enable;
    }
}
