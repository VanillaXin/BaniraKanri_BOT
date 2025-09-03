package xin.vanilla.banira.domain;

import com.mikuac.shiro.model.ArrayMsg;
import lombok.Data;
import lombok.experimental.Accessors;
import xin.vanilla.banira.plugin.common.BaniraBot;

import java.util.List;

@Data
@Accessors(chain = true, fluent = true)
public class BaniraCodeContext implements Cloneable {
    private final BaniraBot bot;
    /**
     * 原始消息
     */
    private final List<ArrayMsg> originalMsg;
    /**
     * 权限获取目标
     */
    private Long opId = 0L;
    /**
     * 群组(若有)
     */
    private Long group;
    /**
     * 消息发送者
     */
    private Long sender;
    /**
     * 作用目标
     */
    private Long target;
    /**
     * 消息内容
     */
    private String msg;
    /**
     * 发送时间
     */
    private Long time;
    /**
     * 消息ID
     */
    private Integer msgId;

    public BaniraCodeContext clone() {
        try {
            return (BaniraCodeContext) super.clone();
        } catch (Exception e) {
            return new BaniraCodeContext(bot, originalMsg)
                    .opId(opId)
                    .group(group)
                    .sender(sender)
                    .target(target)
                    .msg(msg)
                    .time(time)
                    .msgId(msgId)
                    ;
        }
    }
}
