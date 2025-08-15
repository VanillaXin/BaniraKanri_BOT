package xin.vanilla.banira.domain;

import com.mikuac.shiro.model.ArrayMsg;
import lombok.Data;
import lombok.experimental.Accessors;
import xin.vanilla.banira.plugin.common.BaniraBot;

import java.util.List;

@Data
@Accessors(chain = true)
public class BaniraCodeContext implements Cloneable {
    private final BaniraBot bot;
    /**
     * 原始消息
     */
    private final List<ArrayMsg> originalMsg;
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

    public BaniraCodeContext clone() {
        try {
            return (BaniraCodeContext) super.clone();
        } catch (Exception e) {
            return new BaniraCodeContext(bot, originalMsg)
                    .setGroup(group)
                    .setSender(sender)
                    .setTarget(target)
                    .setMsg(msg)
                    ;
        }
    }
}
