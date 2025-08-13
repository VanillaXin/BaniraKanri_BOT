package xin.vanilla.banira.domain;

import lombok.Data;
import lombok.experimental.Accessors;
import xin.vanilla.banira.plugin.common.BaniraBot;

@Data
@Accessors(chain = true)
public class BaniraCodeContext implements Cloneable {
    private final BaniraBot bot;
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
            return new BaniraCodeContext(bot)
                    .setGroup(group)
                    .setSender(sender)
                    .setTarget(target)
                    .setMsg(msg)
                    ;
        }
    }
}
