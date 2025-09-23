package xin.vanilla.banira.domain;

import com.mikuac.shiro.model.ArrayMsg;
import lombok.Data;
import lombok.experimental.Accessors;
import xin.vanilla.banira.plugin.common.BaniraBot;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Data
@Accessors(chain = true, fluent = true)
public class BaniraCodeContext implements Cloneable {
    /**
     * 权限获取目标
     */
    private Long opId = 0L;

    private final BaniraBot bot;
    /**
     * 原始消息
     */
    private final List<ArrayMsg> originalMsg;
    /**
     * 群组
     */
    private final Long _group;
    /**
     * 消息发送者
     */
    private final Long _sender;
    /**
     * 作用目标
     */
    private final Long _target;

    /**
     * 群组(解码后)
     */
    private Long group;
    /**
     * 消息发送者(解码后)
     */
    private Long sender;
    /**
     * 作用目标(解码后)
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
    /**
     * 操作者
     */
    private Long operator;

    /**
     * 关键词记录
     */
    private KeywordRecord keywordRecord;

    /**
     * 运行时参数集
     */
    private final Map<String, String> values = new HashMap<>();

    public BaniraCodeContext(BaniraBot bot, List<ArrayMsg> originalMsg, Long _group, Long _sender, Long _target) {
        this.bot = bot;
        this.originalMsg = originalMsg;
        this._group = _group;
        this.group = _group;
        this._sender = _sender;
        this.sender = _sender;
        this._target = _target;
        this.target = _target;
    }

    public BaniraCodeContext clone() {
        try {
            return (BaniraCodeContext) super.clone();
        } catch (Exception e) {
            return new BaniraCodeContext(bot, originalMsg, _group, _sender, _target)
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
