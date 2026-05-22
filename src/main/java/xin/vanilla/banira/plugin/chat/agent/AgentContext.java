package xin.vanilla.banira.plugin.chat.agent;

import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.enums.EnumMessageType;
import xin.vanilla.banira.plugin.common.BaniraBot;

/**
 * Agent 单次请求上下文
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
public class AgentContext {

    private BaniraBot bot;
    private BaniraCodeContext messageContext;
    private Long groupId;
    private Long senderId;
    private EnumMessageType msgType;
    private String userMessage;
    private String msgId;
    /**
     * 本回合已由确定性链路执行的群管结果，供 LLM 组织回复（勿直接照搬）。
     */
    private String kanriFeedback;
    /**
     * 本回合禁言是否已由 {@link xin.vanilla.banira.plugin.chat.KanriMuteFollowUpHandler} 成功执行。
     */
    private boolean kanriMuteSucceeded;

    public static AgentContext from(BaniraBot bot, BaniraCodeContext ctx) {
        return new AgentContext()
                .bot(bot)
                .messageContext(ctx)
                .groupId(ctx.group())
                .senderId(ctx.sender())
                .msgType(ctx.msgType())
                .userMessage(ctx.msg())
                .msgId(ctx.msgId() != null ? String.valueOf(ctx.msgId()) : "");
    }

    public long botId() {
        return bot.getSelfId();
    }

    public long scopeGroupId() {
        return msgType == EnumMessageType.GROUP ? groupId : 0L;
    }

}
