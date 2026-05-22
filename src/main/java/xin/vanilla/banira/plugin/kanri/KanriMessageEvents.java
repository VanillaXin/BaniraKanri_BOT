package xin.vanilla.banira.plugin.kanri;

import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import com.mikuac.shiro.model.ArrayMsg;
import jakarta.annotation.Nonnull;
import xin.vanilla.banira.domain.BaniraCodeContext;

import java.util.Collections;
import java.util.List;

/**
 * 从 AI 聊天上下文构造群消息事件，供群管指令复用
 */
public final class KanriMessageEvents {

    private KanriMessageEvents() {
    }

    @Nonnull
    public static GroupMessageEvent from(@Nonnull BaniraCodeContext ctx) {
        GroupMessageEvent event = new GroupMessageEvent();
        List<ArrayMsg> arrayMsg = ctx.originalMsg() != null ? ctx.originalMsg() : Collections.emptyList();
        event.setArrayMsg(arrayMsg);
        event.setGroupId(ctx.group() != null ? ctx.group() : 0L);
        event.setUserId(ctx.sender() != null ? ctx.sender() : 0L);
        event.setMessageId(ctx.msgId() != null ? ctx.msgId() : 0);
        event.setMessage(ctx.msg() != null ? ctx.msg() : "");
        event.setTime(ctx.time() != null ? ctx.time() : System.currentTimeMillis() / 1000);
        return event;
    }

}
