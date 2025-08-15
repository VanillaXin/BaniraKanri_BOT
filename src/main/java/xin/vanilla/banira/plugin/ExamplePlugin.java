package xin.vanilla.banira.plugin;

import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.GroupMessageHandler;
import com.mikuac.shiro.annotation.MessageHandlerFilter;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.mapper.param.MessageRecordQueryParam;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.service.IMessageRecordManager;

import java.util.List;

@Slf4j
@Shiro
@Component
public class ExamplePlugin extends BasePlugin {

    @Resource
    private IMessageRecordManager messageRecordManager;

    /**
     * 被AT时触发回复
     *
     * @param tob   机器人实例
     * @param event 消息事件
     * @return 是否拦截事件传递
     */
    @AnyMessageHandler
    @MessageHandlerFilter(cmd = "/hello")
    public boolean hello(Bot tob, AnyMessageEvent event) {
        BaniraBot bot = new BaniraBot(tob);
        // LOGGER.debug(event.getMessage());
        bot.sendMsg(event, "Hello Excel!", false);
        return false;
    }

    @GroupMessageHandler
    @MessageHandlerFilter(cmd = "/bk test page")
    public void testPage(Bot tob, GroupMessageEvent event) {
        MessageRecordQueryParam param = new MessageRecordQueryParam(false, 1, 10);
        List<MessageRecord> list = messageRecordManager.getMessageRecordList(param);
        System.out.println(list);
    }

    @GroupMessageHandler
    @MessageHandlerFilter(cmd = "/bk test limit")
    public void testLimit(Bot tob, GroupMessageEvent event) {
        MessageRecordQueryParam param = new MessageRecordQueryParam();
        param.setLimit(5);
        param.setOffset(0);
        List<MessageRecord> list = messageRecordManager.getMessageRecordList(param);
        System.out.println(list);
    }

    @GroupMessageHandler
    @MessageHandlerFilter(cmd = "/bk test face")
    public void testFace(Bot tob, GroupMessageEvent event) {
        BaniraBot bot = new BaniraBot(tob);
        bot.sendGroupMsg(event.getGroupId()
                , MsgUtils.builder().face(123).build()
                , false
        );
        bot.sendGroupMsg(event.getGroupId()
                , MsgUtils.builder().face(67).build()
                , false
        );
    }

}
