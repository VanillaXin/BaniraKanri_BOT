package xin.vanilla.banira.plugin;

import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.basic.BaseInstructionsConfig;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.config.entity.group.AIChatGroupConfig;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.enums.EnumMessageType;
import xin.vanilla.banira.plugin.chat.AIChatService;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.service.IMessageRecordManager;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * AI聊天插件
 */
@Slf4j
@Shiro
@Component
public class AIChatPlugin extends BasePlugin {

    private final Map<Long, AIChatService> chatServiceMap = new ConcurrentHashMap<>();
    @Resource
    private IMessageRecordManager messageRecordManager;

    /**
     * 获取帮助信息
     *
     * @param groupId 群组ID
     * @param types   帮助类型
     */
    @Nonnull
    @Override
    public List<String> getHelpInfo(@Nullable Long groupId, @Nonnull String... types) {
        List<String> result = new ArrayList<>();
        String type = CollectionUtils.getFirst(types);
        BaseInstructionsConfig baseIns = BaniraUtils.getBaseIns();
        if (insConfig.get().aiChat().stream().anyMatch(s -> StringUtils.isNullOrEmptyEx(type) || s.equalsIgnoreCase(type))) {
            result.add("AI聊天 - 启用：\n\n" +
                    BaniraUtils.getInsPrefixWithSpace() +
                    insConfig.get().aiChat() +
                    baseIns.enable()
            );
            result.add("AI聊天 - 禁用：\n\n" +
                    BaniraUtils.getInsPrefixWithSpace() +
                    insConfig.get().aiChat() +
                    baseIns.disable()
            );
            result.add("AI聊天 - 刷新：\n\n" +
                    BaniraUtils.getInsPrefixWithSpace() +
                    insConfig.get().aiChat() +
                    baseIns.refresh()
            );
        }
        return result;
    }

    @AnyMessageHandler
    public boolean config(BaniraBot bot, AnyMessageEvent event) {
        if (!BaniraUtils.isGlobalOp(event.getUserId())) return false;
        BaniraCodeContext context = new BaniraCodeContext(bot, event);

        BaseInstructionsConfig baseIns = BaniraUtils.getBaseIns();
        if (super.isCommand(context)
                && insConfig.get().aiChat().stream().anyMatch(s -> super.deleteCommandPrefix(context).startsWith(s + " "))
        ) {
            String argString = super.deleteCommandPrefix(context);
            String[] split = argString.split("\\s+");
            if (split.length < 2) return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());

            String operate = split[1];
            if (baseIns.enable().contains(operate)) {
                AIChatGroupConfig config = BaniraUtils.getGroupConfigOrGlobal(AIChatGroupConfig.class, event.getGroupId());
                if (config.chatConfig() == null) {
                    config.chatConfig(new ChatConfig());
                }
                config.chatConfig().enabled(true);
                if (BaniraUtils.saveGroupConfig()) {
                    bot.sendMsg(event, MsgUtils.builder().reply(event.getMessageId()).text("已生成聊天配置，请手动修改相关配置并保存。").build(), false);
                } else {
                    return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
                }
            } else if (baseIns.disable().contains(operate)) {
                AIChatGroupConfig config = BaniraUtils.getGroupConfigOrGlobal(AIChatGroupConfig.class, event.getGroupId());
                if (config.chatConfig() == null) {
                    config.chatConfig(new ChatConfig());
                }
                config.chatConfig().enabled(false);
                if (BaniraUtils.saveGroupConfig()) {
                    bot.sendMsg(event, MsgUtils.builder().reply(event.getMessageId()).text("已禁用聊天配置。").build(), false);
                } else {
                    return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
                }
            } else if (baseIns.refresh().contains(operate)) {
                this.chatServiceMap.remove(event.getGroupId());
                return bot.setMsgEmojiLikeOk(event.getMessageId());
            } else {
                return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            }
        }

        return false;
    }

    @AnyMessageHandler
    public boolean reply(BaniraBot bot, AnyMessageEvent event) {
        BaniraCodeContext context = new BaniraCodeContext(bot
                , event.getArrayMsg()
                , event.getGroupId()
                , event.getUserId()
                , event.getUserId()
        )
                .msg(event.getMessage())
                .msgId(event.getMessageId())
                .time(event.getTime())
                .msgType(EnumMessageType.getType(event));

        AIChatService chatService = null;
        if (context.msgType() == EnumMessageType.GROUP) {
            AIChatGroupConfig config = BaniraUtils.hasGroupConfig(AIChatGroupConfig.class, event.getGroupId())
                    ? BaniraUtils.getGroupConfigOrGlobal(AIChatGroupConfig.class, event.getGroupId())
                    : null;
            if (config != null && config.chatConfig() != null) {
                chatService = chatServiceMap.computeIfAbsent(event.getGroupId(), k -> new AIChatService(config.chatConfig(), messageRecordManager));
            }
        } else {
            AIChatGroupConfig globalConfig = BaniraUtils.getGroupConfigOrGlobal(AIChatGroupConfig.class, 0L);
            if (globalConfig.chatConfig() != null) {
                chatService = chatServiceMap.computeIfAbsent(0L, k -> new AIChatService(globalConfig.chatConfig(), messageRecordManager));
            }
        }

        if (chatService == null) return false;
        return chatService.generateAndSendReply(bot, context);
    }

}
