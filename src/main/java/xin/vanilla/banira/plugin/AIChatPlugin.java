package xin.vanilla.banira.plugin;

import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.basic.BaseInstructionsConfig;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.config.entity.group.AIChatGroupConfig;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.enums.EnumMessageType;
import xin.vanilla.banira.plugin.chat.AIChatService;
import xin.vanilla.banira.plugin.chat.AIChatServiceFactory;
import xin.vanilla.banira.plugin.chat.ChatConfigSupport;
import xin.vanilla.banira.plugin.chat.ChatRecallService;
import xin.vanilla.banira.plugin.chat.capability.*;
import xin.vanilla.banira.plugin.chat.web.WeatherService;
import xin.vanilla.banira.plugin.chat.web.WebSearchService;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.plugin.help.HelpTopic;
import xin.vanilla.banira.plugin.help.HelpTopics;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.JsonUtils;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

/**
 * AI聊天插件
 */
@Slf4j
@Shiro
@Component
public class AIChatPlugin extends BasePlugin implements AiCapabilityProvider {

    private final Map<Long, CachedChatService> chatServiceMap = new ConcurrentHashMap<>();
    @Resource
    private AIChatServiceFactory chatServiceFactory;
    @Resource
    private AiCapabilityRegistry capabilityRegistry;
    @Resource
    private ChatRecallService chatRecallService;
    @Resource
    private WebSearchService webSearchService;
    @Resource
    private WeatherService weatherService;

    @Override
    public void registerHelpTopics(@Nonnull List<HelpTopic> topics, Long groupId) {
        BaseInstructionsConfig base = BaniraUtils.getBaseIns();
        String prefix = BaniraUtils.getInsPrefixWithSpace();
        List<String> aiChat = insConfig.get().aiChat();
        String cmd = aiChat.getFirst();
        String aiCmd = prefix + cmd;
        topics.add(HelpTopics.of("AI聊天", "AI 对话功能。", 100, aiChat)
                .child(HelpTopics.sub("对话", "启用后自动响应对话消息。", 1, List.of("对话", "聊天"),
                        "启用后在群内或私聊发送普通消息即可触发 AI 回复。"))
                .child(HelpTopics.opEnable(base, aiCmd + " " + base.enable().getFirst()))
                .child(HelpTopics.opDisable(base, aiCmd + " " + base.disable().getFirst()))
                .child(HelpTopics.opRefresh(base, aiCmd + " " + base.refresh().getFirst())));
    }

    @Override
    public void registerAiCapabilities(@Nonnull List<AiCapability> capabilities, Long groupId) {
        capabilities.add(new AiCapability()
                .name("recall_last_ai_reply")
                .description("撤回我在当前群或私聊里上一轮发送的回复。用户说“撤回你刚刚发的消息”“撤回上一条”时使用；只会撤回我自己的消息。")
                .parameterHint("count=可选数字；不填则撤回上一轮回复的全部消息")
                .parameters(List.of(
                        AiCapabilityParameter.optional("count", "要撤回的消息条数；不填则撤回上一轮全部回复")
                ))
                .resultMode(AiCapabilityResultMode.DIRECT_SEND)
                .mutationPolicy(AiMutationPolicy.CURRENT_MESSAGE_INTENT)
                .access(AiCapabilityAccess.ADMIN)
                .mutating(true)
                .executor((ctx, args) -> chatRecallService.recallLastAiReply(ctx, args)));
        capabilities.add(new AiCapability()
                .name("web_search")
                .description("搜索公开网页资料，适合查询人物、学历、新闻、百科外的信息；只返回搜索结果摘要和链接。")
                .parameterHint("query=搜索关键词")
                .parameters(List.of(
                        AiCapabilityParameter.required("query", "搜索关键词，必须来自当前问题")
                ))
                .access(AiCapabilityAccess.PUBLIC)
                .allowQuotedContext(true)
                .executor((ctx, args) -> webSearchService.search(args.getOrDefault("query", ""))));
        capabilities.add(new AiCapability()
                .name("get_weather")
                .description("查询指定城市当前天气和今日温度。用户问天气、温度、下雨、冷不冷、热不热时优先使用。")
                .parameterHint("location=城市名")
                .parameters(List.of(
                        AiCapabilityParameter.required("location", "城市名，例如 成都、北京、上海")
                ))
                .access(AiCapabilityAccess.PUBLIC)
                .executor((ctx, args) -> weatherService.currentWeather(args.getOrDefault("location", ""))));
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
                AIChatGroupConfig config = BaniraUtils.getGroupConfigForEdit(AIChatGroupConfig.class, event.getGroupId());
                ChatConfig globalChat = BaniraUtils.getGroupConfigOrGlobal(AIChatGroupConfig.class, 0L).chatConfig();
                config.chatConfig(ChatConfigSupport.prepareEnable(config.chatConfig(), globalChat));
                if (BaniraUtils.saveGroupConfig()) {
                    invalidateChatCaches(event.getGroupId());
                    return bot.setMsgEmojiLikeOk(event.getMessageId());
                }
                return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            } else if (baseIns.disable().contains(operate)) {
                AIChatGroupConfig config = BaniraUtils.getGroupConfigForEdit(AIChatGroupConfig.class, event.getGroupId());
                if (config.chatConfig() == null) {
                    config.chatConfig(ChatConfigSupport.createEnabledTemplate().enabled(false));
                } else {
                    config.chatConfig().enabled(false);
                }
                if (BaniraUtils.saveGroupConfig()) {
                    invalidateChatCaches(event.getGroupId());
                    return bot.setMsgEmojiLikeOk(event.getMessageId());
                }
                return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            } else if (baseIns.refresh().contains(operate)) {
                invalidateChatCaches(event.getGroupId());
                return bot.setMsgEmojiLikeOk(event.getMessageId());
            } else {
                return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            }
        }

        return false;
    }

    @AnyMessageHandler
    public boolean reply(BaniraBot bot, AnyMessageEvent event) {
        if (event.getUserId() == bot.getSelfId()) {
            return false;
        }

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

        boolean directMentioned = bot.isMentioned(context.originalMsg());
        if (super.isCommand(context) && !directMentioned) {
            return false;
        }

        ChatConfig chatConfig = null;
        long cacheKey = context.msgType() == EnumMessageType.GROUP ? event.getGroupId() : 0L;
        if (context.msgType() == EnumMessageType.GROUP) {
            AIChatGroupConfig config = BaniraUtils.getGroupConfigOrGlobal(AIChatGroupConfig.class, event.getGroupId());
            chatConfig = config != null ? config.chatConfig() : null;
        } else {
            AIChatGroupConfig globalConfig = BaniraUtils.getGroupConfigOrGlobal(AIChatGroupConfig.class, 0L);
            chatConfig = globalConfig != null ? globalConfig.chatConfig() : null;
        }

        if (chatConfig == null || !chatConfig.enabled()) {
            return false;
        }
        AIChatService chatService = getChatService(cacheKey, chatConfig);
        if (chatService == null) return false;
        return chatService.generateAndSendReply(bot, context);
    }

    private void invalidateChatCaches(long groupId) {
        chatServiceMap.remove(groupId);
        chatServiceMap.remove(0L);
        capabilityRegistry.invalidate();
    }

    private AIChatService getChatService(long cacheKey, ChatConfig chatConfig) {
        String fingerprint = fingerprint(chatConfig);
        CachedChatService cached = chatServiceMap.get(cacheKey);
        if (cached != null && Objects.equals(cached.fingerprint(), fingerprint)) {
            return cached.service();
        }
        AIChatService service = chatServiceFactory.create(chatConfig);
        chatServiceMap.put(cacheKey, new CachedChatService(fingerprint, service));
        LOGGER.info("AI chat service cache refreshed: group={}, fingerprint={}", cacheKey, Integer.toHexString(fingerprint.hashCode()));
        return service;
    }

    private static String fingerprint(ChatConfig chatConfig) {
        String json = JsonUtils.toJsonString(chatConfig);
        return json != null ? json : String.valueOf(System.identityHashCode(chatConfig));
    }

    private record CachedChatService(String fingerprint, AIChatService service) {
    }

}
