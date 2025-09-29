package xin.vanilla.banira.plugin.chat;

import com.mikuac.shiro.common.utils.MessageConverser;
import com.mikuac.shiro.common.utils.ShiroUtils;
import com.mikuac.shiro.dto.action.response.LoginInfoResp;
import com.mikuac.shiro.model.ArrayMsg;
import dev.langchain4j.data.message.*;
import dev.langchain4j.model.chat.ChatModel;
import dev.langchain4j.model.chat.response.ChatResponse;
import dev.langchain4j.model.openai.OpenAiChatModel;
import lombok.extern.slf4j.Slf4j;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.enums.EnumMessageType;
import xin.vanilla.banira.mapper.param.MessageRecordQueryParam;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.service.IMessageRecordManager;
import xin.vanilla.banira.start.SpringContextHolder;
import xin.vanilla.banira.util.DateUtils;
import xin.vanilla.banira.util.StringUtils;

import java.time.Duration;
import java.util.*;

@Slf4j
public class AIChatService {

    private final ChatConfig cfg;
    private final ChatModel chatModel;
    private final ReplyDecisionMaker decisionMaker;
    private final MessageSplitter splitter;
    private final Random random = new Random();

    public AIChatService(ChatConfig cfg) {
        this.cfg = Objects.requireNonNull(cfg, "cfg");
        this.chatModel = OpenAiChatModel.builder()
                .apiKey(cfg.apiKey())
                .modelName(StringUtils.isNullOrEmptyEx(cfg.modelName()) ? "gpt-4o" : cfg.modelName())
                .temperature(cfg.temperature())
                .timeout(Duration.ofSeconds(cfg.timeout()))
                .maxRetries(cfg.maxRetries())
                .baseUrl(cfg.baseUrl())
                .build();
        this.decisionMaker = new ReplyDecisionMaker(cfg);
        this.splitter = new MessageSplitter(cfg);
    }

    /**
     * @param ctx 消息上下文
     * @return LLM 生成的纯文本回复
     */
    public String generateReply(BaniraBot bot, BaniraCodeContext ctx) {
        if (!decisionMaker.shouldReply(ctx, bot.isMentioned(ctx.originalMsg()))) return null;
        LoginInfoResp loginInfoEx = bot.getLoginInfoEx();
        Date now = new Date();
        List<ChatMessage> prompt = new ArrayList<>();
        for (String sysPrompt : cfg.systemPrompt()) {
            prompt.add(SystemMessage.from(sysPrompt));
        }
        prompt.add(SystemMessage.systemMessage("当前时间是：" + DateUtils.toDateTimeString(now)));
        prompt.add(SystemMessage.systemMessage("你的昵称为：" + loginInfoEx.getNickname()));
        prompt.add(SystemMessage.systemMessage("回复应尽量简短"));
        List<MessageRecord> records = getHistoryRecords(bot, ctx);
        for (MessageRecord record : records) {
            ChatMessage message = convertMessage(record.getMsgRecode(), bot, record.getGroupId(), record.getSenderId());
            if (message != null) prompt.add(message);
        }
        ChatMessage message = convertMessage(ctx.msg(), bot, ctx.group(), ctx.sender());
        if (message == null) return null;
        prompt.add(message);
        ChatResponse chatResponse = this.chatModel.chat(prompt);
        return chatResponse.aiMessage().text();
    }

    public boolean generateAndSendReply(BaniraBot bot, BaniraCodeContext ctx) {
        String reply = generateReply(bot, ctx);
        if (reply == null) return false;
        // 消息过长，合并转发
        if (reply.length() > cfg.maxForwardLength()) {
            List<String> split = MessageSplitter.split(reply, cfg.maxCharsPerPart(), 99);
            LoginInfoResp loginInfoEx = bot.getLoginInfoEx();
            List<Map<String, Object>> forwardMsg = new ArrayList<>();
            forwardMsg.add(ShiroUtils.generateSingleMsg(ctx.sender(), bot.getUserNameEx(ctx.group(), ctx.sender()), ctx.msg()));
            for (String msg : split) {
                forwardMsg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname(), msg));
            }
            if (ctx.msgType() == EnumMessageType.GROUP) {
                bot.sendGroupForwardMsg(ctx.group(), forwardMsg);
            } else {
                bot.sendPrivateForwardMsg(ctx.sender(), forwardMsg);
            }
        } else {
            for (String msg : splitter.split(reply)) {
                try {
                    // 延时
                    Thread.sleep(this.random.nextInt(cfg.minTypingSpeed(), cfg.maxTypingSpeed()));
                } catch (Exception ignored) {
                }
                if (ctx.msgType() == EnumMessageType.GROUP) {
                    bot.sendGroupMsg(ctx.group(), msg, false);
                } else {
                    bot.sendPrivateMsg(ctx.sender(), msg, false);
                }
            }
        }
        return true;
    }

    private static ChatMessage convertMessage(String msgRecode, BaniraBot bot, Long groupId, Long senderId) {
        ChatMessage result = null;
        String senderName = bot.getUserNameEx(groupId, senderId);
        if (bot.getSelfId() == senderId) {
            result = AiMessage.aiMessage(msgRecode);
        } else {
            List<ArrayMsg> arrayMsgList = MessageConverser.stringToArray(msgRecode);
            List<Content> contents = new ArrayList<>();
            for (ArrayMsg arrayMsg : arrayMsgList) {
                Content content = MessageConvert.toContent(bot, groupId, arrayMsg, true);
                if (content != null) {
                    contents.add(content);
                }
            }
            if (!contents.isEmpty()) {
                result = UserMessage.userMessage(senderName, contents);
            }
        }
        return result;
    }

    private List<MessageRecord> getHistoryRecords(BaniraBot bot, BaniraCodeContext ctx) {
        MessageRecordQueryParam msgRecordParam = new MessageRecordQueryParam(true, 1, cfg.historyLimit());
        msgRecordParam.setBotId(bot.getSelfId());
        msgRecordParam.setMsgType(ctx.msgType().name());
        if (ctx.msgType() == EnumMessageType.GROUP) {
            msgRecordParam.setGroupId(ctx.group());
        } else if (ctx.msgType() == EnumMessageType.MEMBER) {
            msgRecordParam.setGroupId(ctx.group());
            msgRecordParam.setTargetId(ctx.sender());
        } else {
            msgRecordParam.setTargetId(ctx.sender());
        }
        msgRecordParam.addOrderBy(MessageRecordQueryParam.ORDER_ID, false);
        return getMessageRecordManager().getMessageRecordList(msgRecordParam);
    }

    private IMessageRecordManager getMessageRecordManager() {
        return SpringContextHolder.getBean(IMessageRecordManager.class);
    }

}
