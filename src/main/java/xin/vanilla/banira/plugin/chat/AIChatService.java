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
        // 让模型保持更“有人味”的口吻与语境
        prompt.add(SystemMessage.systemMessage("你是一位轻松友善的聊天伙伴，语气自然、口语化，允许适度表情符号和感叹（不要过度），使用第一人称，尽量简短且避免机械感。"));
        prompt.add(SystemMessage.systemMessage("如果问题简单，可随性用一句话回应；遇到不确定可反问或给出简单思考，而不是生硬的模板化回答。"));
        prompt.add(SystemMessage.systemMessage("请参考最近对话上下文，避免重复用户原话；能给出直接答案就不要堆砌前后缀；如果上下文模糊，可先澄清再回答。"));
        prompt.add(SystemMessage.systemMessage("当前时间是：" + DateUtils.toDateTimeString(now)));
        prompt.add(SystemMessage.systemMessage("你的昵称为：" + loginInfoEx.getNickname()));
        prompt.add(SystemMessage.systemMessage("回复应尽量简短"));
        List<MessageRecord> records = normalizeHistoryRecords(getHistoryRecords(bot, ctx));
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
                    Thread.sleep(calcTypingDelayMillis(msg));
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

    /**
     * 清洗并按时间顺序整理历史记录
     */
    private List<MessageRecord> normalizeHistoryRecords(List<MessageRecord> records) {
        if (records == null || records.isEmpty()) return Collections.emptyList();
        List<MessageRecord> cleaned = new ArrayList<>();
        for (MessageRecord record : records) {
            if (record == null) continue;
            if (StringUtils.isNullOrEmptyEx(record.getMsgRecode())) continue;
            cleaned.add(record);
        }
        cleaned.sort(Comparator.comparing(MessageRecord::getTime, Comparator.nullsLast(Long::compareTo)));
        if (cleaned.size() > cfg.historyLimit()) {
            return cleaned.subList(cleaned.size() - cfg.historyLimit(), cleaned.size());
        }
        return cleaned;
    }

    /**
     * 根据消息长度与配置计算较自然的打字延迟
     * 避免机械的固定间隔
     */
    private long calcTypingDelayMillis(String msg) {
        if (StringUtils.isNullOrEmptyEx(msg)) return random.nextLong(80, 120);
        int len = msg.length();
        int minPerChar = Math.max(30, cfg.minTypingSpeed());
        int maxPerChar = Math.max(minPerChar + 1, cfg.maxTypingSpeed());
        // 基于字符数的期望延迟，再加入 0.5~1.2 的抖动
        double jitter = 0.5 + random.nextDouble(0.7);
        long estimate = (long) (len * random.nextInt(minPerChar, maxPerChar) * jitter);
        // 进一步加一小段起始思考时间
        long thinking = random.nextLong(120, 420);
        return Math.min(5000L, Math.max(120L, estimate + thinking));
    }

}
