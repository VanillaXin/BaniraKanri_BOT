package xin.vanilla.banira.plugin;

import com.mikuac.shiro.annotation.GroupMessageHandler;
import com.mikuac.shiro.annotation.PrivateMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import com.mikuac.shiro.dto.event.message.MessageEvent;
import com.mikuac.shiro.dto.event.message.PrivateMessageEvent;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.coder.common.BaniraCode;
import xin.vanilla.banira.coder.common.BaniraCodeUtils;
import xin.vanilla.banira.coder.message.ToGroupCode;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.enums.EnumMessageType;
import xin.vanilla.banira.plugin.chat.capability.*;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.plugin.help.HelpTopic;
import xin.vanilla.banira.plugin.help.HelpTopics;
import xin.vanilla.banira.plugin.kanri.KanriHandler;
import xin.vanilla.banira.plugin.kanri.KanriHandlerRegistry;
import xin.vanilla.banira.plugin.kanri.KanriService;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;

/**
 * 群管指令
 */
@Slf4j
@Shiro
@Component
public class KanriPlugin extends BasePlugin implements AiCapabilityProvider {

    @Autowired(required = false)
    private List<KanriHandler> handlers = new ArrayList<>();
    @Resource
    private KanriHandlerRegistry handlerRegistry;
    @Resource
    private KanriService kanriService;
    @Resource
    private ToGroupCode toGroupCode;

    private static final List<String> KANRI_ALIASES = List.of("kanri", "群管");

    @Override
    public void registerHelpTopics(@Nonnull List<HelpTopic> topics, Long groupId) {
        HelpTopic kanri = HelpTopics.of("群管", "群管理相关指令。", 1, KANRI_ALIASES);
        handlers.stream()
                .map(KanriHandler::getHelpSubTopic)
                .forEach(kanri::child);
        topics.add(kanri);
    }

    @Override
    public void registerAiCapabilities(@Nonnull List<AiCapability> capabilities, Long groupId) {
        capabilities.add(new AiCapability()
                .name("list_kanri_actions")
                .description("列出 AI 可调用的群管动作（禁言/解禁/名片/头衔/精华/群名等，不含踢人、管理员名单与权限类操作）。")
                .parameterHint("无参数")
                .access(AiCapabilityAccess.ADMIN)
                .executor((ctx, args) -> kanriService.listAiActions()));
        capabilities.add(new AiCapability()
                .name("execute_kanri")
                .description("执行允许的群管动作。以当前对话发起者为操作者，权限与群管指令一致；支持禁言、解禁、名片、头衔、精华、群名等 AI 白名单动作。")
                .parameterHint("action=动作别名,args=参数字符串(空格分隔；禁言/解禁用 QQ/全员 与时长，名片/头衔用 QQ 与内容，精华用 add/del 与消息，群名用新名称)")
                .parameters(List.of(
                        AiCapabilityParameter.required("action", "群管动作别名，必须是 list_kanri_actions 返回的允许动作"),
                        AiCapabilityParameter.required("args", "动作参数字符串；目标和参数必须来自当前消息、结构化 @ 或引用消息"),
                        AiCapabilityParameter.optional("confirm", "用户明确确认后才可填 true")
                ))
                .access(AiCapabilityAccess.ADMIN)
                .mutationPolicy(AiMutationPolicy.EXPLICIT_CONFIRMATION)
                .confirmationPolicy(AiConfirmationPolicy.ALWAYS)
                .mutating(true)
                .sensitive(true)
                .requireConfirmation(true)
                .executor((ctx, args) -> {
                    try {
                        String action = AiCapabilityArgs.require(args, "action");
                        String argLine = args.getOrDefault("args", "");
                        return kanriService.execute(ctx, action, kanriService.parseArgs(argLine));
                    } catch (IllegalArgumentException e) {
                        return e.getMessage();
                    }
                }));
    }

    @GroupMessageHandler
    public boolean group(BaniraBot bot, GroupMessageEvent event) {
        BaniraCodeContext context = this.decodeToGroupCode(
                new BaniraCodeContext(bot
                        , event.getArrayMsg()
                        , event.getGroupId()
                        , event.getUserId()
                        , event.getUserId()
                )
                        .msg(event.getMessage())
        );

        if (BaniraUtils.isGroupIdValid(context.group())) {
            return execute(bot, event, context.msg(), context.group(), event.getMessageId());
        }
        return false;
    }

    @PrivateMessageHandler
    public boolean friend(BaniraBot bot, PrivateMessageEvent event) {
        BaniraCodeContext context = this.decodeToGroupCode(
                new BaniraCodeContext(bot
                        , event.getArrayMsg()
                        , 0L
                        , event.getUserId()
                        , event.getUserId()
                )
                        .msg(event.getMessage())
        );

        if (BaniraUtils.isGroupIdValid(context.group())) {
            return execute(bot, event, context.msg(), context.group(), event.getMessageId());
        }
        return false;
    }

    private boolean execute(BaniraBot bot, MessageEvent event, String message, long groupId, int msgId) {
        BaniraCodeContext codeContext = new BaniraCodeContext(bot
                , event.getArrayMsg()
                , groupId
                , event.getUserId()
                , event.getUserId()
        )
                .msg(message)
                .msgId(msgId)
                .time(event.getTime())
                .msgType(EnumMessageType.getType(event));

        if (!super.isKanriCommand(codeContext)) return false;
        message = super.replaceKanriCommand(codeContext);

        String[] parts = message.split("\\s+");
        String kanriAction = parts[0].trim().toLowerCase(Locale.ROOT);

        KanriContext context = new KanriContext(event
                , bot
                , groupId
                , event.getUserId()
                , KanriContext.getMsgId(event)
                , KanriContext.getGuildMsgId(event)
                , message.replace(parts[0], "").stripLeading()
        );

        int result = KanriHandler.NIL;
        KanriHandler handler = handlerRegistry.resolve(kanriAction);
        if (handler != null) {
            if (!handler.botHasPermission(context)) {
                result = KanriHandler.BOT_NO_OP;
            } else if (handler.hasPermission(context)) {
                String[] args = Arrays.copyOfRange(parts, 1, parts.length);
                try {
                    result = handler.execute(context, args);
                } catch (Exception e) {
                    LOGGER.error("Kanri command parsing failed", e);
                    result = KanriHandler.FAIL;
                }
            } else {
                result = KanriHandler.NO_OP;
            }
        }

        Integer emoji;
        switch (result) {
            case KanriHandler.NO_OP -> emoji = 123;
            case KanriHandler.BOT_NO_OP -> emoji = 8;
            case KanriHandler.FAIL -> emoji = 67;
            case KanriHandler.SUCCESS -> emoji = 124;
            default -> emoji = null;
        }

        return emoji != null && bot.setMsgEmojiLike(msgId, emoji);
    }

    private BaniraCodeContext decodeToGroupCode(BaniraCodeContext context) {
        BaniraCodeContext clone = context.clone();
        List<BaniraCode> codeList = BaniraCodeUtils.getAllBaniraCode(clone.msg());
        if (CollectionUtils.isNotNullOrEmpty(codeList)) {
            BaniraCode textBaniraCode = BaniraCodeUtils.getTextBaniraCode(codeList);
            if (textBaniraCode != null) {
                clone.msg(textBaniraCode.getData().get("text").getAsString());
                for (int i = 0; i < codeList.size(); i++) {
                    String placeholder = BaniraCodeUtils.placeholder(i);
                    BaniraCode code = codeList.get(i);
                    toGroupCode.execute(clone, code, placeholder);
                    if (clone.msg().contains(placeholder)) {
                        clone.msg(clone.msg().replace(placeholder, code.getRaw()));
                    }
                }
            }
        }
        return clone;
    }

}
