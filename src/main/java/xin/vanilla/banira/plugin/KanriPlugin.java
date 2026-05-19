package xin.vanilla.banira.plugin;

import com.mikuac.shiro.annotation.GroupMessageHandler;
import com.mikuac.shiro.annotation.PrivateMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import com.mikuac.shiro.dto.event.message.MessageEvent;
import com.mikuac.shiro.dto.event.message.PrivateMessageEvent;
import jakarta.annotation.Nonnull;
import jakarta.annotation.PostConstruct;
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
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.plugin.help.HelpTopic;
import xin.vanilla.banira.plugin.help.HelpTopics;
import xin.vanilla.banira.plugin.kanri.KanriHandler;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.*;

/**
 * 群管指令
 */
@Slf4j
@Shiro
@Component
public class KanriPlugin extends BasePlugin {
    @Autowired(required = false)
    private List<KanriHandler> handlers = new ArrayList<>();
    private final Map<String, KanriHandler> handlerByAction = new HashMap<>();
    @Resource
    private ToGroupCode toGroupCode;

    private static final List<String> KANRI_ALIASES = List.of("kanri", "群管");

    @PostConstruct
    public void initHandlerMap() {
        handlerByAction.clear();
        for (KanriHandler handler : handlers) {
            for (String action : handler.getAction()) {
                String actionKey = action == null ? "" : action.trim().toLowerCase(Locale.ROOT);
                if (StringUtils.isNullOrEmptyEx(actionKey)) {
                    continue;
                }
                KanriHandler existed = handlerByAction.putIfAbsent(actionKey, handler);
                if (existed != null) {
                    if (existed == handler) {
                        LOGGER.warn("Duplicate action '{}' found in handler '{}', ignored", action, handler.getClass().getSimpleName());
                        continue;
                    }
                    LOGGER.warn("Duplicate kanri action '{}' between handlers '{}' and '{}', keep '{}'",
                            action,
                            existed.getClass().getSimpleName(),
                            handler.getClass().getSimpleName(),
                            existed.getClass().getSimpleName());
                }
            }
        }
    }

    @Override
    public void registerHelpTopics(@Nonnull List<HelpTopic> topics, Long groupId) {
        HelpTopic kanri = HelpTopics.of("群管", "群管理相关指令。", 1, KANRI_ALIASES);
        handlers.stream()
                .map(KanriHandler::getHelpSubTopic)
                .forEach(kanri::child);
        topics.add(kanri);
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
        KanriHandler handler = handlerByAction.get(kanriAction);
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
            // no
            case KanriHandler.NO_OP -> emoji = 123;
            // sleep
            case KanriHandler.BOT_NO_OP -> emoji = 8;
            // broken heart
            case KanriHandler.FAIL -> emoji = 67;
            // ok
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
