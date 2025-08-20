package xin.vanilla.banira.plugin;

import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.common.utils.ShiroUtils;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.action.common.ActionData;
import com.mikuac.shiro.dto.action.common.MsgId;
import com.mikuac.shiro.dto.action.response.LoginInfoResp;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * 指令帮助插件
 */
@Slf4j
@Shiro
@Component
public class HelpPlugin extends BasePlugin {

    @Autowired(required = false)
    private List<BasePlugin> plugins = new ArrayList<>();

    @Nonnull
    @Override
    public List<String> getHelpInfo(@Nonnull String type, @Nullable Long groupId) {
        return List.of();
    }

    @AnyMessageHandler
    public boolean help(Bot tob, AnyMessageEvent event) {
        BaniraBot bot = new BaniraBot(tob);
        String message = event.getMessage();
        if (super.isCommand(message)
                && globalConfig.get().instConfig().base().help() != null
                && globalConfig.get().instConfig().base().help().stream().anyMatch(ins -> super.replaceCommand(message).startsWith(ins))
        ) {
            try {

                String argString = super.replaceCommand(message);
                String[] split = argString.split("\\s+");

                long page = StringUtils.toLong(CollectionUtils.getLast(split), 1L);

                String type;
                if (split.length == 1) {
                    type = "";
                } else if (split.length == 2 && page != StringUtils.toLong(split[1])) {
                    type = split[1];
                } else if (split.length == 3) {
                    type = split[1];
                } else {
                    return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
                }

                LoginInfoResp loginInfoEx = bot.getLoginInfoEx();
                List<Map<String, Object>> msg = new ArrayList<>();
                msg.add(ShiroUtils.generateSingleMsg(event.getUserId(), event.getSender().getNickname(), event.getMessage()));

                plugins.stream()
                        .map(plugin -> plugin.getHelpInfo(type, event.getGroupId()))
                        .flatMap(List::stream)
                        .sorted()
                        .skip((page - 1) * 99L)
                        .limit(99L)
                        .forEach(help -> msg.add(
                                ShiroUtils.generateSingleMsg(
                                        bot.getSelfId()
                                        , loginInfoEx.getNickname()
                                        , MsgUtils.builder().text(help).build()
                                )
                        ));

                ActionData<MsgId> msgId = bot.sendForwardMsg(event, msg);
                return bot.isActionDataMsgIdNotEmpty(msgId);
            } catch (Exception e) {
                LOGGER.error("Failed to generate help", e);
                return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            }
        }
        return false;
    }

}
