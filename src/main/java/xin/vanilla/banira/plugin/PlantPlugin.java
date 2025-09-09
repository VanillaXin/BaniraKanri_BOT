package xin.vanilla.banira.plugin;

import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import jakarta.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.PlantCipher;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * 花言草语
 */
@Slf4j
@Shiro
@Component
public class PlantPlugin extends BasePlugin {

    /**
     * 获取帮助信息
     *
     * @param groupId 群组ID
     * @param types   帮助类型
     */
    @Nonnull
    @Override
    public List<String> getHelpInfo(Long groupId, @Nonnull String... types) {
        List<String> result = new ArrayList<>();
        String type = CollectionUtils.getFirst(types);
        if (insConfig.get().plant().stream().anyMatch(s -> StringUtils.isNullOrEmptyEx(type) || s.equalsIgnoreCase(type))) {
            result.add("花言草语：\n" +
                    "将消息内容进行植物编码。\n\n" +
                    BaniraUtils.getInsPrefixWithSpace() +
                    insConfig.get().plant()
            );
        }
        return result;
    }

    @AnyMessageHandler
    public boolean code(BaniraBot bot, AnyMessageEvent event) {
        String message = event.getMessage();
        String msg = super.replaceCommand(message);
        if (super.isCommand(message)
                && insConfig.get().plant().stream().anyMatch(msg::startsWith)
        ) {
            String content;
            if (BaniraUtils.hasReply(event.getArrayMsg())) {
                content = replaceReply(bot.getReplyContentString(event.getArrayMsg()));
            } else {
                String[] split = msg.split("\\s");
                if (split.length > 1)
                    content = msg.replaceAll("^" + StringUtils.escapeExprSpecialWord(split[0]) + "\\s", "");
                else
                    return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            }
            if (content.startsWith("阁下请喝") && content.endsWith("茶")) {
                bot.sendMsg(event
                        , MsgUtils.builder()
                                .reply(event.getMessageId())
                                .text(PlantCipher.decode(content.replaceAll("^阁下请喝|茶$", "")))
                                .build()
                        , false
                );
            } else {
                bot.sendMsg(event
                        , MsgUtils.builder()
                                .reply(event.getMessageId())
                                .text("阁下请喝")
                                .text(PlantCipher.encode(content))
                                .text("茶")
                                .build()
                        , false
                );
            }
        }
        return false;
    }
}
