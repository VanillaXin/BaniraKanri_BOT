package xin.vanilla.banira.plugin;

import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import jakarta.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.domain.KeyValue;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.plugin.help.HelpTopic;
import xin.vanilla.banira.plugin.help.HelpTopics;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.PlantCipher;
import xin.vanilla.banira.util.StringUtils;

import java.util.List;

/**
 * 花言草语
 */
@Slf4j
@Shiro
@Component
public class PlantPlugin extends BasePlugin {

    @Override
    public void registerHelpTopics(@Nonnull List<HelpTopic> topics, Long groupId) {
        String cmd = insConfig.get().plant().getFirst();
        String plantCmd = BaniraUtils.getInsPrefixWithSpace() + cmd;
        topics.add(HelpTopics.of("花言草语", "将消息内容进行植物编码/解码。", 99, insConfig.get().plant())
                .detail("用法1（回复消息）：\n" + plantCmd + "（回复要编码/解码的消息）\n\n"
                        + "用法2（跟内容）：\n" + plantCmd + " <要编码/解码的内容>"));
    }

    @AnyMessageHandler
    public boolean code(BaniraBot bot, AnyMessageEvent event) {
        BaniraCodeContext context = new BaniraCodeContext(bot, event);
        String msg = super.deleteCommandPrefix(context);
        if (super.isCommand(context)
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
            try {

                if (PlantCipher.isAroundLocator(content)) {
                    bot.sendMsg(event
                            , MsgUtils.builder()
                                    .reply(event.getMessageId())
                                    .text(PlantCipher.decode(content))
                                    .build()
                            , false
                    );
                } else {
                    KeyValue<String, String> locator = CollectionUtils.getRandomElement(PlantCipher.LOCATOR);
                    bot.sendMsg(event
                            , MsgUtils.builder()
                                    .reply(event.getMessageId())
                                    .text(locator.getKey())
                                    .text(PlantCipher.encode(content))
                                    .text(locator.getValue())
                                    .build()
                            , false
                    );
                }
            } catch (Exception e) {
                LOGGER.error("Failed to plant", e);
                return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            }
        }
        return false;
    }
}
