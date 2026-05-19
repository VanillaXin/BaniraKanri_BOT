package xin.vanilla.banira.plugin;

import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import lombok.extern.slf4j.Slf4j;
import lombok.val;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.plugin.help.HelpTopic;
import xin.vanilla.banira.plugin.help.HelpTopics;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * 戳一戳
 */
@Slf4j
@Shiro
@Component
public class TapPlugin extends BasePlugin {

    @Override
    public void registerHelpTopics(@Nonnull List<HelpTopic> topics, Long groupId) {
        topics.add(HelpTopics.of("戳一戳", "对指定用户发送戳一戳。", 99, insConfig.get().tap())
                .detail(BaniraUtils.getInsPrefixWithSpace() + insConfig.get().tap() + " <QQ号|艾特> ... [<次数>]"));
    }

    @AnyMessageHandler
    public boolean tap(BaniraBot bot, AnyMessageEvent event) {
        BaniraCodeContext context = new BaniraCodeContext(bot, event);
        if (super.isCommand(context)
                && insConfig.get().tap().stream().anyMatch(s -> super.deleteCommandPrefix(context).startsWith(s + " "))
        ) {
            val split = super.deleteCommandPrefix(context).split("\\s+");
            val args = Arrays.copyOfRange(split, 1, split.length);

            val groupId = event.getGroupId();
            val targets = BaniraUtils.getUserIdsWithReply(bot, event.getGroupId(), event.getArrayMsg(), args);
            var num = Math.min(10, Math.max(StringUtils.toInt(CollectionUtils.getLast(args)), 1));

            for (Long targetId : targets) {
                for (int i = 0; i < num; i++) {
                    if (BaniraUtils.isGroupIdValid(groupId)) {
                        bot.sendGroupPoke(groupId, targetId);
                    } else {
                        bot.sendFriendPoke(event.getUserId(), targetId);
                    }
                }
            }

        }
        return false;
    }
}
