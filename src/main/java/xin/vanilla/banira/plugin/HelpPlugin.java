package xin.vanilla.banira.plugin;

import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.common.utils.ShiroUtils;
import com.mikuac.shiro.dto.action.common.ActionData;
import com.mikuac.shiro.dto.action.common.MsgId;
import com.mikuac.shiro.dto.action.response.LoginInfoResp;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.coder.common.EventCoder;
import xin.vanilla.banira.coder.common.MessageCoder;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.plugin.help.HelpMessage;
import xin.vanilla.banira.plugin.help.HelpService;
import xin.vanilla.banira.plugin.help.HelpTopic;
import xin.vanilla.banira.plugin.help.HelpTopics;
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

    private static final List<String> CODE_ALIASES = List.of(
            "BaniraCode", "baniracode", "bkode", "bk", "code", "特殊码", "bk码"
    );

    @Resource
    private HelpService helpService;
    @Autowired(required = false)
    private List<MessageCoder> msgCoders = new ArrayList<>();
    @Autowired(required = false)
    private List<EventCoder> eventCoders = new ArrayList<>();

    @Override
    public void registerHelpTopics(@Nonnull List<HelpTopic> topics, Long groupId) {
        HelpTopic codeTopic = HelpTopics.of("BaniraCode", "消息码与事件码，用于高级消息编排。", 90, CODE_ALIASES);
        msgCoders.stream()
                .filter(coder -> StringUtils.isNotNullOrEmpty(coder.getName()))
                .forEach(coder -> codeTopic.child(
                        HelpTopics.sub(
                                "消息码 - " + coder.getName(),
                                coder.getDesc(),
                                100,
                                List.of(coder.getName()),
                                buildCodeDetail(coder.getDesc(), coder.getExample())
                        )
                ));
        eventCoders.stream()
                .filter(coder -> StringUtils.isNotNullOrEmpty(coder.getName()))
                .forEach(coder -> codeTopic.child(
                        HelpTopics.sub(
                                "事件码 - " + coder.getName(),
                                coder.getDesc(),
                                100,
                                List.of(coder.getName()),
                                buildCodeDetail(coder.getDesc(), coder.getExample())
                        )
                ));
        codeTopic.detail(
                """
                        BaniraCode 可选属性：
                        $w：将该解析结果写入运行时变量集，$w:变量名称
                        $r：从运行时变量集读取结果作为 value，$r:变量名称
                        $auto：与 $r 配合($r:$auto)，将 Code 的 value 中 $ 开头的占位符替换为运行时变量集中存在的值
                        $void：与 $w 配合($w:xxx,$void:true)，使 Code 结果仅写入运行时变量集而不体现在回复内容中"""
        );
        topics.add(codeTopic);
    }

    @AnyMessageHandler
    public boolean help(BaniraBot bot, AnyMessageEvent event) {
        BaniraCodeContext context = new BaniraCodeContext(bot, event);
        if (!super.isCommand(context)
                || insConfig.get().base().help() == null
                || insConfig.get().base().help().stream().noneMatch(ins -> {
            String s = super.deleteCommandPrefix(context);
            return s.startsWith(ins + " ") || s.equals(ins);
        })) {
            return false;
        }

        try {
            String argString = super.deleteCommandPrefix(context);
            String[] split = argString.split("\\s+");
            long page = StringUtils.toLong(CollectionUtils.getLast(split), -1);

            String featureAlias = null;
            String subAlias = null;
            if (split.length == 1) {
                page = 1;
            } else if (split.length == 2) {
                if (page == StringUtils.toLong(CollectionUtils.getLast(split))) {
                    page = Math.max(page, 1);
                } else {
                    featureAlias = split[1];
                    page = 1;
                }
            } else {
                int argEnd = split.length;
                if (page == StringUtils.toLong(CollectionUtils.getLast(split))) {
                    argEnd = split.length - 1;
                } else {
                    page = 1;
                }
                if (argEnd >= 2) {
                    featureAlias = split[1];
                }
                if (argEnd >= 3) {
                    subAlias = split[2];
                }
            }

            List<HelpMessage> helpMessages = helpService.buildMessages(event.getGroupId(), featureAlias, subAlias, page);
            if (helpMessages.isEmpty()) {
                return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
            }

            LoginInfoResp loginInfoEx = bot.getLoginInfoEx();
            String defaultNickname = loginInfoEx.getNickname();
            List<Map<String, Object>> msg = new ArrayList<>();
            msg.add(ShiroUtils.generateSingleMsg(
                    event.getUserId(), event.getSender().getNickname(), event.getMessage()
            ));
            helpMessages.forEach(help -> msg.add(
                    ShiroUtils.generateSingleMsg(
                            bot.getSelfId(),
                            StringUtils.isNotNullOrEmpty(help.senderName()) ? help.senderName() : defaultNickname,
                            MsgUtils.builder().text(help.content()).build()
                    )
            ));

            ActionData<MsgId> msgId = bot.sendForwardMsg(event, msg);
            return bot.isActionDataMsgIdNotEmpty(msgId);
        } catch (Exception e) {
            LOGGER.error("Failed to generate help", e);
            return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
        }
    }

    private String buildCodeDetail(String desc, List<String> example) {
        StringBuilder sb = new StringBuilder();
        if (StringUtils.isNotNullOrEmpty(desc)) {
            sb.append(desc).append("\n\n");
        }
        for (int i = 0; i < example.size(); i++) {
            sb.append("例子");
            if (example.size() > 1) {
                sb.append(i + 1);
            }
            sb.append("：\n").append(example.get(i));
            if (i != example.size() - 1) {
                sb.append("\n\n");
            }
        }
        return sb.toString();
    }

}
