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
import xin.vanilla.banira.plugin.chat.capability.AiCapability;
import xin.vanilla.banira.plugin.chat.capability.AiCapabilityParameter;
import xin.vanilla.banira.plugin.chat.capability.AiCapabilityProvider;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.plugin.help.HelpMessage;
import xin.vanilla.banira.plugin.help.HelpService;
import xin.vanilla.banira.plugin.help.HelpTopic;
import xin.vanilla.banira.plugin.help.HelpTopics;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * 指令帮助插件
 */
@Slf4j
@Shiro
@Component
public class HelpPlugin extends BasePlugin implements AiCapabilityProvider {

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

    @Override
    public void registerAiCapabilities(@Nonnull List<AiCapability> capabilities, Long groupId) {
        capabilities.add(new AiCapability()
                .name("search_help")
                .description("搜索机器人功能帮助。可查询功能名称、子功能及用法说明。")
                .parameterHint("path=功能路径(空格或逗号分隔),page=页码(默认1)")
                .parameters(List.of(
                        AiCapabilityParameter.optional("path", "功能路径，多个层级可用空格或逗号分隔"),
                        AiCapabilityParameter.optional("page", "页码，默认 1")
                ))
                .executor((ctx, args) -> {
                    String pathArg = args.getOrDefault("path", "");
                    long page = 1;
                    try {
                        page = Long.parseLong(args.getOrDefault("page", "1"));
                    } catch (NumberFormatException ignored) {
                    }
                    List<String> path = parseHelpPath(pathArg);
                    return helpService.buildMessages(groupId, path, page).stream()
                            .map(HelpMessage::content)
                            .reduce((a, b) -> a + "\n\n" + b)
                            .orElse("未找到相关帮助。");
                }));
        capabilities.add(new AiCapability()
                .name("list_help_topics")
                .description("列出当前群可用的顶层功能主题名称与别名。")
                .parameterHint("无参数")
                .executor((ctx, args) -> helpService.listTopicNames(groupId)));
    }

    @Nonnull
    private static List<String> parseHelpPath(@Nonnull String pathArg) {
        if (pathArg.isBlank()) {
            return List.of();
        }
        String[] parts = pathArg.contains(",")
                ? pathArg.split(",")
                : pathArg.trim().split("\\s+");
        return Arrays.stream(parts)
                .map(String::trim)
                .filter(part -> !part.isEmpty())
                .toList();
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
            HelpQueryArgs args = parseHelpArgs(split);

            LoginInfoResp loginInfoEx = bot.getLoginInfoEx();
            String defaultNickname = loginInfoEx.getNickname();
            List<Map<String, Object>> msg = new ArrayList<>();
            msg.add(ShiroUtils.generateSingleMsg(
                    event.getUserId(), event.getSender().getNickname(), event.getMessage()
            ));

            if (args.extended()) {
                List<Map<String, Object>> helpNodes = helpService.buildExtendedNodes(
                        event.getGroupId(), args.path(), args.page(), bot.getSelfId(), defaultNickname
                );
                if (helpNodes.isEmpty()) {
                    return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
                }
                msg.addAll(helpNodes);
            } else {
                List<HelpMessage> helpMessages = helpService.buildMessages(event.getGroupId(), args.path(), args.page());
                if (helpMessages.isEmpty()) {
                    return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
                }
                helpMessages.forEach(help -> msg.add(
                        ShiroUtils.generateSingleMsg(
                                bot.getSelfId(),
                                StringUtils.isNotNullOrEmpty(help.senderName()) ? help.senderName() : defaultNickname,
                                MsgUtils.builder().text(help.content()).build()
                        )
                ));
            }

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

    @Nonnull
    private HelpQueryArgs parseHelpArgs(@Nonnull String[] split) {
        if (split.length <= 1) {
            return new HelpQueryArgs(List.of(), 1, false);
        }
        int start = 1;
        boolean extended = false;
        if ("-ex".equalsIgnoreCase(split[1])) {
            extended = true;
            start = 2;
        }
        if (split.length <= start) {
            return new HelpQueryArgs(List.of(), 1, extended);
        }
        int argEnd = split.length;
        long page = 1;
        String last = split[split.length - 1];
        long maybePage = StringUtils.toLong(last, -1);
        if (maybePage >= 1 && String.valueOf(maybePage).equals(last)) {
            page = maybePage;
            argEnd = split.length - 1;
        }
        List<String> path = new ArrayList<>();
        for (int i = start; i < argEnd; i++) {
            path.add(split[i]);
        }
        return new HelpQueryArgs(path, page, extended);
    }

    private record HelpQueryArgs(@Nonnull List<String> path, long page, boolean extended) {
    }

}
