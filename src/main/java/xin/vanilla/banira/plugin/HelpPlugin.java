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
import jakarta.annotation.Nullable;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.coder.common.EventCoder;
import xin.vanilla.banira.coder.common.MessageCoder;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 指令帮助插件
 */
@Slf4j
@Shiro
@Component
public class HelpPlugin extends BasePlugin {

    @Autowired(required = false)
    private List<BasePlugin> plugins = new ArrayList<>();
    @Autowired(required = false)
    private List<MessageCoder> msgCoders = new ArrayList<>();
    @Autowired(required = false)
    private List<EventCoder> eventCoders = new ArrayList<>();


    private static final List<String> codeType = List.of(
            "BaniraCode", "baniracode", "bkode", "bk", "code", "特殊码", "bk码"
    );

    @Nonnull
    @Override
    public List<String> getHelpInfo(@Nullable Long groupId, @Nonnull String... types) {
        return List.of();
    }

    @AnyMessageHandler
    public boolean help(BaniraBot bot, AnyMessageEvent event) {
        BaniraCodeContext context = new BaniraCodeContext(bot, event);
        if (super.isCommand(context)
                && insConfig.get().base().help() != null
                && insConfig.get().base().help().stream().anyMatch(ins -> super.deleteCommandPrefix(context).startsWith(ins + " "))
        ) {
            try {

                String argString = super.deleteCommandPrefix(context);
                String[] split = argString.split("\\s+");

                long page = StringUtils.toLong(CollectionUtils.getLast(split), -1);

                String[] type;
                if (split.length == 1) {
                    type = new String[]{};
                } else if (split.length == 2) {
                    if (page != StringUtils.toLong(CollectionUtils.getLast(split))) {
                        type = Arrays.copyOfRange(split, 1, split.length);
                    } else {
                        type = new String[]{};
                    }
                } else if (split.length >= 3) {
                    int len;
                    if (page != StringUtils.toLong(CollectionUtils.getLast(split))) {
                        len = split.length;
                    } else {
                        len = split.length - 1;
                    }
                    type = Arrays.copyOfRange(split, 1, len);
                } else {
                    return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
                }
                if (page <= 0) page = 1;

                LoginInfoResp loginInfoEx = bot.getLoginInfoEx();
                List<Map<String, Object>> msg = new ArrayList<>();
                msg.add(ShiroUtils.generateSingleMsg(event.getUserId(), event.getSender().getNickname(), event.getMessage()));
                List<String> helpMsgList;
                if (type.length > 0 && codeType.contains(CollectionUtils.getOrDefault(type, 0, ""))) {
                    msg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                            , "指令帮助：" + "\n\n" +
                                    BaniraUtils.getInsPrefixWithSpace() +
                                    insConfig.get().base().help() + " " +
                                    "[<指令类型>]" + " " + "[<页数>]" + "\n\n" +
                                    "例子：" + "\n" +
                                    BaniraUtils.getInsPrefixWithSpace() +
                                    insConfig.get().base().help().getFirst() + " " + CollectionUtils.getRandomElement(codeType) + "\n\n" +
                                    "BaniraCode可选属性：\n" +
                                    "$w：将该解析结果写入运行时变量集，$w:变量名称\n" +
                                    "$r：从运行时变量集读取结果作为value，$r:变量名称\n" +
                                    "$auto：与$r配合($r:$auto)，将Code的value中$开头的占位符替换为运行时变量集中存在的值\n" +
                                    "$void：与$w配合($w:xxx,$void:true)，使Code结果仅写入运行时变量集而不体现在回复内容中\n"
                    ));
                    Set<String> coderType = new HashSet<>();
                    if (type.length > 1) {
                        coderType.addAll(BaniraUtils.mutableListOf(type).subList(1, type.length));
                    }
                    helpMsgList = msgCoders.stream()
                            .filter(coder -> coderType.isEmpty() || coderType.contains(coder.getName()))
                            .filter(coder -> StringUtils.isNotNullOrEmpty(coder.getName()))
                            .map(coder -> {
                                String title = String.format("消息码 - %s\n%s", coder.getName(), coder.getDesc());
                                return buildCodeExample(title, coder.getExample());
                            }).collect(Collectors.toList());
                    eventCoders.stream()
                            .filter(coder -> coderType.isEmpty() || coderType.contains(coder.getName()))
                            .filter(coder -> StringUtils.isNotNullOrEmpty(coder.getName()))
                            .map(coder -> {
                                String title = String.format("事件码 - %s\n%s", coder.getName(), coder.getDesc());
                                return buildCodeExample(title, coder.getExample());
                            }).forEach(helpMsgList::add);
                } else {
                    msg.add(ShiroUtils.generateSingleMsg(bot.getSelfId(), loginInfoEx.getNickname()
                            , "指令帮助：" + "\n\n" +
                                    BaniraUtils.getInsPrefixWithSpace() +
                                    insConfig.get().base().help() + " " +
                                    "[<指令类型>]" + " " + "[<页数>]" + "\n\n" +
                                    "例子：" + "\n" +
                                    BaniraUtils.getInsPrefixWithSpace() +
                                    insConfig.get().base().help().getFirst() + " keyword"
                    ));
                    helpMsgList = plugins.stream()
                            .map(plugin -> plugin.getHelpInfo(event.getGroupId(), type))
                            .flatMap(List::stream).toList();
                }

                helpMsgList.stream()
                        .sorted()
                        .skip((page - 1) * 98L)
                        .limit(98L)
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

    private String buildCodeExample(String title, List<String> example) {
        StringBuilder sb = new StringBuilder(title).append("\n\n");
        for (int i = 0; i < example.size(); i++) {
            sb.append("例子");
            if (example.size() > 1) sb.append(i + 1);
            sb.append("：\n");
            sb.append(example.get(i));
            if (i != example.size() - 1) {
                sb.append("\n\n");
            }
        }
        return sb.toString();
    }

}
