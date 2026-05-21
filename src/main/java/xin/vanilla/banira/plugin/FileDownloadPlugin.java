package xin.vanilla.banira.plugin;

import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import jakarta.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.domain.BaniraCodeContext;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.plugin.filedownload.*;
import xin.vanilla.banira.plugin.help.HelpTopic;
import xin.vanilla.banira.plugin.help.HelpTopics;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * 文件下载并发送至对话
 */
@Slf4j
@Shiro
@Component
public class FileDownloadPlugin extends BasePlugin {
    @Autowired
    private FileDownloadService fileDownloadService;
    @Autowired
    private FileDownloadPageProbeService pageProbeService;
    @Autowired
    private FileDownloadSelectionStore selectionStore;

    @Override
    public void registerHelpTopics(@Nonnull List<HelpTopic> topics, Long groupId) {
        String cmd = insConfig.get().fileDownload().getFirst();
        String fileCmd = BaniraUtils.getInsPrefixWithSpace() + cmd;
        topics.add(HelpTopics.of("文件下载", "从链接下载文件并发送到当前对话。", 99, insConfig.get().fileDownload())
                .detail("用法：\n" + fileCmd + " <URL>"));
    }

    /**
     * 回复序号选择要下载的文件
     */
    @AnyMessageHandler
    public boolean select(BaniraBot bot, AnyMessageEvent event) {
        if (!BaniraUtils.hasReply(event.getArrayMsg())) {
            return false;
        }
        if (!hasPermission(event)) {
            return false;
        }
        Long replyId = BaniraUtils.getReplyId(event.getArrayMsg());
        Optional<FileDownloadSelectionSession> sessionOpt = selectionStore.findByMessageId(replyId);
        if (sessionOpt.isEmpty()) {
            return false;
        }
        FileDownloadSelectionSession session = sessionOpt.get();
        if (!Objects.equals(session.userId(), event.getUserId())) {
            return bot.setMsgEmojiLikeNo(event.getMessageId());
        }
        Integer index = parseSelectionIndex(event.getMessage());
        if (index == null || index < 1 || index > session.candidates().size()) {
            return FileDownloadFeedback.fail(bot, event, "请输入 1-" + session.candidates().size() + " 之间的序号");
        }
        FileDownloadCandidate candidate = session.candidates().get(index - 1);
        selectionStore.remove(session.messageId());
        if (!fileDownloadService.submitDirect(bot, event, candidate.url())) {
            return FileDownloadFeedback.fail(bot, event, "当前下载任务已达上限，请稍后再试");
        }
        return FileDownloadFeedback.accept(bot, event.getMessageId());
    }

    @AnyMessageHandler
    public boolean download(BaniraBot bot, AnyMessageEvent event) {
        BaniraCodeContext context = new BaniraCodeContext(bot, event);
        if (!super.isCommand(context)) {
            return false;
        }
        String commandBody = super.deleteCommandPrefix(context).trim();
        List<String> aliases = insConfig.get().fileDownload();
        if (aliases.stream().noneMatch(alias -> commandBody.equals(alias) || commandBody.startsWith(alias + " "))) {
            return false;
        }
        if (!hasPermission(event)) {
            return bot.setMsgEmojiLikeNo(event.getMessageId());
        }

        List<String> urls = collectUrls(bot, event, commandBody, aliases);
        if (CollectionUtils.isNullOrEmpty(urls)) {
            return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
        }

        List<String> accepted = new ArrayList<>();
        for (String url : urls) {
            String resolvedUrl = pageProbeService.resolvePageUrl(url);
            boolean submitted = pageProbeService.matchesPage(resolvedUrl)
                    ? pageProbeService.submitProbe(bot, event, resolvedUrl)
                    : fileDownloadService.submit(bot, event, url);
            if (submitted) {
                accepted.add(url);
            }
        }

        if (accepted.isEmpty()) {
            return FileDownloadFeedback.fail(bot, event, "当前下载任务已达上限，请稍后再试");
        }
        return FileDownloadFeedback.accept(bot, event.getMessageId());
    }

    private boolean hasPermission(AnyMessageEvent event) {
        Long userId = event.getUserId();
        if (BaniraUtils.isOwner(userId) || BaniraUtils.isButler(userId)) {
            return true;
        }
        return BaniraUtils.isMaid(event.getGroupId(), userId);
    }

    private List<String> collectUrls(BaniraBot bot, AnyMessageEvent event, String commandBody, List<String> aliases) {
        String args = removeCommandAlias(commandBody, aliases);
        List<String> urls = fileDownloadService.extractUrls(args);
        if (CollectionUtils.isNotNullOrEmpty(urls)) {
            return urls;
        }
        if (BaniraUtils.hasReply(event.getArrayMsg())) {
            String replyText = BaniraUtils.getReplyContentString(bot, event.getMessage());
            return fileDownloadService.extractUrls(replyText);
        }
        return List.of();
    }

    private String removeCommandAlias(String commandBody, List<String> aliases) {
        if (StringUtils.isNullOrEmptyEx(commandBody)) {
            return "";
        }
        for (String alias : aliases) {
            if (StringUtils.isNullOrEmptyEx(alias)) {
                continue;
            }
            if (commandBody.equalsIgnoreCase(alias)) {
                return "";
            }
            String prefix = alias + " ";
            if (commandBody.regionMatches(true, 0, prefix, 0, prefix.length())) {
                return commandBody.substring(prefix.length()).trim();
            }
        }
        return commandBody;
    }

    private Integer parseSelectionIndex(String message) {
        if (StringUtils.isNullOrEmptyEx(message)) {
            return null;
        }
        String text = message.replaceAll("\\[CQ:[^\\]]+]", "").trim();
        if (!text.matches("\\d+")) {
            return null;
        }
        try {
            return Integer.parseInt(text);
        } catch (NumberFormatException e) {
            return null;
        }
    }

}

