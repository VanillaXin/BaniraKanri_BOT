package xin.vanilla.banira.plugin.chat;

import com.mikuac.shiro.common.utils.MessageConverser;
import com.mikuac.shiro.dto.action.common.ActionData;
import com.mikuac.shiro.dto.action.response.GetForwardMsgResp;
import com.mikuac.shiro.dto.action.response.MsgResp;
import com.mikuac.shiro.enums.MsgTypeEnum;
import com.mikuac.shiro.model.ArrayMsg;
import dev.langchain4j.data.message.Content;
import dev.langchain4j.data.message.ImageContent;
import dev.langchain4j.data.message.TextContent;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.HttpUtils;
import xin.vanilla.banira.util.StringUtils;

import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.Locale;

public final class MessageConvert {

    private static final int MAX_INLINE_IMAGE_BYTES = 8 * 1024 * 1024;
    private static final int MAX_FORWARD_DEPTH = 2;
    private static final int MAX_FORWARD_NODES = 12;

    public static Content toContent(BaniraBot bot, Long groupId, ArrayMsg arrayMsg, boolean retainMedia) {
        return toContent(bot, groupId, arrayMsg, retainMedia, null);
    }

    public static Content toContent(BaniraBot bot, Long groupId, ArrayMsg arrayMsg, boolean retainMedia, ChatMessageContextFormatter.UserInfoCache cache) {
        List<Content> contents = toContents(bot, groupId, arrayMsg, retainMedia, cache);
        return contents.isEmpty() ? null : contents.getFirst();
    }

    public static List<Content> toContents(BaniraBot bot, Long groupId, ArrayMsg arrayMsg, boolean retainMedia, ChatMessageContextFormatter.UserInfoCache cache) {
        return toContents(bot, groupId, arrayMsg, retainMedia, cache, 0);
    }

    private static List<Content> toContents(BaniraBot bot, Long groupId, ArrayMsg arrayMsg, boolean retainMedia,
                                            ChatMessageContextFormatter.UserInfoCache cache, int depth) {
        List<Content> contents = new ArrayList<>();
        if (arrayMsg == null || arrayMsg.getType() == null || depth > MAX_FORWARD_DEPTH) {
            return contents;
        }
        Content result = null;
        try {
            switch (arrayMsg.getType()) {
                case at: {
                    long qq = arrayMsg.getLongData("qq");
                    if (qq == bot.getSelfId()) {
                        result = null;
                    } else if (qq == 0) {
                        result = new TextContent("@全体成员");
                    } else {
                        result = new TextContent(ChatMessageContextFormatter.atText(bot, groupId, qq, cache));
                    }
                }
                break;
                case image: {
                    String url = arrayMsg.getStringData("url");
                    if (retainMedia) {
                        result = imageContentOrLink(arrayMsg, url);
                    }
                    if (!retainMedia) {
                        result = imageLinkContent(arrayMsg, url);
                    }
                }
                break;
                case video: {
                    result = new TextContent("[视频]");
                }
                break;
                case reply: {
                    long replyId = arrayMsg.getLongData("id");
                    if (isRecalledReply(bot, groupId, replyId)) {
                        contents.add(new TextContent("[quoted message recalled]\n"));
                        break;
                    }
                    long replyUserId = BaniraUtils.getReplyUserId(bot, groupId, List.of(arrayMsg));
                    String sender = replyUserId > 0
                            ? ChatMessageContextFormatter.describeUser(bot, groupId, replyUserId, cache)
                            : "未知用户";
                    contents.add(new TextContent("引用消息：消息ID=" + replyId + "，发送者=" + sender + "\n"));
                    List<ArrayMsg> replyContent = BaniraUtils.getReplyContent(bot, List.of(arrayMsg));
                    if (replyContent != null) {
                        for (ArrayMsg quoted : replyContent) {
                            if (quoted == null || quoted.getType() == null || quoted.getType() == MsgTypeEnum.reply) {
                                continue;
                            }
                            contents.addAll(toContents(bot, groupId, quoted, retainMedia, cache, depth + 1));
                        }
                    }
                }
                break;
                case forward: {
                    contents.addAll(forwardContents(bot, groupId, arrayMsg, retainMedia, cache, depth));
                }
                break;
                case text: {
                    String text = ChatInputSanitizer.sanitizeUserText(arrayMsg.toCQCode());
                    if (!StringUtils.isNullOrEmptyEx(text)) {
                        result = new TextContent(text);
                    }
                }
            }
        } catch (Exception ignored) {
        }
        if (result != null) {
            contents.add(result);
        }
        return contents;
    }

    @Nonnull
    public static String toPlainText(BaniraBot bot, Long groupId, ArrayMsg arrayMsg, ChatMessageContextFormatter.UserInfoCache cache) {
        return toPlainText(bot, groupId, arrayMsg, cache, 0);
    }

    @Nonnull
    private static String toPlainText(BaniraBot bot, Long groupId, ArrayMsg arrayMsg, ChatMessageContextFormatter.UserInfoCache cache, int depth) {
        if (arrayMsg == null || arrayMsg.getType() == null || depth > 2) {
            return "";
        }
        try {
            return switch (arrayMsg.getType()) {
                case at -> {
                    long qq = arrayMsg.getLongData("qq");
                    if (qq == bot.getSelfId()) {
                        yield "@你 ";
                    }
                    if (qq == 0) {
                        yield "@全体成员 ";
                    }
                    yield ChatMessageContextFormatter.atText(bot, groupId, qq, cache);
                }
                case image -> imageLinkText(arrayMsg);
                case video -> "[视频]";
                case reply -> replyPlainText(bot, groupId, arrayMsg, cache, depth);
                case forward -> forwardPlainText(bot, groupId, arrayMsg, cache, depth);
                case text -> ChatInputSanitizer.sanitizeUserText(arrayMsg.toCQCode());
                default -> ChatInputSanitizer.sanitizeUserText(arrayMsg.toCQCode());
            };
        } catch (Exception ignored) {
            return "";
        }
    }

    @Nonnull
    private static String replyPlainText(BaniraBot bot, Long groupId, ArrayMsg arrayMsg, ChatMessageContextFormatter.UserInfoCache cache, int depth) {
        long replyId = arrayMsg.getLongData("id");
        if (isRecalledReply(bot, groupId, replyId)) {
            return "[quoted message recalled] ";
        }
        long replyUserId = BaniraUtils.getReplyUserId(bot, groupId, List.of(arrayMsg));
        String sender = replyUserId > 0
                ? ChatMessageContextFormatter.describeUser(bot, groupId, replyUserId, cache)
                : "未知用户";
        StringBuilder builder = new StringBuilder("引用消息[msgId=")
                .append(replyId)
                .append(", 发送者=")
                .append(sender)
                .append("] ");
        List<ArrayMsg> replyContent = BaniraUtils.getReplyContent(bot, List.of(arrayMsg));
        if (replyContent != null) {
            for (ArrayMsg quoted : replyContent) {
                if (quoted == null || quoted.getType() == null || quoted.getType() == MsgTypeEnum.reply) {
                    continue;
                }
                builder.append(toPlainText(bot, groupId, quoted, cache, depth + 1));
            }
        }
        return builder.toString();
    }

    @Nonnull
    private static List<Content> forwardContents(@Nonnull BaniraBot bot, @Nullable Long groupId, @Nonnull ArrayMsg arrayMsg,
                                                 boolean retainMedia, ChatMessageContextFormatter.UserInfoCache cache, int depth) {
        List<Content> contents = new ArrayList<>();
        List<MsgResp> nodes = resolveForwardNodes(bot, arrayMsg);
        if (nodes.isEmpty()) {
            contents.add(new TextContent("[合并转发消息，内容未能展开]"));
            return contents;
        }
        contents.add(new TextContent("合并转发内容如下，节点里的图片也属于被引用内容：\n"));
        int count = 0;
        for (MsgResp node : nodes) {
            if (node == null || count >= MAX_FORWARD_NODES) {
                break;
            }
            count++;
            Long senderId = node.getUserId();
            String sender = BaniraUtils.isUserIdValid(senderId)
                    ? ChatMessageContextFormatter.describeUser(bot, groupId, senderId, cache)
                    : "未知用户";
            contents.add(new TextContent("转发节点 " + count + "，发送者=" + sender + "\n"));
            for (ArrayMsg segment : nodeArrayMsg(node)) {
                if (segment == null || segment.getType() == null) {
                    continue;
                }
                contents.addAll(toContents(bot, groupId, segment, retainMedia, cache, depth + 1));
            }
            contents.add(new TextContent("\n"));
        }
        return contents;
    }

    @Nonnull
    private static String forwardPlainText(@Nonnull BaniraBot bot, @Nullable Long groupId, @Nonnull ArrayMsg arrayMsg,
                                           ChatMessageContextFormatter.UserInfoCache cache, int depth) {
        List<MsgResp> nodes = resolveForwardNodes(bot, arrayMsg);
        if (nodes.isEmpty()) {
            return "[合并转发消息，内容未能展开]";
        }
        StringBuilder builder = new StringBuilder("合并转发内容：");
        int count = 0;
        for (MsgResp node : nodes) {
            if (node == null || count >= MAX_FORWARD_NODES) {
                break;
            }
            count++;
            Long senderId = node.getUserId();
            String sender = BaniraUtils.isUserIdValid(senderId)
                    ? ChatMessageContextFormatter.describeUser(bot, groupId, senderId, cache)
                    : "未知用户";
            builder.append(" [节点").append(count).append(" 发送者=").append(sender).append("] ");
            for (ArrayMsg segment : nodeArrayMsg(node)) {
                builder.append(toPlainText(bot, groupId, segment, cache, depth + 1));
            }
        }
        return builder.toString();
    }

    @Nonnull
    private static List<MsgResp> resolveForwardNodes(@Nonnull BaniraBot bot, @Nonnull ArrayMsg arrayMsg) {
        List<MsgResp> nodes = BaniraUtils.getForwardContentFirst(arrayMsg);
        if (!nodes.isEmpty()) {
            return nodes;
        }
        long forwardId = arrayMsg.getLongData("id");
        if (forwardId <= 0) {
            return List.of();
        }
        try {
            ActionData<GetForwardMsgResp> resp = bot.getForwardMsg((int) forwardId);
            if (bot.isActionDataNotEmpty(resp) && resp.getData() != null && resp.getData().getMessages() != null) {
                return resp.getData().getMessages();
            }
        } catch (Exception ignored) {
        }
        return List.of();
    }

    private static boolean isRecalledReply(@Nonnull BaniraBot bot, @Nullable Long groupId, long replyId) {
        if (replyId <= 0 || replyId > Integer.MAX_VALUE) {
            return false;
        }
        try {
            if (BaniraUtils.isGroupIdValid(groupId)) {
                var record = bot.getMessageRecordManager().getGroupMessageRecord(groupId, (int) replyId);
                return record != null && record.recalled();
            }
        } catch (Exception ignored) {
        }
        return false;
    }

    @Nonnull
    private static List<ArrayMsg> nodeArrayMsg(@Nonnull MsgResp node) {
        if (node.getArrayMsg() != null && !node.getArrayMsg().isEmpty()) {
            return node.getArrayMsg();
        }
        if (StringUtils.isNotNullOrEmpty(node.getMessage())) {
            return MessageConverser.stringToArray(node.getMessage());
        }
        return List.of();
    }

    @Nonnull
    private static Content imageContentOrLink(@Nonnull ArrayMsg arrayMsg, String url) {
        if (StringUtils.isNullOrEmptyEx(url)) {
            return imageLinkContent(arrayMsg, url);
        }
        try {
            ImageContent content = imageContentFromBytes(downloadImageBytes(url));
            if (content != null) {
                return content;
            }
        } catch (Exception ignored) {
        }
        return imageLinkContent(arrayMsg, url);
    }

    private static byte[] downloadImageBytes(@Nonnull String url) {
        String trimmed = url.trim();
        String lower = trimmed.toLowerCase(Locale.ROOT);
        if (lower.startsWith("data:image/")) {
            int comma = trimmed.indexOf(',');
            if (comma > 0 && trimmed.substring(0, comma).toLowerCase(Locale.ROOT).contains(";base64")) {
                try {
                    return Base64.getDecoder().decode(trimmed.substring(comma + 1));
                } catch (Exception ignored) {
                    return null;
                }
            }
            return null;
        }
        if (lower.startsWith("file:")) {
            try {
                Path path = Path.of(URI.create(trimmed));
                if (Files.isRegularFile(path) && Files.size(path) <= MAX_INLINE_IMAGE_BYTES) {
                    return Files.readAllBytes(path);
                }
            } catch (Exception ignored) {
                return null;
            }
            return null;
        }
        return HttpUtils.downloadBytes(trimmed);
    }

    static ImageContent imageContentFromBytes(byte[] bytes) {
        if (bytes == null || bytes.length == 0 || bytes.length > MAX_INLINE_IMAGE_BYTES) {
            return null;
        }
        String mimeType = detectImageMime(bytes);
        if (StringUtils.isNullOrEmptyEx(mimeType)) {
            return null;
        }
        String base64 = Base64.getEncoder().encodeToString(bytes);
        return new ImageContent(base64, mimeType);
    }

    static String detectImageMime(byte[] bytes) {
        if (bytes == null || bytes.length < 4) {
            return "";
        }
        if (bytes.length >= 8
                && (bytes[0] & 0xFF) == 0x89
                && bytes[1] == 0x50
                && bytes[2] == 0x4E
                && bytes[3] == 0x47
                && bytes[4] == 0x0D
                && bytes[5] == 0x0A
                && bytes[6] == 0x1A
                && bytes[7] == 0x0A) {
            return "image/png";
        }
        if ((bytes[0] & 0xFF) == 0xFF
                && (bytes[1] & 0xFF) == 0xD8
                && (bytes[2] & 0xFF) == 0xFF) {
            return "image/jpeg";
        }
        if (bytes.length >= 6
                && bytes[0] == 'G'
                && bytes[1] == 'I'
                && bytes[2] == 'F'
                && bytes[3] == '8'
                && (bytes[4] == '7' || bytes[4] == '9')
                && bytes[5] == 'a') {
            return "image/gif";
        }
        if (bytes.length >= 12
                && bytes[0] == 'R'
                && bytes[1] == 'I'
                && bytes[2] == 'F'
                && bytes[3] == 'F'
                && bytes[8] == 'W'
                && bytes[9] == 'E'
                && bytes[10] == 'B'
                && bytes[11] == 'P') {
            return "image/webp";
        }
        return "";
    }

    @Nonnull
    private static TextContent imageLinkContent(@Nonnull ArrayMsg arrayMsg, String url) {
        return new TextContent(imageLinkText(arrayMsg, url));
    }

    @Nonnull
    private static String imageLinkText(@Nonnull ArrayMsg arrayMsg) {
        return imageLinkText(arrayMsg, arrayMsg.getStringData("url"));
    }

    @Nonnull
    private static String imageLinkText(@Nonnull ArrayMsg arrayMsg, String url) {
        if (StringUtils.isNotNullOrEmpty(url)) {
            return "[图片链接] " + url.trim();
        }
        String file = arrayMsg.getStringData("file");
        if (StringUtils.isNotNullOrEmpty(file)) {
            return "[图片] " + file.trim();
        }
        return "[图片]";
    }

}
