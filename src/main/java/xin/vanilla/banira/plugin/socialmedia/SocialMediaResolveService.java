package xin.vanilla.banira.plugin.socialmedia;

import jakarta.annotation.Nonnull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * 社交媒体链接解析，供插件指令与 AI 能力共用
 */
@Service
public class SocialMediaResolveService {

    @Autowired(required = false)
    private List<SocialMediaParser> parsers = new ArrayList<>();
    @Autowired
    private SocialMediaApiService socialMediaApiService;

    @Nonnull
    public String parseMessage(@Nonnull String message) {
        if (StringUtils.isNullOrEmptyEx(message)) {
            return "消息内容为空";
        }
        if (CollectionUtils.isNullOrEmpty(parsers)) {
            return "未配置社交媒体解析器";
        }
        StringBuilder builder = new StringBuilder();
        for (SocialMediaParser parser : parsers) {
            if (!parser.hasSocialMedia(message)) {
                continue;
            }
            List<SocialMediaContent> contents = socialMediaApiService.parse(parser, message);
            for (SocialMediaContent content : contents) {
                if (!builder.isEmpty()) {
                    builder.append("\n\n");
                }
                builder.append(formatContent(content));
            }
        }
        if (builder.isEmpty()) {
            return "未识别到可解析的社交媒体链接";
        }
        return builder.toString();
    }

    @Nonnull
    private static String formatContent(@Nonnull SocialMediaContent content) {
        StringBuilder builder = new StringBuilder();
        if (StringUtils.isNotNullOrEmpty(content.title())) {
            builder.append("标题：").append(content.title()).append('\n');
        }
        if (StringUtils.isNotNullOrEmpty(content.authorName())) {
            builder.append("作者：").append(content.authorName()).append('\n');
        }
        if (StringUtils.isNotNullOrEmpty(content.desc())) {
            builder.append("简介：").append(content.desc()).append('\n');
        }
        if (StringUtils.isNotNullOrEmpty(content.summary())) {
            builder.append("摘要：").append(content.summary()).append('\n');
        }
        if (StringUtils.isNotNullOrEmpty(content.url())) {
            builder.append("链接：").append(content.url());
        }
        return builder.toString().trim();
    }

}
