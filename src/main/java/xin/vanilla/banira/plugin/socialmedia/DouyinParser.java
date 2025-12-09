package xin.vanilla.banira.plugin.socialmedia;

import cn.hutool.http.HttpUtil;
import com.google.gson.JsonObject;
import com.mikuac.shiro.common.utils.MsgUtils;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.util.JsonUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Component
public class DouyinParser implements SocialMediaParser {

    private static final String DOUYIN_API = "https://apis.jxcxin.cn/api/douyin?url=%s";

    private static final Pattern DOUYIN_PATTERN = Pattern.compile(
            // 标准链接
            "((https?://)?(www\\.)?douyin\\.com/(video|note)/(\\d{18,}))" +
                    "|" +
                    // 分享链接
                    "((https?://)?(www\\.)?iesdouyin\\.com/share/video/(\\d{18,}))" +
                    "|" +
                    // 短链接
                    "((https?://)?v\\.douyin\\.com/(\\w{6,12})(/?))" +
                    "|" +
                    // 纯数字ID
                    "((?<!\\d)(\\d{18,})(?!\\d))"
    );

    // 提取标准链接中的视频ID
    private static final Pattern STANDARD_URL_PATTERN =
            Pattern.compile("douyin\\.com/(video|note)/(\\d{18,})");

    // 提取分享链接中的视频ID
    private static final Pattern SHARE_URL_PATTERN =
            Pattern.compile("iesdouyin\\.com/share/video/(\\d{18,})");

    // 提取短链接中的路径
    private static final Pattern SHORT_URL_PATTERN =
            Pattern.compile("v\\.douyin\\.com/([0-9A-Za-z]{6,12})");

    private List<String> extractVideoIds(String msg) {
        List<String> videoIds = new ArrayList<>();

        if (msg == null || msg.trim().isEmpty()) {
            return videoIds;
        }

        Matcher matcher = DOUYIN_PATTERN.matcher(msg);

        while (matcher.find()) {
            String match = matcher.group();

            // 跳过空匹配
            if (match == null || match.trim().isEmpty()) {
                continue;
            }

            if (match.contains("v.douyin.com/")) {
                // 短链接
                Matcher shortMatcher = SHORT_URL_PATTERN.matcher(match);
                if (shortMatcher.find()) {
                    String shortId = shortMatcher.group(1);
                    if (shortId != null && !shortId.isEmpty()) {
                        videoIds.add(shortId);
                    }
                }
            } else if (match.contains("iesdouyin.com/")) {
                // 分享链接
                Matcher shareMatcher = SHARE_URL_PATTERN.matcher(match);
                if (shareMatcher.find()) {
                    String videoId = shareMatcher.group(1);
                    if (videoId != null && !videoId.isEmpty()) {
                        videoIds.add(videoId);
                    }
                }
            } else if (match.contains("douyin.com/")) {
                // 标准链接
                Matcher standardMatcher = STANDARD_URL_PATTERN.matcher(match);
                if (standardMatcher.find()) {
                    String videoId = standardMatcher.group(2);
                    if (videoId != null && !videoId.isEmpty()) {
                        videoIds.add(videoId);
                    }
                }
            } else if (match.matches("\\d{18,}")) {
                // 纯数字ID
                videoIds.add(match);
            }
        }

        // 去重
        List<String> uniqueIds = new ArrayList<>();
        for (String id : videoIds) {
            if (!uniqueIds.contains(id)) {
                uniqueIds.add(id);
            }
        }

        return uniqueIds;
    }

    @Override
    public boolean hasSocialMedia(String msg) {
        if (msg == null || msg.trim().isEmpty()) {
            return false;
        }
        return DOUYIN_PATTERN.matcher(msg).find();
    }

    @Override
    public List<SocialMediaContent> parse(String msg) {
        List<SocialMediaContent> contents = new ArrayList<>();

        List<String> videoUrls = new ArrayList<>();
        List<String> videoIds = extractVideoIds(msg);
        for (String videoId : videoIds) {
            if (videoId.startsWith("http")) {
                videoUrls.add(videoId);
            } else if (videoId.matches("\\d{18,}")) {
                videoUrls.add("https://www.douyin.com/video/" + videoId);
            } else {
                videoUrls.add("https://v.douyin.com/" + videoId);
            }
        }
        for (String url : videoUrls) {
            String string = HttpUtil.get(DOUYIN_API.formatted(url));
            JsonObject jsonObject = JsonUtils.parseJsonObject(string);
            if (jsonObject != null && JsonUtils.getInt(jsonObject, "code", 201) == 200) {
                SocialMediaContent content = new SocialMediaContent();
                content.title(JsonUtils.getString(jsonObject, "data.title"));
                content.cover(JsonUtils.getString(jsonObject, "data.cover"));
                content.authorName(JsonUtils.getString(jsonObject, "data.author"));
                content.authorAvatar(JsonUtils.getString(jsonObject, "data.avatar"));
                content.video(JsonUtils.getString(jsonObject, "data.url"));
                content.like(JsonUtils.getInt(jsonObject, "data.like"));
                content.url(url);

                build(content);
                contents.add(content);
            }
        }
        return contents;
    }

    @Override
    public void build(SocialMediaContent content) {
        MsgUtils builder = MsgUtils.builder();
        builder.text("标题：" + content.title()).text("\n")
                .text("作者：" + content.authorName()).text("\n")
                .img(content.cover())
                .text("点赞：" + content.like()).text("\n")
                .text("链接：" + content.url());
        content.msg(builder.build());
    }
}
