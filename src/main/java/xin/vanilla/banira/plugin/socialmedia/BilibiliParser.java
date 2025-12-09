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
public class BilibiliParser implements SocialMediaParser {

    private static final String BILIBILI_API = "https://api.mir6.com/api/bzjiexi?type=json&url=%s";

    private static final Pattern BILIBILI_PATTERN =
            Pattern.compile(
                    // 标准链接
                    "(https?://)?(www\\.)?bilibili\\.com/video/(BV[0-9A-Za-z]{10,})" +
                            "|" +
                            // 短链接
                            "(https?://)?b23\\.tv/([0-9A-Za-z]{6,12})" +
                            "|" +
                            // 纯BV号
                            "(BV[0-9A-Za-z]{10,})" +
                            "|" +
                            // 纯AV号
                            "av([0-9]+)"
            );

    private static final Pattern STANDARD_URL_PATTERN =
            Pattern.compile("bilibili\\.com/video/(BV[0-9A-Za-z]{10,})");

    private static final Pattern SHORT_URL_PATTERN =
            Pattern.compile("b23\\.tv/([0-9A-Za-z]{6,12})");

    private List<String> extractVideoIds(String msg) {
        List<String> videoIds = new ArrayList<>();
        if (msg == null || msg.trim().isEmpty()) {
            return videoIds;
        }
        Matcher matcher = BILIBILI_PATTERN.matcher(msg);
        while (matcher.find()) {
            String match = matcher.group();
            if (match.contains("bilibili.com/video/")) {
                // 标准链接
                Matcher standardMatcher = STANDARD_URL_PATTERN.matcher(match);
                if (standardMatcher.find()) {
                    String bvId = standardMatcher.group(1);
                    if (bvId != null && !bvId.isEmpty()) {
                        videoIds.add(bvId);
                    }
                }
            } else if (match.contains("b23.tv/")) {
                // 短链接
                Matcher shortMatcher = SHORT_URL_PATTERN.matcher(match);
                if (shortMatcher.find()) {
                    String shortId = shortMatcher.group(1);
                    if (shortId != null && !shortId.isEmpty()) {
                        videoIds.add(shortId);
                    }
                }
            } else if (match.startsWith("BV")) {
                // 纯BV号
                videoIds.add(match);
            } else if (match.startsWith("av")) {
                // 纯AV号
                videoIds.add(match.substring(2));
            }
        }
        return videoIds;
    }

    @Override
    public boolean hasSocialMedia(String msg) {
        if (msg == null || msg.trim().isEmpty()) {
            return false;
        }
        return BILIBILI_PATTERN.matcher(msg).find();
    }

    @Override
    public List<SocialMediaContent> parse(String msg) {
        List<SocialMediaContent> contents = new ArrayList<>();

        List<String> videoUrls = new ArrayList<>();
        for (String videoId : extractVideoIds(msg)) {
            if (videoId.startsWith("http")) {
                videoUrls.add(videoId);
            } else if (videoId.startsWith("BV")) {
                videoUrls.add("https://www.bilibili.com/video/" + videoId);
            } else {
                videoUrls.add("https://b23.tv/" + videoId);
            }
        }
        for (String url : videoUrls) {
            String string = HttpUtil.get(BILIBILI_API.formatted(url));
            JsonObject jsonObject = JsonUtils.parseJsonObject(string);
            if (jsonObject != null && JsonUtils.getInt(jsonObject, "code", 201) == 200) {
                SocialMediaContent content = new SocialMediaContent();
                content.title(JsonUtils.getString(jsonObject, "title"));
                content.cover(JsonUtils.getString(jsonObject, "imgurl"));
                content.desc(JsonUtils.getString(jsonObject, "desc"));
                content.authorName(JsonUtils.getString(jsonObject, "user.name"));
                content.authorAvatar(JsonUtils.getString(jsonObject, "user.user_img"));
                content.video(JsonUtils.getString(jsonObject, "data.[0].video_url"));
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
                .text("UP：" + content.authorName())
                // .text("粉丝：" + content.fans())
                .text("\n")
                .img(content.cover())
                // .text("播放：" + content.play()).text("弹幕：" + content.danmaku()).text("\n")
                // .text("点赞：" + content.like()).text("投币：" + content.coin()).text("\n")
                // .text("收藏：" + content.collect()).text("转发：" + content.share()).text("\n")
                // .text(content.playing() + " 人正在观看")
                .text("简介：" + content.desc()).text("\n")
                .text("链接：" + content.url());
        content.msg(builder.build());
    }
}
