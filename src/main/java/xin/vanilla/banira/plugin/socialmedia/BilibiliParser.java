package xin.vanilla.banira.plugin.socialmedia;

import cn.hutool.http.HttpUtil;
import com.google.gson.JsonObject;
import com.mikuac.shiro.common.utils.MsgUtils;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.util.JsonUtils;
import xin.vanilla.banira.util.RegexpHelper;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Component
public class BilibiliParser implements SocialMediaParser {

    private static final String BILIBILI_API = "https://api.mir6.com/api/bzjiexi?type=json&url=%s";

    private static final Pattern BILIBILI_PATTERN = new RegexpHelper()
            .groupNonIg(
                    // 标准链接: https://www.bilibili.com/video/BVxxxxx
                    "(?:https?://)?(?:www\\.)?bilibili\\.com/video/(?<bvId>BV[0-9A-Za-z]{10,})",
                    // 短链接: https://b23.tv/xxxxxx (支持6-20个字符，更宽松)
                    "(?:https?://)?b23\\.tv/(?<shortId>[0-9A-Za-z]{6,20})(?:/|\\s|$)",
                    // 纯BV号: BVxxxxx
                    "(?<pureBvId>BV[0-9A-Za-z]{10,})",
                    // 纯AV号: av123456
                    "av(?<avId>\\d+)"
            )
            .compile();

    private List<String> extractVideoIds(String msg) {
        Set<String> videoIds = new LinkedHashSet<>();

        if (msg == null || msg.trim().isEmpty()) {
            return new ArrayList<>(videoIds);
        }

        Matcher matcher = BILIBILI_PATTERN.matcher(msg);

        while (matcher.find()) {
            String bvId = matcher.group("bvId");
            if (bvId != null && !bvId.isEmpty()) {
                videoIds.add(bvId);
                continue;
            }

            String shortId = matcher.group("shortId");
            if (shortId != null && !shortId.isEmpty()) {
                shortId = shortId.trim();
                videoIds.add(shortId);
                continue;
            }

            String pureBvId = matcher.group("pureBvId");
            if (pureBvId != null && !pureBvId.isEmpty()) {
                videoIds.add(pureBvId);
                continue;
            }

            String avId = matcher.group("avId");
            if (avId != null && !avId.isEmpty()) {
                videoIds.add("av" + avId);
            }
        }

        return new ArrayList<>(videoIds);
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
        List<String> videoIds = extractVideoIds(msg);

        for (String videoId : videoIds) {
            String url;
            if (videoId.startsWith("http")) {
                url = videoId;
            } else if (videoId.startsWith("BV") || videoId.startsWith("av")) {
                url = "https://www.bilibili.com/video/" + videoId;
            } else {
                url = "https://b23.tv/" + videoId;
            }

            String response = HttpUtil.get(BILIBILI_API.formatted(url));
            JsonObject jsonObject = JsonUtils.parseJsonObject(response);
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
