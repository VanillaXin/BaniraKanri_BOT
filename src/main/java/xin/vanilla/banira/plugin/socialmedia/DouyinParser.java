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
public class DouyinParser implements SocialMediaParser {

    private static final String DOUYIN_API = "https://apis.jxcxin.cn/api/douyin?url=%s";

    // 使用命名捕获组统一提取视频ID和短链接ID（包含模式，匹配文本中任何位置的链接）
    private static final Pattern DOUYIN_PATTERN = new RegexpHelper()
            .groupNonIg(
                    // 标准链接: https://www.douyin.com/video/123456789012345678
                    "(?:https?://)?(?:www\\.)?douyin\\.com/(?:video|note)/(?<videoId>\\d{18,})",
                    // 分享链接: https://www.iesdouyin.com/share/video/123456789012345678
                    "(?:https?://)?(?:www\\.)?iesdouyin\\.com/share/video/(?<shareVideoId>\\d{18,})",
                    // 短链接: https://v.douyin.com/xxxxxx (支持6-20个字符，更宽松)
                    "(?:https?://)?v\\.douyin\\.com/(?<shortId>[0-9A-Za-z]{6,20})(?:/|\\s|$)",
                    // 纯数字ID: 123456789012345678
                    "(?<!\\d)(?<pureId>\\d{18,})(?!\\d)"
            )
            .compile();

    private List<String> extractVideoIds(String msg) {
        Set<String> videoIds = new LinkedHashSet<>();

        if (msg == null || msg.trim().isEmpty()) {
            return new ArrayList<>(videoIds);
        }

        Matcher matcher = DOUYIN_PATTERN.matcher(msg);

        while (matcher.find()) {
            String videoId = matcher.group("videoId");
            if (videoId != null && !videoId.isEmpty()) {
                videoIds.add(videoId);
                continue;
            }

            String shareVideoId = matcher.group("shareVideoId");
            if (shareVideoId != null && !shareVideoId.isEmpty()) {
                videoIds.add(shareVideoId);
                continue;
            }

            String shortId = matcher.group("shortId");
            if (shortId != null && !shortId.isEmpty()) {
                shortId = shortId.trim();
                videoIds.add(shortId);
                continue;
            }

            String pureId = matcher.group("pureId");
            if (pureId != null && !pureId.isEmpty()) {
                videoIds.add(pureId);
            }
        }

        return new ArrayList<>(videoIds);
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
        List<String> videoIds = extractVideoIds(msg);

        for (String videoId : videoIds) {
            String url;
            if (videoId.startsWith("http")) {
                url = videoId;
            } else if (videoId.matches("\\d{18,}")) {
                // 纯数字ID转换为标准链接
                url = "https://www.douyin.com/video/" + videoId;
            } else {
                // 短链接ID
                url = "https://v.douyin.com/" + videoId;
            }

            String response = HttpUtil.get(DOUYIN_API.formatted(url));
            JsonObject jsonObject = JsonUtils.parseJsonObject(response);
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
