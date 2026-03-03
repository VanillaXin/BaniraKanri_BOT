package xin.vanilla.banira.plugin;

import com.google.gson.JsonObject;
import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import jakarta.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.domain.KeyValue;
import xin.vanilla.banira.enums.EnumCacheFileType;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.HttpUtils;
import xin.vanilla.banira.util.JsonUtils;
import xin.vanilla.banira.util.StringUtils;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 抖音直发解析（仅发送视频，不发送文本）
 */
@Slf4j
@Shiro
@Component
public class DouyinDirectPlugin extends BasePlugin {

    private static final Pattern SHORT_URL_PATTERN = Pattern.compile("https?://v\\.douyin\\.com/[\\w-]{6,32}/?");
    private static final Pattern DIRECT_URL_PATTERN = Pattern.compile("https?://(?:www\\.)?(?:douyin\\.com|iesdouyin\\.com)/[^\\s]+");
    private static final Pattern AWEME_ID_IN_URL_PATTERN = Pattern.compile("/(?:video|note)/(\\d+)");
    private static final Pattern PURE_AWEME_ID_PATTERN = Pattern.compile("(?<!\\d)(\\d{18,})(?!\\d)");
    private static final Pattern AWEME_ID_QUERY_PATTERN = Pattern.compile("[?&]aweme_id=(\\d+)");

    private static final String AWEME_DETAIL_API = "https://www.douyin.com/aweme/v1/web/aweme/detail/?aweme_id=%s";

    @Nonnull
    @Override
    public List<String> getHelpInfo(Long groupId, @Nonnull String... types) {
        return List.of();
    }

    @AnyMessageHandler
    public boolean parse(BaniraBot bot, AnyMessageEvent event) {
        String message = normalizeMessage(event.getMessage());
        if (StringUtils.isNullOrEmptyEx(message)) {
            return false;
        }

        Set<String> awemeIds = extractAwemeIds(message);
        if (awemeIds.isEmpty()) {
            return false;
        }

        bot.setMsgEmojiLike(event.getMessageId(), 162);

        boolean sent = false;
        for (String awemeId : awemeIds) {
            JsonObject detail = requestAwemeDetail(awemeId);
            String videoPath = downloadVideoToLocal(detail);
            if (StringUtils.isNullOrEmptyEx(videoPath)) {
                continue;
            }
            String cover = getCoverUrl(detail);
            bot.sendMsg(event, MsgUtils.builder().video(videoPath, cover).build(), false);
            sent = true;
        }

        if (sent) {
            return bot.setMsgEmojiLikeHeart(event.getMessageId());
        }
        return bot.setMsgEmojiLikeBrokenHeart(event.getMessageId());
    }

    private Set<String> extractAwemeIds(String message) {
        Set<String> ids = new LinkedHashSet<>();

        Matcher shortMatcher = SHORT_URL_PATTERN.matcher(message);
        while (shortMatcher.find()) {
            String shortUrl = shortMatcher.group();
            String finalUrl = HttpUtils.getRedirectedUrl(shortUrl);
            String awemeId = extractAwemeIdFromUrl(finalUrl);
            if (StringUtils.isNotNullOrEmpty(awemeId)) {
                ids.add(awemeId);
            }
        }

        Matcher directMatcher = DIRECT_URL_PATTERN.matcher(message);
        while (directMatcher.find()) {
            String directUrl = directMatcher.group();
            String awemeId = extractAwemeIdFromUrl(directUrl);
            if (StringUtils.isNotNullOrEmpty(awemeId)) {
                ids.add(awemeId);
            }
        }

        Matcher pureIdMatcher = PURE_AWEME_ID_PATTERN.matcher(message);
        while (pureIdMatcher.find()) {
            ids.add(pureIdMatcher.group(1));
        }

        return ids;
    }

    private String extractAwemeIdFromUrl(String url) {
        if (StringUtils.isNullOrEmptyEx(url)) {
            return null;
        }

        Matcher pathMatcher = AWEME_ID_IN_URL_PATTERN.matcher(url);
        if (pathMatcher.find()) {
            return pathMatcher.group(1);
        }

        Matcher queryMatcher = AWEME_ID_QUERY_PATTERN.matcher(url);
        if (queryMatcher.find()) {
            return queryMatcher.group(1);
        }

        return null;
    }

    private String getPlayableVideoUrl(JsonObject detail) {
        if (detail == null) {
            return null;
        }
        String downloadUrl = JsonUtils.getString(detail, "aweme_detail.video.download_addr.url_list.[0]");
        if (StringUtils.isNotNullOrEmpty(downloadUrl)) {
            return downloadUrl;
        }
        return JsonUtils.getString(detail, "aweme_detail.video.play_addr.url_list.[0]");
    }

    private String getCoverUrl(JsonObject detail) {
        if (detail == null) {
            return null;
        }
        String dynamicCover = JsonUtils.getString(detail, "aweme_detail.video.dynamic_cover.url_list.[0]");
        if (StringUtils.isNotNullOrEmpty(dynamicCover)) {
            return dynamicCover;
        }
        return JsonUtils.getString(detail, "aweme_detail.video.cover.url_list.[0]");
    }

    private JsonObject requestAwemeDetail(String awemeId) {
        String apiUrl = AWEME_DETAIL_API.formatted(URLEncoder.encode(awemeId, StandardCharsets.UTF_8));
        String response = HttpUtils.getString(
                apiUrl,
                new KeyValue<>("accept", "application/json, text/plain, */*"),
                new KeyValue<>("origin", "https://open.douyin.com"),
                new KeyValue<>("referer", "https://www.douyin.com/"),
                new KeyValue<>("user-agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36")
        );
        return JsonUtils.parseJsonObject(response);
    }

    private String downloadVideoToLocal(JsonObject detail) {
        String videoUrl = getPlayableVideoUrl(detail);
        if (StringUtils.isNullOrEmptyEx(videoUrl)) {
            return null;
        }

        String fileName = BaniraUtils.downloadFileToCachePath(
                videoUrl,
                EnumCacheFileType.video,
                new KeyValue<>("referer", "https://www.douyin.com/"),
                new KeyValue<>("origin", "https://www.douyin.com"),
                new KeyValue<>("user-agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36")
        );
        if (StringUtils.isNullOrEmptyEx(fileName)) {
            return null;
        }
        return BaniraUtils.getCacheAbsolutePath(fileName, EnumCacheFileType.video);
    }

    private String normalizeMessage(String message) {
        if (message == null) {
            return null;
        }
        return message
                .replace("\\/", "/")
                .replace("&amp;", "&")
                .trim();
    }
}
