package xin.vanilla.banira.plugin.socialmedia;

import cn.hutool.http.HttpUtil;
import com.google.gson.JsonObject;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import xin.vanilla.banira.util.HttpUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.JsonUtils;
import xin.vanilla.banira.util.StringUtils;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 社交媒体外部接口服务
 */
@Slf4j
@Service
public class SocialMediaApiService {

    private static final Pattern DOUYIN_PATH_AWEME_ID_PATTERN = Pattern.compile("/(?:video|note)/(\\d+)");
    private static final Pattern DOUYIN_QUERY_AWEME_ID_PATTERN = Pattern.compile("[?&]aweme_id=(\\d+)");
    private static final Pattern DOUYIN_SHORT_URL_PATTERN = Pattern.compile("https?://v\\.douyin\\.com/[\\w-]{6,32}/?");
    private static final Pattern PURE_AWEME_ID_PATTERN = Pattern.compile("^\\d{18,}$");

    @Resource
    private Supplier<SocialMediaSettings> socialMediaConfig;

    public List<SocialMediaContent> parse(SocialMediaParser parser, String msg) {
        if (parser == null || StringUtils.isNullOrEmptyEx(msg) || !parser.hasSocialMedia(msg)) {
            return List.of();
        }
        List<String> targets = parser.extractTargets(msg);
        if (CollectionUtils.isNullOrEmpty(targets)) {
            return List.of();
        }
        List<SocialMediaApiSettings> parserApis = getParserApis(parser.type());
        if (CollectionUtils.isNullOrEmpty(parserApis)) {
            return List.of();
        }

        List<SocialMediaContent> result = new ArrayList<>();
        for (String target : targets) {
            SocialMediaContent parsed = parseOneTarget(parser, target, parserApis);
            if (parsed != null) {
                result.add(parsed);
            }
        }
        return result;
    }

    private SocialMediaContent parseOneTarget(SocialMediaParser parser, String target, List<SocialMediaApiSettings> parserApis) {
        for (SocialMediaApiSettings apiConfig : parserApis) {
            if (apiConfig == null || StringUtils.isNullOrEmptyEx(apiConfig.api())) {
                continue;
            }
            try {
                String apiTarget = resolveApiTarget(target, apiConfig.targetMode());
                if (StringUtils.isNullOrEmptyEx(apiTarget)) {
                    continue;
                }
                String api = buildApiUrl(apiConfig.api(), apiTarget);
                String response = requestApi(api, apiConfig.headers());
                JsonObject jsonObject = JsonUtils.parseJsonObject(response);
                if (jsonObject == null) {
                    continue;
                }
                SocialMediaContent content = parseContent(jsonObject, target, apiConfig.fields());
                if (content == null) {
                    continue;
                }
                parser.build(content);
                return content;
            } catch (Exception e) {
                LOGGER.debug("social media api parse failed, parser={}, target={}, api={}", parser.type(), target, apiConfig.api(), e);
            }
        }
        return null;
    }

    private SocialMediaContent parseContent(JsonObject jsonObject, String target, SocialMediaApiFieldSettings fields) {
        if (fields == null) {
            return null;
        }
        if (!isSuccess(jsonObject, fields)) {
            return null;
        }

        SocialMediaContent content = new SocialMediaContent();
        content.url(target);
        content.title(getString(jsonObject, fields.titlePath()));
        content.cover(getString(jsonObject, fields.coverPathFallback(), fields.coverPath()));
        content.desc(getString(jsonObject, fields.descPath()));
        content.authorName(getString(jsonObject, fields.authorNamePath()));
        content.authorAvatar(getString(jsonObject, fields.authorAvatarPath()));
        content.video(getString(jsonObject, fields.videoPathFallback(), fields.videoPath()));
        content.audio(getString(jsonObject, fields.audioPath()));
        content.summary(getString(jsonObject, fields.summaryPath()));
        content.like(getInt(jsonObject, fields.likePath()));
        if (!hasAnyMainContent(content)) {
            return null;
        }
        return content;
    }

    private boolean isSuccess(JsonObject jsonObject, SocialMediaApiFieldSettings fields) {
        if (StringUtils.isNullOrEmptyEx(fields.successCodePath())) {
            return true;
        }
        int code = JsonUtils.getInt(jsonObject, fields.successCodePath(), Integer.MIN_VALUE);
        if (code == Integer.MIN_VALUE) {
            return false;
        }
        if (fields.successCode() == null) {
            return true;
        }
        return code == fields.successCode();
    }

    private boolean hasAnyMainContent(SocialMediaContent content) {
        return StringUtils.isNotNullOrEmpty(content.title())
                || StringUtils.isNotNullOrEmpty(content.desc())
                || StringUtils.isNotNullOrEmpty(content.cover())
                || StringUtils.isNotNullOrEmpty(content.video())
                || StringUtils.isNotNullOrEmpty(content.summary());
    }

    private String buildApiUrl(String apiTemplate, String target) {
        String encoded = URLEncoder.encode(target, StandardCharsets.UTF_8);
        if (apiTemplate.contains("%s")) {
            return apiTemplate.formatted(encoded);
        }
        return apiTemplate + encoded;
    }

    private String requestApi(String api, Map<String, String> headers) {
        if (headers == null || headers.isEmpty()) {
            return HttpUtil.get(api);
        }
        return HttpUtil.createGet(api).headerMap(headers, true).execute().body();
    }

    private String resolveApiTarget(String target, String targetMode) {
        String mode = StringUtils.isNullOrEmptyEx(targetMode) ? "url" : targetMode.trim().toLowerCase(Locale.ROOT);
        if ("awemeid".equals(mode)) {
            return extractAwemeId(target);
        }
        return target;
    }

    private String extractAwemeId(String target) {
        if (StringUtils.isNullOrEmptyEx(target)) {
            return null;
        }
        String trimmed = target.trim();
        if (PURE_AWEME_ID_PATTERN.matcher(trimmed).matches()) {
            return trimmed;
        }

        String redirected = trimmed;
        if (DOUYIN_SHORT_URL_PATTERN.matcher(trimmed).find()) {
            redirected = HttpUtils.getRedirectedUrl(trimmed);
        }
        if (StringUtils.isNullOrEmptyEx(redirected)) {
            redirected = trimmed;
        }

        Matcher pathMatcher = DOUYIN_PATH_AWEME_ID_PATTERN.matcher(redirected);
        if (pathMatcher.find()) {
            return pathMatcher.group(1);
        }

        Matcher queryMatcher = DOUYIN_QUERY_AWEME_ID_PATTERN.matcher(redirected);
        if (queryMatcher.find()) {
            return queryMatcher.group(1);
        }
        return null;
    }

    private String getString(JsonObject jsonObject, String path) {
        if (StringUtils.isNullOrEmptyEx(path)) {
            return "";
        }
        return JsonUtils.getString(jsonObject, path, "");
    }

    private String getString(JsonObject jsonObject, List<String> fallbackPaths, String path) {
        if (CollectionUtils.isNotNullOrEmpty(fallbackPaths)) {
            for (String fallbackPath : fallbackPaths) {
                String value = getString(jsonObject, fallbackPath);
                if (StringUtils.isNotNullOrEmpty(value)) {
                    return value;
                }
            }
        }
        return getString(jsonObject, path);
    }

    private int getInt(JsonObject jsonObject, String path) {
        if (StringUtils.isNullOrEmptyEx(path)) {
            return 0;
        }
        return JsonUtils.getInt(jsonObject, path, 0);
    }

    private List<SocialMediaApiSettings> getParserApis(String parserType) {
        if (StringUtils.isNullOrEmptyEx(parserType)) {
            return List.of();
        }
        if (socialMediaConfig == null || socialMediaConfig.get() == null) {
            return List.of();
        }
        List<SocialMediaApiSettings> apis = socialMediaConfig.get().apis();
        if (CollectionUtils.isNullOrEmpty(apis)) {
            return List.of();
        }
        String type = parserType.trim().toLowerCase(Locale.ROOT);
        return apis.stream()
                .filter(api -> api != null && StringUtils.isNotNullOrEmpty(api.parser()))
                .filter(api -> type.equals(api.parser().trim().toLowerCase(Locale.ROOT)))
                .toList();
    }
}
