package xin.vanilla.banira.plugin.chat.web;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import jakarta.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.plugin.chat.AiTextLimits;
import xin.vanilla.banira.util.HttpUtils;
import xin.vanilla.banira.util.JsonUtils;
import xin.vanilla.banira.util.StringUtils;
import xin.vanilla.banira.util.http.HttpResponse;

import java.net.URI;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.*;
import java.util.regex.Pattern;

/**
 * 给 AI Agent 使用的轻量网页搜索能力，只返回公开搜索结果摘要。
 */
@Slf4j
@Component
public class WebSearchService {

    private static final int MAX_RESULTS = 5;
    private static final int MAX_SEARXNG_INSTANCES = 6;
    private static final String USER_AGENT = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36";
    private static final String SEARXNG_URLS_PROPERTY = "banira.search.searxng.urls";
    private static final String SEARXNG_URLS_ENV = "BANIRA_SEARCH_SEARXNG_URLS";
    private static final List<String> PUBLIC_SEARXNG_FALLBACKS = List.of(
            "https://search.rhscz.eu/",
            "https://searxng.website/",
            "https://search.zina.dev/",
            "https://seek.fyi/"
    );
    private static final Pattern CQ_CODE = Pattern.compile("\\[CQ:[^]]+]");

    @Nonnull
    public String search(@Nonnull String query) {
        String trimmed = query.trim();
        if (StringUtils.isNullOrEmptyEx(trimmed)) {
            return "搜索关键词不能为空";
        }
        String normalizedQuery = normalizeQuery(trimmed);
        String searchQuery = buildSearchQuery(normalizedQuery);
        LOGGER.debug("AI web search queryChars={} normalizedChars={} searchChars={}",
                trimmed.length(), normalizedQuery.length(), searchQuery.length());
        List<SearchResult> results = searchSearxng(searchQuery);
        if (results.isEmpty()) {
            results = searchBing(searchQuery);
        }
        if (results.isEmpty()) {
            return "没搜到可用结果";
        }
        return formatResults(normalizedQuery, results);
    }

    @Nonnull
    String searchForTest(@Nonnull String query, @Nonnull List<SearchResult> results) {
        return formatResults(normalizeQuery(query), results);
    }

    @Nonnull
    private static String formatResults(@Nonnull String normalizedQuery, @Nonnull List<SearchResult> results) {
        StringBuilder builder = new StringBuilder("查询：").append(normalizedQuery);
        for (int i = 0; i < results.size(); i++) {
            SearchResult result = results.get(i);
            builder.append("\n").append(i + 1).append(". ").append(result.title());
            if (StringUtils.isNotNullOrEmpty(result.snippet())) {
                builder.append("\n摘要：").append(result.snippet());
            }
            builder.append("\n链接：").append(result.url());
        }
        return AiTextLimits.truncate(builder.toString(), AiTextLimits.MAX_CAPABILITY_RESULT);
    }

    @Nonnull
    private static String normalizeQuery(@Nonnull String query) {
        String normalized = CQ_CODE.matcher(query).replaceAll(" ");
        normalized = normalized
                .replaceAll("\\s+", " ")
                .replaceAll("@\\S{1,32}", " ")
                .replaceAll("^(帮我查一下|帮我查|帮我搜一下|帮我搜|查一下|搜一下|搜索|查询)", "")
                .replaceAll("^(一下|下|关于|看看|看一下)", "")
                .trim();
        return normalized.replaceAll("\\s+", " ").trim();
    }

    @Nonnull
    private static String buildSearchQuery(@Nonnull String normalizedQuery) {
        String searchQuery = normalizedQuery;
        if (searchQuery.matches(".*(是什么|什么意思|啥意思|含义|解释).*")) {
            searchQuery = searchQuery + " 中文 含义";
        }
        if (containsChinese(searchQuery) && !searchQuery.contains("-porn")) {
            searchQuery = searchQuery + " -porn -sex -xxx -nsfw";
        }
        return searchQuery;
    }

    @Nonnull
    private List<SearchResult> searchSearxng(@Nonnull String query) {
        List<String> instances = resolveSearxngInstances();
        for (String instance : instances) {
            List<SearchResult> results = searchSearxngInstance(instance, query);
            if (!results.isEmpty()) {
                LOGGER.debug("AI web search used SearXNG instance={}", safeInstanceForLog(instance));
                return results;
            }
        }
        return List.of();
    }

    @Nonnull
    private List<SearchResult> searchSearxngInstance(@Nonnull String instance, @Nonnull String query) {
        String endpoint = buildSearxngEndpoint(instance);
        String url = endpoint
                + "?format=json"
                + "&categories=general"
                + "&language=zh-CN"
                + "&safesearch=2"
                + "&q=" + URLEncoder.encode(query, StandardCharsets.UTF_8);
        HttpResponse response;
        try {
            response = HttpUtils.get(url)
                    .header("User-Agent", USER_AGENT)
                    .header("Accept", "application/json")
                    .header("Accept-Language", "zh-CN,zh;q=0.9,en;q=0.7")
                    .timeout(Duration.ofSeconds(5))
                    .execute();
        } catch (Exception e) {
            LOGGER.debug("AI SearXNG search request failed instance={} error={}", safeInstanceForLog(instance), e.toString());
            return List.of();
        }
        if (response == null || response.statusCode() < 200 || response.statusCode() >= 300) {
            LOGGER.debug("AI SearXNG search failed instance={} status={}", safeInstanceForLog(instance),
                    response != null ? response.statusCode() : null);
            return List.of();
        }
        return parseSearxngResults(response.getBodyAsString());
    }

    @Nonnull
    private static List<String> resolveSearxngInstances() {
        LinkedHashSet<String> instances = new LinkedHashSet<>();
        addConfiguredInstances(instances, System.getProperty(SEARXNG_URLS_PROPERTY));
        addConfiguredInstances(instances, System.getenv(SEARXNG_URLS_ENV));
        instances.addAll(PUBLIC_SEARXNG_FALLBACKS);
        return instances.stream()
                .filter(StringUtils::isNotNullOrEmpty)
                .limit(MAX_SEARXNG_INSTANCES)
                .toList();
    }

    private static void addConfiguredInstances(@Nonnull Set<String> target, String raw) {
        if (StringUtils.isNullOrEmptyEx(raw)) {
            return;
        }
        Arrays.stream(raw.split("[,;\\s]+"))
                .map(String::trim)
                .filter(StringUtils::isNotNullOrEmpty)
                .forEach(target::add);
    }

    @Nonnull
    private static String buildSearxngEndpoint(@Nonnull String instance) {
        String trimmed = instance.trim();
        while (trimmed.endsWith("/")) {
            trimmed = trimmed.substring(0, trimmed.length() - 1);
        }
        return trimmed.endsWith("/search") ? trimmed : trimmed + "/search";
    }

    @Nonnull
    private List<SearchResult> searchBing(@Nonnull String query) {
        String url = "https://www.bing.com/search?adlt=strict&setlang=zh-CN&q=" + URLEncoder.encode(query, StandardCharsets.UTF_8);
        HttpResponse response;
        try {
            response = HttpUtils.get(url)
                    .header("User-Agent", USER_AGENT)
                    .header("Accept-Language", "zh-CN,zh;q=0.9,en;q=0.7")
                    .timeout(Duration.ofSeconds(10))
                    .execute();
        } catch (Exception e) {
            LOGGER.debug("AI Bing search request failed error={}", e.toString());
            return List.of();
        }
        if (response == null || response.statusCode() < 200 || response.statusCode() >= 300) {
            LOGGER.debug("AI web search failed status={}", response != null ? response.statusCode() : null);
            return List.of();
        }
        return parseBingResults(response.getBodyAsString());
    }

    @Nonnull
    static List<SearchResult> parseSearxngResults(String json) {
        if (StringUtils.isNullOrEmptyEx(json)) {
            return List.of();
        }
        JsonObject root = JsonUtils.parseJsonObject(json);
        if (root == null || !root.has("results") || !root.get("results").isJsonArray()) {
            return List.of();
        }
        JsonArray array = root.getAsJsonArray("results");
        List<SearchResult> results = new ArrayList<>();
        for (JsonElement element : array) {
            if (element == null || !element.isJsonObject()) {
                continue;
            }
            JsonObject item = element.getAsJsonObject();
            String title = clean(jsonString(item, "title"));
            String url = clean(jsonString(item, "url"));
            String snippet = clean(firstNonBlank(
                    jsonString(item, "content"),
                    jsonString(item, "snippet")
            ));
            if (StringUtils.isNullOrEmptyEx(title) || StringUtils.isNullOrEmptyEx(url) || isLowValueResult(title, url, snippet)) {
                continue;
            }
            results.add(new SearchResult(title, url, snippet));
            if (results.size() >= MAX_RESULTS) {
                break;
            }
        }
        return results;
    }

    @Nonnull
    static List<SearchResult> parseBingResults(String html) {
        if (StringUtils.isNullOrEmptyEx(html)) {
            return List.of();
        }
        Document document = Jsoup.parse(html, "https://www.bing.com");
        List<SearchResult> results = new ArrayList<>();
        for (Element item : document.select("li.b_algo")) {
            Element link = item.selectFirst("h2 a[href]");
            if (link == null) {
                continue;
            }
            String title = clean(link.text());
            String url = normalizeResultUrl(clean(link.absUrl("href")));
            String snippet = cleanFirst(item,
                    ".b_caption p",
                    ".b_lineclamp2",
                    ".b_lineclamp3",
                    "p");
            if (StringUtils.isNullOrEmptyEx(title) || StringUtils.isNullOrEmptyEx(url) || isLowValueResult(title, url, snippet)) {
                continue;
            }
            results.add(new SearchResult(title, url, snippet));
            if (results.size() >= MAX_RESULTS) {
                break;
            }
        }
        return results;
    }

    private static boolean isLowValueResult(@Nonnull String title, @Nonnull String url, @Nonnull String snippet) {
        String haystack = (title + " " + url + " " + snippet).toLowerCase(Locale.ROOT);
        return haystack.contains("youtube help")
                || haystack.contains("porn")
                || haystack.contains("pussy")
                || haystack.contains("nsfw")
                || haystack.contains("xxx")
                || haystack.contains("sex")
                || haystack.contains("nipple")
                || haystack.contains("teenbeauties")
                || haystack.contains("freepussy")
                || haystack.contains("scrolller.com")
                || haystack.contains("reddit.com/r/nsfw")
                || haystack.contains("flickr.com/photos")
                || haystack.contains("magnific.com/photos")
                || haystack.contains("bing.com/ck/")
                || haystack.contains("bing.com/aclick")
                || haystack.contains("bing.com/search")
                || haystack.contains("support.google.com/youtube")
                || haystack.contains("accounts.google.com")
                || haystack.contains("microsoft support")
                || haystack.contains("google help")
                || haystack.contains("google account help")
                || haystack.contains("support.google.com")
                || haystack.contains("enable javascript")
                || haystack.contains("captcha");
    }

    private static boolean containsChinese(@Nonnull String text) {
        for (int i = 0; i < text.length(); i++) {
            char c = text.charAt(i);
            if (c >= '\u4e00' && c <= '\u9fff') {
                return true;
            }
        }
        return false;
    }

    @Nonnull
    private static String jsonString(@Nonnull JsonObject object, @Nonnull String key) {
        JsonElement value = object.get(key);
        if (value == null || value.isJsonNull()) {
            return "";
        }
        try {
            return value.getAsString();
        } catch (Exception ignored) {
            return "";
        }
    }

    @Nonnull
    private static String firstNonBlank(String... values) {
        if (values == null) {
            return "";
        }
        for (String value : values) {
            if (StringUtils.isNotNullOrEmpty(value)) {
                return value;
            }
        }
        return "";
    }

    @Nonnull
    private static String safeInstanceForLog(@Nonnull String instance) {
        try {
            URI uri = URI.create(instance);
            if (StringUtils.isNullOrEmptyEx(uri.getHost())) {
                return "(invalid)";
            }
            return uri.getScheme() + "://" + uri.getHost();
        } catch (Exception ignored) {
            return "(invalid)";
        }
    }

    @Nonnull
    private static String normalizeResultUrl(@Nonnull String url) {
        if (StringUtils.isNullOrEmptyEx(url)) {
            return "";
        }
        if (!url.contains("bing.com/ck/")) {
            return url;
        }
        try {
            String query = URI.create(url).getRawQuery();
            if (StringUtils.isNullOrEmptyEx(query)) {
                return "";
            }
            for (String part : query.split("&")) {
                int eq = part.indexOf('=');
                if (eq <= 0) {
                    continue;
                }
                String key = part.substring(0, eq);
                String value = java.net.URLDecoder.decode(part.substring(eq + 1), StandardCharsets.UTF_8);
                if ("u".equals(key) && value.startsWith("a1")) {
                    String decoded = new String(Base64.getUrlDecoder().decode(value.substring(2)), StandardCharsets.UTF_8);
                    return decoded.startsWith("http") ? decoded : "";
                }
                if ("u".equals(key) && value.startsWith("http")) {
                    return value;
                }
            }
        } catch (Exception ignored) {
        }
        return "";
    }

    @Nonnull
    private static String cleanFirst(@Nonnull Element root, String... selectors) {
        for (String selector : selectors) {
            Element element = root.selectFirst(selector);
            if (element != null && StringUtils.isNotNullOrEmpty(element.text())) {
                return clean(element.text());
            }
        }
        return "";
    }

    @Nonnull
    private static String clean(String text) {
        if (text == null) {
            return "";
        }
        String cleaned = text.contains("<") || text.contains("&")
                ? Jsoup.parse(text).text()
                : text;
        return cleaned.replace('\u00A0', ' ')
                .replaceAll("\\s+", " ")
                .trim();
    }

    public record SearchResult(String title, String url, String snippet) {
    }
}
