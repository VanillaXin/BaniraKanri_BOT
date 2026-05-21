package xin.vanilla.banira.util;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import lombok.extern.slf4j.Slf4j;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import xin.vanilla.banira.util.html.HtmlScreenshotUtils;
import xin.vanilla.banira.util.mcmod.*;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * MCMod 用户中心详情解析
 */
@Slf4j
public final class McModUserPageParser {

    private McModUserPageParser() {
    }

    private static final Pattern USER_ID_PATTERN = Pattern.compile("(?:center\\.mcmod\\.cn/|uid=)(\\d+)");
    private static final Pattern STAT_LINE_PATTERN = Pattern.compile("^(.+?)[:：]\\s*(.+)$");
    private static final Pattern CLASS_LINK_ID_PATTERN = Pattern.compile("/(class|modpack)/(\\d+)\\.html");
    private static final int DEVELOPER_MOD_LIMIT = 20;

    @Nullable
    public static McModUserPageDetail fetchDetail(@Nonnull McModSearchResult result, @Nonnull String typeName) {
        String userId = extractUserId(result.getLink());
        return fetchDetail(result.getLink(), userId, StringUtils.nullToEmpty(result.getTitle()), typeName);
    }

    @Nullable
    public static McModUserPageDetail fetchDetail(@Nullable String link, @Nullable String userId,
                                                  @Nullable String fallbackName, @Nonnull String typeName) {
        if (StringUtils.isNullOrEmpty(userId)) {
            userId = extractUserId(link);
        }
        if (StringUtils.isNullOrEmpty(userId)) {
            return null;
        }

        McModUserCardResult card = McModUtils.getUserCard(userId);
        if (card == null) {
            LOGGER.warn("Mcmod user card api failed, uid={}", userId);
            return null;
        }

        McModUserPageDetail detail = fromUserCard(card, userId, link, typeName, fallbackName);
        enrichFromUserCenterPage(detail);
        return detail.isRenderable() ? detail : null;
    }

    @Nullable
    public static String extractUserId(@Nullable String url) {
        if (StringUtils.isNullOrEmpty(url)) {
            return null;
        }
        Matcher matcher = USER_ID_PATTERN.matcher(url);
        return matcher.find() ? matcher.group(1) : null;
    }

    @Nonnull
    private static McModUserPageDetail fromUserCard(@Nonnull McModUserCardResult card, @Nonnull String userId,
                                                     @Nullable String link, @Nonnull String typeName,
                                                     @Nullable String fallbackName) {
        McModUserPageDetail detail = new McModUserPageDetail()
                .userId(userId)
                .link(StringUtils.isNotNullOrEmpty(link) ? link : McModUtils.getUserCenterUrl(userId))
                .typeName(typeName)
                .username(StringUtils.isNotNullOrEmpty(card.getUsername()) ? card.getUsername() : fallbackName)
                .avatarUrl(fixUrl(card.getAvatar()))
                .sign(card.getSign())
                .rank(card.getRank())
                .online(card.getOnline());

        if (card.getExp() != null) {
            detail.expTotal(card.getExp().getTotal())
                    .expYet(card.getExp().getYet())
                    .expRate(card.getExp().getRate());
        }
        if (card.getTracker() != null) {
            detail.trackers().addAll(card.getTracker());
        }
        if (card.getBadge() != null) {
            detail.badges().addAll(card.getBadge());
        }
        appendTrackerStats(detail);
        return detail;
    }

    private static void appendTrackerStats(@Nonnull McModUserPageDetail detail) {
        detail.trackers().forEach(tracker -> {
            if (StringUtils.isNullOrEmpty(tracker.getTitle()) || StringUtils.isNullOrEmpty(tracker.getValue())) {
                return;
            }
            String value = tracker.getValue();
            if (StringUtils.isNotNullOrEmpty(tracker.getRank()) && !"--".equals(tracker.getRank())) {
                value = value + " · 排名 " + tracker.getRank();
            }
            detail.stats().add(new McModPageProp(tracker.getTitle(), value));
        });
    }

    private static void enrichFromUserCenterPage(@Nonnull McModUserPageDetail detail) {
        String html = HtmlScreenshotUtils.fetchRemoteHtmlLoose(detail.link(), 6000);
        if (StringUtils.isNullOrEmpty(html) || html.length() < 500) {
            return;
        }
        try {
            Document document = Jsoup.parse(html, detail.link());
            parseUserMeta(document, detail);
            parseDeveloperMods(document, detail);
        } catch (Exception e) {
            LOGGER.debug("Failed to parse user center page: {}", detail.link(), e);
        }
    }

    private static void parseUserMeta(@Nonnull Document document, @Nonnull McModUserPageDetail detail) {
        document.select(".user-info li, .user-meta li, .profile-meta li, .panel-body li, .list-group-item").forEach(element -> {
            String label = cleanText(text(element.selectFirst("span, .t, .label, h4")));
            String value = cleanText(text(element.selectFirst(".n, .value, .text, p")));
            if (StringUtils.isNullOrEmpty(label) || StringUtils.isNullOrEmpty(value)) {
                String line = cleanText(element.text());
                Matcher matcher = STAT_LINE_PATTERN.matcher(line);
                if (matcher.matches()) {
                    label = matcher.group(1).trim();
                    value = matcher.group(2).trim();
                }
            }
            if (StringUtils.isNullOrEmpty(label) || StringUtils.isNullOrEmpty(value)) {
                return;
            }
            if (label.contains("用户组")) {
                detail.userGroup(value);
            } else if (label.contains("注册")) {
                detail.registerTime(value);
            }
            addStatIfAbsent(detail, label, value);
        });

        String bodyText = document.body() != null ? cleanText(document.body().text()) : "";
        if (StringUtils.isNullOrEmpty(detail.userGroup())) {
            detail.userGroup(extractStat(bodyText, "用户组[：:\\s]*([^\\s]+)"));
        }
        if (StringUtils.isNullOrEmpty(detail.userGroup())) {
            for (Element element : document.select("span, div, li, p, td")) {
                String line = cleanText(element.text());
                if (!line.contains("用户组")) {
                    continue;
                }
                Matcher matcher = STAT_LINE_PATTERN.matcher(line);
                if (matcher.matches()) {
                    detail.userGroup(matcher.group(2).trim());
                    break;
                }
            }
        }
        if (StringUtils.isNullOrEmpty(detail.registerTime())) {
            detail.registerTime(extractStat(bodyText, "于 (\\d{4}-\\d{2}-\\d{2}[\\s\\d:]*) 注册"));
            if (StringUtils.isNullOrEmpty(detail.registerTime())) {
                detail.registerTime(extractStat(bodyText, "注册时间[：:]\\s*(\\d{4}-\\d{2}-\\d{2}[\\s\\d:]*)"));
            }
        }
        addStatIfAbsent(detail, "总编辑次数", extractStat(bodyText, "总编辑次数[：:]\\s*([\\d,]+\\s*次?)"));
        addStatIfAbsent(detail, "总编辑字数", extractStat(bodyText, "总编辑字数[：:]\\s*([\\d,]+\\s*[^\\s]+)"));
        addStatIfAbsent(detail, "总短评数", extractStat(bodyText, "总短评数[：:]\\s*([\\d,]+)"));
        addStatIfAbsent(detail, "个人教程数", extractStat(bodyText, "个人教程数[：:]\\s*([\\d,]+)"));
    }

    private static void parseDeveloperMods(@Nonnull Document document, @Nonnull McModUserPageDetail detail) {
        Map<String, String> modNames = new LinkedHashMap<>();
        detail.developerMods().forEach(name -> {
            String key = normalizeModName(name);
            if (StringUtils.isNotNullOrEmpty(key)) {
                modNames.put(key, name);
            }
        });

        Elements links = document.select("a[href*='/class/'], a[href*='/modpack/']");
        for (Element link : links) {
            String href = link.attr("href");
            if (StringUtils.isNullOrEmpty(href)) {
                continue;
            }
            String name = cleanText(link.text());
            if (StringUtils.isNullOrEmpty(name) || name.length() > 80) {
                continue;
            }
            if (name.contains("查看更多") || name.contains("编辑动态") || name.contains("主页")) {
                continue;
            }
            String dedupeKey = resolveModDedupeKey(name, href);
            String existing = modNames.get(dedupeKey);
            if (existing == null || name.length() > existing.length()) {
                modNames.put(dedupeKey, name);
            }
            if (modNames.size() >= DEVELOPER_MOD_LIMIT) {
                break;
            }
        }

        detail.developerMods().clear();
        detail.developerMods().addAll(modNames.values());
    }

    @Nonnull
    private static String resolveModDedupeKey(@Nonnull String name, @Nonnull String href) {
        Matcher matcher = CLASS_LINK_ID_PATTERN.matcher(href);
        if (matcher.find()) {
            return matcher.group(1) + ":" + matcher.group(2);
        }
        return normalizeModName(name);
    }

    @Nonnull
    private static String normalizeModName(@Nonnull String name) {
        String normalized = name.replaceAll("^\\[[^\\]]+]\\s*", "").trim();
        normalized = normalized.replaceAll("\\s*\\([^)]*\\)\\s*$", "").trim();
        return normalized;
    }

    private static void addStatIfAbsent(@Nonnull McModUserPageDetail detail, @Nonnull String label, @Nullable String value) {
        if (StringUtils.isNullOrEmpty(value)) {
            return;
        }
        boolean exists = detail.stats().stream().anyMatch(item -> label.equals(item.label()));
        if (!exists) {
            detail.stats().add(new McModPageProp(label, value));
        }
    }

    @Nullable
    private static String extractStat(@Nonnull String text, @Nonnull String regex) {
        Matcher matcher = Pattern.compile(regex).matcher(text);
        if (matcher.find()) {
            return matcher.group(1).trim();
        }
        return null;
    }

    @Nullable
    private static String fixUrl(@Nullable String url) {
        if (StringUtils.isNullOrEmpty(url)) {
            return null;
        }
        if (url.startsWith("//")) {
            return "https:" + url;
        }
        return url;
    }

    @Nullable
    private static String text(@Nullable Element element) {
        return element != null ? element.text() : null;
    }

    @Nonnull
    private static String cleanText(@Nullable String text) {
        if (text == null) {
            return "";
        }
        return text.replace('\u00a0', ' ')
                .replaceAll("[ \\t\\f\\v]+", " ")
                .trim();
    }
}
