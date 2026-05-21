package xin.vanilla.banira.util;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import lombok.extern.slf4j.Slf4j;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import xin.vanilla.banira.domain.KeyValue;
import xin.vanilla.banira.util.http.HttpResponse;
import xin.vanilla.banira.util.html.HtmlScreenshotUtils;
import xin.vanilla.banira.util.mcmod.*;

import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * MCMod 详情页解析（优先网页抓取）
 */
@Slf4j
public final class McModPageParser {

    private McModPageParser() {
    }

    private static final String USER_AGENT =
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36";
    private static final Pattern SHORT_NAME_PATTERN = Pattern.compile("^\\[([^\\]]+)]");
    private static final Pattern ID_FROM_LINK_PATTERN = Pattern.compile("/(class|modpack|author)/(\\d+)\\.html");
    private static final Pattern YXD_TOKEN_PATTERN = Pattern.compile("yxd_token=([a-f0-9]+)");
    private static final Pattern LABEL_VALUE_PATTERN = Pattern.compile("^(.+?)[:：]\\s*(.+)$");
    private static final int DESCRIPTION_MAX_LENGTH = 260;
    private static final String[] VERSION_LOADERS = {
            "NeoForge", "Forge", "Fabric", "Quilt", "LiteLoader", "Rift", "DataPack"
    };
    private static final Pattern LOADER_HEAD_PATTERN = Pattern.compile(
            "(NeoForge|Forge|Fabric|Quilt|LiteLoader|Rift|DataPack)\\s*[:：]\\s*",
            Pattern.CASE_INSENSITIVE
    );
    private static final Pattern MC_VERSION_TOKEN_PATTERN = Pattern.compile(
            "\\d+\\.\\d+(?:\\.\\d+)?(?:-[\\w.]+)?"
    );
    private static volatile String cachedYxdToken;

    // region 入口

    @Nullable
    public static McModPageDetail fetchDetail(@Nonnull McModContent content, @Nonnull String typeName) {
        return fetchDetail(content, typeName, null);
    }

    @Nullable
    public static McModPageDetail fetchDetail(@Nonnull McModContent content, @Nonnull String typeName,
                                              @Nullable Long groupId) {
        McModPageDetail detail = fetchDetail(content.getDetailUrl(), content.getType(), typeName, groupId);
        if (detail == null) {
            return null;
        }
        if (StringUtils.isNullOrEmpty(detail.shortName()) && StringUtils.isNotNullOrEmpty(content.getShortName())) {
            detail.shortName(content.getShortName());
        }
        if (StringUtils.isNullOrEmpty(detail.title())) {
            detail.title(content.getFormattedName());
        }
        if (StringUtils.isNullOrEmpty(detail.id())) {
            detail.id(String.valueOf(content.getId()));
        }
        if (StringUtils.isNullOrEmpty(detail.link())) {
            detail.link(content.getDetailUrl());
        }
        if (StringUtils.isNullOrEmpty(detail.coverUrl()) && StringUtils.isNotNullOrEmpty(content.getCoverImageUrl())) {
            detail.coverUrl(content.getCoverImageUrl());
        }
        if (StringUtils.isNullOrEmpty(detail.iconUrl()) && StringUtils.isNotNullOrEmpty(content.getCoverImageUrl())) {
            detail.iconUrl(content.getCoverImageUrl());
        }
        return detail;
    }

    @Nullable
    public static McModPageDetail fetchDetail(@Nonnull McModSearchResult result, @Nonnull String typeName) {
        return fetchDetail(result, typeName, null);
    }

    @Nullable
    public static McModPageDetail fetchDetail(@Nonnull McModSearchResult result, @Nonnull String typeName,
                                              @Nullable Long groupId) {
        String link = result.getLink();
        if (StringUtils.isNullOrEmpty(link)) {
            return null;
        }
        EnumContentType type = resolveContentType(link, typeName);
        McModPageDetail detail = fetchDetail(link, type, typeName, groupId);
        if (detail == null) {
            return null;
        }
        if (StringUtils.isNullOrEmpty(detail.title()) && StringUtils.isNotNullOrEmpty(result.getTitle())) {
            detail.title(result.getTitle());
        }
        if (StringUtils.isNullOrEmpty(detail.subTitle()) && StringUtils.isNotNullOrEmpty(result.getSubtitle())) {
            detail.subTitle(result.getSubtitle());
        }
        if (StringUtils.isNullOrEmpty(detail.description()) && StringUtils.isNotNullOrEmpty(result.getSummary())) {
            detail.description(result.getSummary());
        }
        if (StringUtils.isNullOrEmpty(detail.coverUrl()) && StringUtils.isNotNullOrEmpty(result.getImageUrl())) {
            detail.coverUrl(result.getImageUrl());
        }
        if (StringUtils.isNullOrEmpty(detail.iconUrl()) && StringUtils.isNotNullOrEmpty(result.getImageUrl())) {
            detail.iconUrl(result.getImageUrl());
        }
        return detail;
    }

    @Nullable
    public static McModPageDetail fetchDetail(@Nonnull String url, @Nullable EnumContentType type,
                                              @Nonnull String typeName) {
        return fetchDetail(url, type, typeName, null);
    }

    @Nullable
    public static McModPageDetail fetchDetail(@Nonnull String url, @Nullable EnumContentType type,
                                              @Nonnull String typeName, @Nullable Long groupId) {
        String html = fetchPageHtml(url, groupId);
        if (StringUtils.isNullOrEmpty(html)) {
            return null;
        }
        try {
            Document document = Jsoup.parse(html, url);
            EnumContentType contentType = type != null ? type : resolveContentType(url, typeName);
            McModPageDetail detail = new McModPageDetail()
                    .contentType(contentType)
                    .typeName(typeName)
                    .link(url)
                    .pageFetched(true);
            Matcher idMatcher = ID_FROM_LINK_PATTERN.matcher(url);
            if (idMatcher.find()) {
                detail.id(idMatcher.group(2));
            }
            if (contentType == EnumContentType.AUTHOR) {
                parseAuthorPage(document, detail);
            } else {
                parseClassPage(document, detail);
            }
            fillShortName(detail);
            syncLegacyAuthors(detail);
            return detail;
        } catch (Exception e) {
            LOGGER.warn("Failed to parse mcmod page: {}", url, e);
            return null;
        }
    }

    // endregion 入口

    // region 页面解析

    private static void parseClassPage(@Nonnull Document document, @Nonnull McModPageDetail detail) {
        parseClassTitle(document, detail);
        parseClassImages(document, detail);
        parseOfficialTags(document, detail);
        parseCategories(document, detail);
        parseClassInfo(document, detail);
        parseLegacyMeta(document, detail);
        parseClassStats(document, detail);
        parseVoteInfo(document, detail);
        parseUserInteractionState(document, detail);
        parseAuthorDetails(document, detail);
        parseVersions(document, detail);
        parseRelatedMods(document, detail);
        parseLinks(document, detail);
        detail.description(parseDescription(document));
    }

    private static void parseClassTitle(@Nonnull Document document, @Nonnull McModPageDetail detail) {
        Element titleElement = document.selectFirst(".class-title");
        if (titleElement == null) {
            return;
        }

        String shortName = cleanText(text(titleElement.selectFirst(".short-name")));
        if (StringUtils.isNotNullOrEmpty(shortName)) {
            if (shortName.startsWith("[") && shortName.endsWith("]")) {
                shortName = shortName.substring(1, shortName.length() - 1).trim();
            }
            detail.shortName(shortName);
        }

        String mainName = cleanText(text(titleElement.selectFirst("h3")));
        String secondaryName = cleanText(text(titleElement.selectFirst("h4")));
        if (StringUtils.isNotNullOrEmpty(mainName) || StringUtils.isNotNullOrEmpty(secondaryName)) {
            detail.title(buildDisplayTitle(detail.shortName(), mainName, secondaryName));
            if (StringUtils.isNotNullOrEmpty(secondaryName)) {
                detail.subTitle(secondaryName);
            }
            return;
        }

        Element titleClone = titleElement.clone();
        titleClone.select(".class-official-group, .short-name").remove();
        String titleText = cleanText(titleClone.text());
        if (StringUtils.isNotNullOrEmpty(titleText)) {
            detail.title(titleText);
        }
    }

    private static void parseClassImages(@Nonnull Document document, @Nonnull McModPageDetail detail) {
        String coverUrl = fixUrl(document, firstImageUrl(document.selectFirst(".class-cover-image img")));
        String iconUrl = fixUrl(document, firstImageUrl(document.selectFirst(".class-icon img")));
        if (StringUtils.isNullOrEmpty(iconUrl)) {
            iconUrl = coverUrl;
        }
        if (StringUtils.isNullOrEmpty(coverUrl)) {
            coverUrl = iconUrl;
        }
        detail.coverUrl(coverUrl).iconUrl(iconUrl);
    }

    private static void parseCategories(@Nonnull Document document, @Nonnull McModPageDetail detail) {
        document.select(".common-class-category li a").forEach(element -> {
            String category = cleanText(element.text());
            if (StringUtils.isNullOrEmpty(category)) {
                Element useElement = element.selectFirst("use");
                if (useElement != null) {
                    String href = useElement.attr("xlink:href");
                    if (StringUtils.isNullOrEmpty(href)) {
                        href = useElement.attr("href");
                    }
                    if (href.contains("category-7")) {
                        category = "LIB";
                    } else if (href.contains("category-")) {
                        category = "CAT";
                    }
                }
            }
            if (StringUtils.isNullOrEmpty(category)) {
                String tooltip = cleanText(element.attr("data-original-title"));
                if (StringUtils.isNotNullOrEmpty(tooltip) && tooltip.length() <= 8) {
                    category = tooltip;
                }
            }
            if (StringUtils.isNotNullOrEmpty(category) && category.length() <= 12) {
                detail.categories().add(category);
            }
        });
    }

    private static void parseClassInfo(@Nonnull Document document, @Nonnull McModPageDetail detail) {
        document.select(".class-info-left > ul > li").forEach(element -> {
            if (element.hasClass("tag") || element.hasClass("author")) {
                if (element.hasClass("tag")) {
                    parseTagLine(element, detail);
                }
                return;
            }
            String line = cleanText(element.text());
            if (StringUtils.isNullOrEmpty(line)) {
                return;
            }
            if (line.contains("支持的MC版本")) {
                Matcher versionMatcher = LABEL_VALUE_PATTERN.matcher(line);
                if (versionMatcher.matches()) {
                    parseVersionsFromText(versionMatcher.group(2).trim(), detail);
                }
                return;
            }
            if (line.contains("Mod作者") || line.contains("开发团队")) {
                return;
            }
            Matcher matcher = LABEL_VALUE_PATTERN.matcher(line);
            if (matcher.matches()) {
                detail.metaItems().add(new McModPageProp(matcher.group(1).trim(), matcher.group(2).trim()));
            }
        });
    }

    private static void parseTagLine(@Nonnull Element element, @Nonnull McModPageDetail detail) {
        Set<String> existingTags = new HashSet<>();
        detail.tags().forEach(tag -> existingTags.add(tag.text()));
        element.select("ul li a").forEach(link -> {
            String labelText = cleanText(link.text()).replaceFirst("^#?", "").trim();
            if (StringUtils.isNullOrEmpty(labelText) || existingTags.contains(labelText)) {
                return;
            }
            detail.tags().add(new McModPageTag(labelText, "#e3f2fd", "#3498db"));
            existingTags.add(labelText);
        });
    }

    private static void parseLegacyMeta(@Nonnull Document document, @Nonnull McModPageDetail detail) {
        if (!detail.metaItems().isEmpty()) {
            return;
        }
        document.select(".class-meta-list li").forEach(element -> {
            String label = cleanText(text(element.selectFirst("h4")));
            String value = cleanText(text(element.selectFirst(".text")));
            if (StringUtils.isNotNullOrEmpty(label) && StringUtils.isNotNullOrEmpty(value)) {
                detail.metaItems().add(new McModPageProp(label, value));
            }
        });
    }

    private static void parseAuthorDetails(@Nonnull Document document, @Nonnull McModPageDetail detail) {
        Elements authorItems = document.select("li.col-lg-12.author .frame ul li, .author-list li, .author li");
        authorItems.forEach(element -> {
            String name = cleanText(text(element.selectFirst(".name")));
            if (StringUtils.isNullOrEmpty(name)) {
                name = cleanText(text(element.selectFirst(".name a")));
            }
            if (StringUtils.isNullOrEmpty(name)) {
                return;
            }
            String role = cleanText(text(element.selectFirst(".position")));
            String avatarUrl = fixUrl(document, firstImageUrl(element.selectFirst(".avatar img, img")));
            detail.authorDetails().add(new McModPageAuthor(name, role, avatarUrl));
        });
    }

    private static void parseVoteInfo(@Nonnull Document document, @Nonnull McModPageDetail detail) {
        document.select(".class-card .text-block span, .text-block span").forEach(element -> {
            String text = cleanText(element.text());
            if (StringUtils.isNullOrEmpty(text)) {
                return;
            }
            if (text.contains("红票")) {
                detail.redVote(text);
            } else if (text.contains("黑票")) {
                detail.blackVote(text);
            }
        });
    }

    private static void parseUserInteractionState(@Nonnull Document document, @Nonnull McModPageDetail detail) {
        Element group = document.selectFirst(".common-fuc-group");
        if (group == null) {
            return;
        }
        McModUserInteractionState state = new McModUserInteractionState().loggedIn(true);
        parseFucActionState(document, "push", state);
        parseFucActionState(document, "like", state);
        parseFucActionState(document, "subscribe", state);
        if (StringUtils.isNotNullOrEmpty(detail.redVote()) && detail.redVote().contains("已投红票")) {
            state.redVoted(true);
        }
        if (StringUtils.isNotNullOrEmpty(detail.blackVote()) && detail.blackVote().contains("已投黑票")) {
            state.blackVoted(true);
        }
        detail.userInteraction(state);
    }

    private static void parseFucActionState(@Nonnull Document document, @Nonnull String className,
                                            @Nonnull McModUserInteractionState state) {
        Element item = document.selectFirst(".common-fuc-group li." + className);
        if (item == null) {
            return;
        }
        Element icon = item.selectFirst(".action i");
        Element label = item.selectFirst(".action span");
        String iconClass = icon != null ? icon.className() : "";
        String actionText = label != null ? cleanText(label.text()) : "";
        boolean active = iconClass.contains("fas");
        switch (className) {
            case "push" -> {
                if (actionText.contains("冷却")) {
                    state.pushCooldown(true).pushed(true);
                } else if (active || actionText.contains("已")) {
                    state.pushed(true);
                }
            }
            case "like" -> {
                if (active || actionText.contains("已")) {
                    state.favorited(true);
                }
            }
            case "subscribe" -> {
                if (active || actionText.contains("已")) {
                    state.subscribed(true);
                }
            }
            default -> {
            }
        }
    }

    private static void parseRelatedMods(@Nonnull Document document, @Nonnull McModPageDetail detail) {
        document.select("fieldset ul li a[href*='/class/'], fieldset ul li a[href*='/modpack/']").forEach(element -> {
            String name = cleanText(element.text());
            if (StringUtils.isNullOrEmpty(name)) {
                name = cleanText(element.attr("data-original-title"));
            }
            if (StringUtils.isNotNullOrEmpty(name)) {
                detail.relatedMods().add(name);
            }
        });
    }

    private static void parseAuthorPage(@Nonnull Document document, @Nonnull McModPageDetail detail) {
        String username = cleanText(text(document.selectFirst(".author-name h5")));
        if (StringUtils.isNullOrEmpty(username)) {
            String pageTitle = document.title();
            if (StringUtils.isNotNullOrEmpty(pageTitle)) {
                username = pageTitle.split("-")[0].trim();
            }
        }
        detail.title(username);

        String subname = document.select(".author-name .subname p").stream()
                .map(Element::text)
                .map(McModPageParser::cleanText)
                .filter(StringUtils::isNotNullOrEmpty)
                .reduce((a, b) -> a + " / " + b)
                .orElse("");
        if (StringUtils.isNotNullOrEmpty(subname)) {
            detail.subTitle(subname);
        }

        String avatarUrl = fixUrl(document, attr(document.selectFirst(".author-user-avatar img"), "src"));
        detail.coverUrl(avatarUrl).iconUrl(avatarUrl);

        String bio = cleanText(text(document.selectFirst(".author-content .text")));
        if (StringUtils.isNotNullOrEmpty(bio)) {
            detail.bio(truncate(bio, DESCRIPTION_MAX_LENGTH));
            detail.description(detail.bio());
        }

        String fullText = document.body() != null ? document.body().text().replaceAll("\\s+", " ") : "";
        addMetaItem(detail, "浏览量", extractStat(fullText, "浏览量[：:]\\s*([\\d,]+)"));
        addMetaItem(detail, "创建日期", extractStat(fullText, "创建日期[：:]\\s*(\\d{4}-\\d{2}-\\d{2}|\\d+年前|\\d+个月前|\\d+天前)"));
        addMetaItem(detail, "最后编辑", extractStat(fullText, "最后编辑[：:]\\s*(\\d{4}-\\d{2}-\\d{2}|\\d+年前|\\d+个月前|\\d+天前)"));
        addMetaItem(detail, "编辑次数", extractStat(fullText, "编辑次数[：:]\\s*(\\d+)"));

        String viewNum = extractStat(fullText, "浏览量[：:]\\s*([\\d,]+)");
        if (StringUtils.isNotNullOrEmpty(viewNum)) {
            detail.viewNum(viewNum);
        }

        Element favElement = document.selectFirst(".author-fav .nums, .common-fuc-group li.like .nums, .fav-count");
        if (favElement != null) {
            String favCount = StringUtils.nullToEmpty(favElement.attr("title"));
            if (StringUtils.isNullOrEmpty(favCount)) {
                favCount = cleanText(favElement.text());
            }
            detail.favNum(favCount);
        }

        document.select(".author-link .common-link-icon-list a, .common-link-icon-frame a").forEach(link -> {
            String name = simplifyLinkName(link.attr("data-original-title"), link.attr("href"), cleanText(link.text()));
            if (StringUtils.isNotNullOrEmpty(name)) {
                detail.links().add(name);
            }
        });
        parseUserInteractionState(document, detail);
    }

    private static void parseOfficialTags(@Nonnull Document document, @Nonnull McModPageDetail detail) {
        document.select(".class-official-group div").forEach(element -> {
            String text = cleanText(element.text());
            if (StringUtils.isNullOrEmpty(text) || text.length() > 20) {
                return;
            }
            String color = "#999";
            String bg = "#eee";
            if (text.contains("开源") || text.contains("活跃") || text.contains("稳定")) {
                color = "#2ecc71";
                bg = "#e8f5e9";
            } else if (text.contains("半弃坑") || text.contains("Beta")) {
                color = "#f39c12";
                bg = "#fef9e7";
            } else if (text.contains("停更") || text.contains("闭源") || text.contains("弃坑")) {
                color = "#e74c3c";
                bg = "#fce4ec";
            }
            detail.tags().add(new McModPageTag(text, bg, color));
        });
    }

    private static void parseLabelTags(@Nonnull Document document, @Nonnull McModPageDetail detail) {
        Set<String> officialTags = new HashSet<>();
        detail.tags().forEach(tag -> officialTags.add(tag.text()));
        document.select(".class-label-list a").forEach(element -> {
            String labelText = cleanText(element.text());
            if (StringUtils.isNullOrEmpty(labelText) || officialTags.contains(labelText)) {
                return;
            }
            String bg = "#e3f2fd";
            String color = "#3498db";
            String cls = element.className();
            if (cls.contains("c_1")) {
                bg = "#e8f5e9";
                color = "#2ecc71";
            } else if (cls.contains("c_3")) {
                bg = "#fff3e0";
                color = "#e67e22";
            }
            detail.tags().add(new McModPageTag(labelText, bg, color));
        });
    }

    private static void parseClassStats(@Nonnull Document document, @Nonnull McModPageDetail detail) {
        String score = cleanText(text(document.selectFirst(".class-score-num")));
        String scoreComment = "";
        if (StringUtils.isNullOrEmpty(score)) {
            score = cleanText(text(document.selectFirst(".class-excount .star .up")));
            scoreComment = cleanText(text(document.selectFirst(".class-excount .star .down")));
        }
        if (StringUtils.isNullOrEmpty(scoreComment)) {
            scoreComment = "暂无评价";
        }
        detail.score(score).scoreComment(scoreComment);

        document.select(".class-excount .star .text").forEach(element -> {
            String text = cleanText(element.text());
            if (StringUtils.isNullOrEmpty(text)) {
                return;
            }
            if (text.contains("昨日平均")) {
                detail.yIndexAvg(text.replace("昨日平均指数:", "").replace("昨日平均指数：", "").trim());
            } else if (text.contains("昨日指数")) {
                detail.yIndex(text.replace("昨日指数:", "").replace("昨日指数：", "").trim());
            }
        });

        document.select(".class-excount .infos .span").forEach(element -> {
            String label = element.selectFirst(".t") != null ? element.selectFirst(".t").text() : "";
            String value = cleanText(element.selectFirst(".n") != null ? element.selectFirst(".n").text() : "");
            if (label.contains("浏览") || label.contains("总浏览")) {
                detail.viewNum(value);
            }
            if (label.contains("填充")) {
                detail.fillRate(value);
            }
        });

        detail.pushNum(getSocialNum(document, "push"));
        detail.favNum(getSocialNum(document, "like"));
        detail.subscribeNum(getSocialNum(document, "subscribe"));
        parseLabelTags(document, detail);
    }

    private static void parseVersions(@Nonnull Document document, @Nonnull McModPageDetail detail) {
        if (!detail.versions().isEmpty()) {
            return;
        }
        Element mcVerRoot = document.selectFirst(".mcver, .class-info-left .mcver, li.mcver");
        if (mcVerRoot == null) {
            return;
        }
        Elements allUls = mcVerRoot.select("ul");
        allUls.forEach(ul -> {
            if (!ul.select("ul").isEmpty()) {
                return;
            }
            final String[] loaderHolder = {""};
            StringBuilder versions = new StringBuilder();
            ul.select("li").forEach(li -> {
                String txt = cleanText(li.text());
                if (StringUtils.isNullOrEmpty(txt)) {
                    return;
                }
                if (txt.contains(":") || txt.contains("：")) {
                    loaderHolder[0] = txt.replace(":", "").replace("：", "").trim();
                } else {
                    if (!versions.isEmpty()) {
                        versions.append(", ");
                    }
                    versions.append(txt);
                }
            });
            if (StringUtils.isNotNullOrEmpty(loaderHolder[0]) && !versions.isEmpty()) {
                String loader = normalizeLoaderName(loaderHolder[0]);
                String cleaned = cleanVersionText(versions.toString());
                if (StringUtils.isNotNullOrEmpty(cleaned)) {
                    detail.versions().add(new McModPageVersion(loader, cleaned));
                }
            }
        });
        if (detail.versions().isEmpty()) {
            document.select(".class-info-left > ul > li").forEach(element -> {
                String line = cleanText(element.text());
                if (line.contains("支持的MC版本")) {
                    Matcher matcher = LABEL_VALUE_PATTERN.matcher(line);
                    if (matcher.matches()) {
                        parseVersionsFromText(matcher.group(2).trim(), detail);
                    }
                }
            });
        }
    }

    private static void parseVersionsFromText(@Nonnull String text, @Nonnull McModPageDetail detail) {
        if (StringUtils.isNullOrEmpty(text) || !detail.versions().isEmpty()) {
            return;
        }
        Matcher headMatcher = LOADER_HEAD_PATTERN.matcher(text);
        String lastLoader = null;
        int lastValueStart = -1;
        while (headMatcher.find()) {
            if (lastLoader != null && lastValueStart >= 0) {
                addVersionEntry(detail, lastLoader, text.substring(lastValueStart, headMatcher.start()));
            }
            lastLoader = normalizeLoaderName(headMatcher.group(1));
            lastValueStart = headMatcher.end();
        }
        if (lastLoader != null && lastValueStart >= 0) {
            addVersionEntry(detail, lastLoader, text.substring(lastValueStart));
        }
    }

    private static void addVersionEntry(@Nonnull McModPageDetail detail, @Nonnull String loader, @Nonnull String raw) {
        String cleaned = cleanVersionText(raw);
        if (StringUtils.isNotNullOrEmpty(cleaned)) {
            detail.versions().add(new McModPageVersion(loader, cleaned));
        }
    }

    @Nonnull
    private static String normalizeLoaderName(@Nonnull String loader) {
        for (String known : VERSION_LOADERS) {
            if (known.equalsIgnoreCase(loader.trim())) {
                return known;
            }
        }
        return loader.trim();
    }

    @Nonnull
    private static String cleanVersionText(@Nonnull String raw) {
        LinkedHashSet<String> tokens = new LinkedHashSet<>();
        Matcher matcher = MC_VERSION_TOKEN_PATTERN.matcher(raw);
        while (matcher.find()) {
            tokens.add(matcher.group());
        }
        if (!tokens.isEmpty()) {
            return String.join(", ", tokens);
        }
        for (String segment : raw.split("[,，;；\\s]+")) {
            segment = stripLoaderNames(segment.trim());
            if (StringUtils.isNullOrEmpty(segment) || isLoaderName(segment)) {
                continue;
            }
            if (segment.matches("[\\d.\\w-]+")) {
                tokens.add(segment);
            }
        }
        return String.join(", ", tokens);
    }

    @Nonnull
    private static String stripLoaderNames(@Nonnull String text) {
        String result = text;
        for (String loader : VERSION_LOADERS) {
            result = result.replaceAll("(?i)" + Pattern.quote(loader), "");
        }
        return result.replaceAll("^[:：\\s,，]+", "").replaceAll("[:：\\s,，]+$", "").trim();
    }

    private static boolean isLoaderName(@Nullable String text) {
        if (StringUtils.isNullOrEmpty(text)) {
            return false;
        }
        for (String loader : VERSION_LOADERS) {
            if (loader.equalsIgnoreCase(text.trim())) {
                return true;
            }
        }
        return false;
    }

    private static void parseLinks(@Nonnull Document document, @Nonnull McModPageDetail detail) {
        document.select(".common-link-icon-frame a").forEach(element -> {
            String name = simplifyLinkName(element.attr("data-original-title"), element.attr("href"), cleanText(element.text()));
            if (StringUtils.isNotNullOrEmpty(name)) {
                detail.links().add(name);
            }
        });
    }

    @Nullable
    private static String parseDescription(@Nonnull Document document) {
        Element descRoot = document.selectFirst(".common-text");
        if (descRoot == null) {
            return null;
        }
        String description = cleanText(descRoot.text());
        if (StringUtils.isNullOrEmpty(description)) {
            return null;
        }
        return truncate(description, DESCRIPTION_MAX_LENGTH);
    }

    // endregion 页面解析

    // region private

    @Nullable
    private static String fetchPageHtml(@Nonnull String url) {
        return fetchPageHtml(url, null);
    }

    @Nullable
    private static String fetchPageHtml(@Nonnull String url, @Nullable Long groupId) {
        try {
            String userCookie = McModUtils.resolveOptionalCookie(groupId);
            if (StringUtils.isNotNullOrEmpty(userCookie)) {
                String loggedInHtml = fetchPageHtmlViaHttp(url, userCookie);
                if (isDetailHtml(loggedInHtml)) {
                    LOGGER.info("Mcmod page fetched via HTTP (logged in), url={}, length={}",
                            url, loggedInHtml.length());
                    return loggedInHtml;
                }
            }

            String html = HtmlScreenshotUtils.fetchRemoteHtml(
                    url, ".class-title, .class-info, .author-name, .class-meta-list");
            if (isDetailHtml(html)) {
                LOGGER.info("Mcmod page fetched via Playwright, url={}, length={}", url, html.length());
                return html;
            }

            html = fetchPageHtmlViaHttp(url, null);
            if (isDetailHtml(html)) {
                LOGGER.info("Mcmod page fetched via HTTP, url={}, length={}", url, html.length());
                return html;
            }

            LOGGER.warn("Mcmod page fetch failed, url={}, playwrightLength={}",
                    url, html == null ? 0 : html.length());
            return null;
        } catch (Exception e) {
            LOGGER.warn("Failed to fetch mcmod page: {}", url, e);
            return null;
        }
    }

    @Nullable
    private static String fetchPageHtmlViaHttp(@Nonnull String url) {
        return fetchPageHtmlViaHttp(url, null);
    }

    @Nullable
    private static String fetchPageHtmlViaHttp(@Nonnull String url, @Nullable String userCookie) {
        try {
            String html = requestPage(url, cachedYxdToken, userCookie);
            if (isDetailHtml(html)) {
                return html;
            }

            String token = extractYxdToken(html);
            if (StringUtils.isNullOrEmpty(token)) {
                token = bootstrapYxdToken(userCookie);
            }
            if (StringUtils.isNullOrEmpty(token)) {
                return null;
            }
            cachedYxdToken = token;

            html = requestPage(url, token, userCookie);
            return isDetailHtml(html) ? html : null;
        } catch (Exception e) {
            LOGGER.warn("Failed to fetch mcmod page via HTTP: {}", url, e);
            return null;
        }
    }

    @Nullable
    private static String bootstrapYxdToken(@Nullable String userCookie) {
        if (StringUtils.isNotNullOrEmpty(cachedYxdToken)) {
            return cachedYxdToken;
        }
        String[] bootstrapUrls = {
                "https://www.mcmod.cn/",
                "https://www.mcmod.cn/class/1.html"
        };
        for (String bootstrapUrl : bootstrapUrls) {
            String html = requestPage(bootstrapUrl, null, userCookie);
            String token = extractYxdToken(html);
            if (StringUtils.isNotNullOrEmpty(token)) {
                cachedYxdToken = token;
                return token;
            }
        }
        return null;
    }

    @Nullable
    private static String requestPage(@Nonnull String url, @Nullable String yxdToken) {
        return requestPage(url, yxdToken, null);
    }

    @Nullable
    private static String requestPage(@Nonnull String url, @Nullable String yxdToken, @Nullable String userCookie) {
        HttpResponse response;
        String cookieHeader = buildRequestCookie(yxdToken, userCookie);
        if (StringUtils.isNotNullOrEmpty(cookieHeader)) {
            response = HttpUtils.get(url,
                    new KeyValue<>("User-Agent", USER_AGENT),
                    new KeyValue<>("Referer", "https://www.mcmod.cn"),
                    new KeyValue<>("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"),
                    new KeyValue<>("Accept-Language", "zh-CN,zh;q=0.9"),
                    new KeyValue<>("Cookie", cookieHeader));
        } else {
            response = HttpUtils.get(url,
                    new KeyValue<>("User-Agent", USER_AGENT),
                    new KeyValue<>("Referer", "https://www.mcmod.cn"),
                    new KeyValue<>("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"),
                    new KeyValue<>("Accept-Language", "zh-CN,zh;q=0.9"));
        }
        if (response == null || response.statusCode() < 200 || response.statusCode() >= 300) {
            return null;
        }
        return response.getBodyAsString();
    }

    @Nullable
    private static String buildRequestCookie(@Nullable String yxdToken, @Nullable String userCookie) {
        StringBuilder builder = new StringBuilder();
        if (StringUtils.isNotNullOrEmpty(userCookie)) {
            builder.append(userCookie.trim());
        }
        if (StringUtils.isNotNullOrEmpty(yxdToken)) {
            if (!builder.isEmpty()) {
                builder.append("; ");
            }
            builder.append("yxd_token=").append(yxdToken);
        }
        return builder.isEmpty() ? null : builder.toString();
    }

    private static boolean isDetailHtml(@Nullable String html) {
        if (StringUtils.isNullOrEmpty(html)) {
            return false;
        }
        return html.contains("class-title")
                || html.contains("author-name")
                || html.contains("class-info")
                || html.contains("class-meta-list");
    }

    @Nullable
    private static String extractYxdToken(@Nonnull String html) {
        Matcher matcher = YXD_TOKEN_PATTERN.matcher(html);
        return matcher.find() ? matcher.group(1) : null;
    }

    @Nonnull
    private static EnumContentType resolveContentType(@Nonnull String url, @Nonnull String typeName) {
        if (url.contains("/modpack/") || typeName.contains("整合包")) {
            return EnumContentType.MODPACK;
        }
        if (url.contains("/author/") || typeName.contains("作者")) {
            return EnumContentType.AUTHOR;
        }
        return EnumContentType.MOD;
    }

    private static void fillShortName(@Nonnull McModPageDetail detail) {
        if (StringUtils.isNotNullOrEmpty(detail.shortName())) {
            return;
        }
        Matcher matcher = SHORT_NAME_PATTERN.matcher(StringUtils.nullToEmpty(detail.title()));
        if (matcher.find()) {
            detail.shortName(matcher.group(1).trim());
        }
    }

    private static void syncLegacyAuthors(@Nonnull McModPageDetail detail) {
        if (detail.authors().isEmpty()) {
            detail.authorDetails().forEach(author -> detail.authors().add(author.name()));
        }
    }

    @Nonnull
    private static String buildDisplayTitle(@Nullable String shortName, @Nullable String mainName, @Nullable String secondaryName) {
        StringBuilder builder = new StringBuilder();
        if (StringUtils.isNotNullOrEmpty(shortName)) {
            builder.append("[").append(shortName).append("] ");
        }
        if (StringUtils.isNotNullOrEmpty(mainName)) {
            builder.append(mainName);
        }
        if (StringUtils.isNotNullOrEmpty(secondaryName)) {
            if (!builder.isEmpty()) {
                builder.append(" ");
            }
            builder.append(secondaryName);
        }
        return builder.toString().trim();
    }

    private static void addMetaItem(@Nonnull McModPageDetail detail, @Nonnull String label, @Nullable String value) {
        if (StringUtils.isNotNullOrEmpty(value)) {
            detail.metaItems().add(new McModPageProp(label, value));
        }
    }

    @Nullable
    private static String extractStat(@Nonnull String text, @Nonnull String regex) {
        Matcher matcher = Pattern.compile(regex).matcher(text);
        if (matcher.find()) {
            String value = matcher.group(1);
            if (value != null && value.length() < 20) {
                return value.trim();
            }
        }
        return null;
    }

    private static String getSocialNum(@Nonnull Document document, @Nonnull String className) {
        String[] selectors = {
                ".common-fuc-group li." + className + " div.nums",
                ".common-fuc-group li." + className + " .nums",
                "li." + className + " div.nums",
                "li." + className + " .nums"
        };
        for (String selector : selectors) {
            Element element = document.selectFirst(selector);
            if (element == null) {
                continue;
            }
            String titleAttr = element.attr("title");
            if (StringUtils.isNotNullOrEmpty(titleAttr) && titleAttr.replace(",", "").trim().matches("\\d+")) {
                return titleAttr.replace(",", "").trim();
            }
            String text = cleanText(element.text()).replace(",", "");
            if (StringUtils.isNotNullOrEmpty(text) && text.matches("\\d+")) {
                return text;
            }
        }
        return "0";
    }

    @Nullable
    private static String simplifyLinkName(@Nullable String title, @Nullable String href, @Nullable String text) {
        String name = StringUtils.isNotNullOrEmpty(title) ? title : text;
        if (StringUtils.isNullOrEmpty(name) && StringUtils.isNotNullOrEmpty(href)) {
            if (href.contains("github")) {
                name = "GitHub";
            } else if (href.contains("curseforge")) {
                name = "CurseForge";
            } else if (href.contains("modrinth")) {
                name = "Modrinth";
            } else if (href.contains("bilibili")) {
                name = "Bilibili";
            } else if (href.contains("mcbbs")) {
                name = "MCBBS";
            } else {
                name = "Link";
            }
        }
        if (StringUtils.isNullOrEmpty(name)) {
            return null;
        }
        if (name.contains("GitHub")) {
            return "GitHub";
        }
        if (name.contains("CurseForge")) {
            return "CurseForge";
        }
        if (name.contains("Modrinth")) {
            return "Modrinth";
        }
        if (name.contains("百科")) {
            return "Wiki";
        }
        return name;
    }

    @Nullable
    private static String fixUrl(@Nonnull Document document, @Nullable String url) {
        if (StringUtils.isNullOrEmpty(url)) {
            return null;
        }
        if (url.startsWith("//")) {
            return "https:" + url;
        }
        if (url.startsWith("http://") || url.startsWith("https://")) {
            return url;
        }
        if (url.startsWith("/")) {
            return "https://www.mcmod.cn" + url;
        }
        return document.baseUri().endsWith("/")
                ? document.baseUri() + url
                : document.baseUri() + "/" + url;
    }

    @Nullable
    private static String attr(@Nullable Element element, @Nonnull String name) {
        return element != null ? element.attr(name) : null;
    }

    @Nullable
    private static String firstImageUrl(@Nullable Element element) {
        if (element == null) {
            return null;
        }
        for (String name : new String[]{"src", "data-src", "data-original", "data-lazy-src"}) {
            String url = attr(element, name);
            if (StringUtils.isNullOrEmpty(url) || url.startsWith("data:")) {
                continue;
            }
            if (url.contains("placeholder") || url.endsWith(".svg")) {
                continue;
            }
            return url;
        }
        return null;
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
                .replaceAll("\\n{3,}", "\n\n")
                .trim();
    }

    @Nonnull
    private static String truncate(@Nonnull String text, int maxLength) {
        if (text.length() <= maxLength) {
            return text;
        }
        return text.substring(0, maxLength - 1) + "…";
    }

    // endregion private
}
