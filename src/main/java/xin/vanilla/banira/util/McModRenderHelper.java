package xin.vanilla.banira.util;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.microsoft.playwright.Browser;
import com.microsoft.playwright.Page;
import com.mikuac.shiro.common.utils.MsgUtils;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import lombok.extern.slf4j.Slf4j;
import xin.vanilla.banira.domain.KeyValue;
import xin.vanilla.banira.plugin.mcmod.McModCommentService;
import xin.vanilla.banira.util.html.HtmlScreenshotConfig;
import xin.vanilla.banira.util.html.HtmlScreenshotResult;
import xin.vanilla.banira.util.html.HtmlScreenshotUtils;
import xin.vanilla.banira.util.mcmod.*;

import java.io.File;
import java.util.ArrayList;
import java.util.Base64;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * MCMod 百科卡片图片渲染
 */
@Slf4j
public final class McModRenderHelper {

    private McModRenderHelper() {
    }

    private static final File TEMPLATE_DIR = new File("config/mcmod_plugin");
    private static final String READY_EXPRESSION = "window.__mcModReady === true";
    private static final String CLIP_SELECTOR = ".card";
    private static final Pattern SHORT_NAME_PATTERN = Pattern.compile("\\[([^\\]]+)]");
    private static final String MCMOD_REFERER = "https://www.mcmod.cn/";
    private static final KeyValue<String, String> MCMOD_UA = new KeyValue<>(
            "User-Agent",
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
    );
    private static final KeyValue<String, String> MCMOD_REFERER_HEADER = new KeyValue<>("Referer", MCMOD_REFERER);

    // region 渲染入口

    @Nullable
    public static String wrapCardMessage(@Nullable String link, @Nullable String imageMsg) {
        if (StringUtils.isNullOrEmpty(imageMsg)) {
            return imageMsg;
        }
        if (StringUtils.isNullOrEmpty(link)) {
            return imageMsg;
        }
        return MsgUtils.builder().text(link).text("\n").build() + imageMsg;
    }

    public static boolean shouldPrefixCardLink(@Nonnull String typeName) {
        return "用户".equals(typeName)
                || "作者".equals(typeName)
                || "模组".equals(typeName)
                || "整合包".equals(typeName);
    }

    @Nullable
    public static String renderContent(@Nonnull McModContent content, @Nonnull String typeName) {
        McModPageDetail detail = McModPageParser.fetchDetail(content, typeName);
        if (detail != null && detail.pageFetched()) {
            String imageMsg = renderDetail(detail);
            if (StringUtils.isNotNullOrEmpty(imageMsg)) {
                LOGGER.info("Mcmod detail card rendered: {}", content.getDetailUrl());
                return imageMsg;
            }
            LOGGER.warn("Mcmod detail card render failed, fallback to simple card: {}", content.getDetailUrl());
        } else {
            LOGGER.warn("Mcmod page fetch failed, fallback to simple card: {}", content.getDetailUrl());
        }
        return renderSimpleContent(content, typeName);
    }

    @Nullable
    public static String renderSearchResult(@Nonnull McModSearchResult result, @Nonnull String typeName) {
        if (isUserCenterType(typeName, result.getLink())) {
            McModUserPageDetail userDetail = McModUserPageParser.fetchDetail(result, typeName);
            if (userDetail != null) {
                String imageMsg = renderUserDetail(userDetail);
                if (StringUtils.isNotNullOrEmpty(imageMsg)) {
                    LOGGER.info("Mcmod user detail card rendered: {}", result.getLink());
                    return imageMsg;
                }
                LOGGER.warn("Mcmod user detail card render failed, fallback to simple search card: {}", result.getLink());
            } else {
                LOGGER.warn("Mcmod user detail fetch failed, fallback to simple search card: {}", result.getLink());
            }
            return renderSimpleSearchResult(result, typeName);
        }

        McModPageDetail detail = McModPageParser.fetchDetail(result, typeName);
        if (detail != null && detail.pageFetched()) {
            String imageMsg = renderDetail(detail);
            if (StringUtils.isNotNullOrEmpty(imageMsg)) {
                return imageMsg;
            }
            LOGGER.warn("Mcmod detail card render failed, fallback to simple search card: {}", result.getLink());
        } else {
            LOGGER.debug("Mcmod page fetch failed, fallback to simple search card: {}", result.getLink());
        }
        return renderSimpleSearchResult(result, typeName);
    }

    @Nullable
    public static String renderList(@Nonnull String typeName, @Nonnull String keyword, @Nonnull List<JsonObject> items) {
        JsonObject data = new JsonObject();
        JsonUtils.setString(data, "type", "list");
        JsonUtils.setString(data, "typeName", typeName);
        JsonUtils.setString(data, "keyword", keyword);
        JsonUtils.setInt(data, "total", items.size());
        JsonArray array = new JsonArray();
        items.forEach(array::add);
        data.add("items", array);
        int height = Math.min(680, 180 + items.size() * 72);
        return render("list.html", data, 780, height);
    }

    @Nullable
    public static String renderUserCard(@Nonnull McModUserCardResult card, @Nonnull String userId) {
        McModUserPageDetail detail = McModUserPageParser.fetchDetail(
                McModUtils.getUserCenterUrl(userId), userId, card.getUsername(), "用户");
        if (detail != null) {
            String imageMsg = renderUserDetail(detail);
            if (StringUtils.isNotNullOrEmpty(imageMsg)) {
                return imageMsg;
            }
        }
        return renderSimpleUserCard(card, userId);
    }

    @Nullable
    private static String renderUserDetail(@Nonnull McModUserPageDetail detail) {
        JsonObject data = buildUserDetailData(detail);
        int height = estimateUserDetailHeight(detail);
        return render("user_detail.html", data, 780, height);
    }

    @Nullable
    private static String renderSimpleUserCard(@Nonnull McModUserCardResult card, @Nonnull String userId) {
        JsonObject data = new JsonObject();
        JsonUtils.setString(data, "type", "user");
        JsonUtils.setString(data, "username", StringUtils.nullToEmpty(card.getUsername()));
        JsonUtils.setString(data, "userId", userId);
        JsonUtils.setString(data, "avatar", embedImageUrl(card.getAvatar()));
        JsonUtils.setString(data, "sign", StringUtils.nullToEmpty(card.getSign()));
        JsonUtils.setString(data, "rank", card.getRank() != null ? String.valueOf(card.getRank()) : "未知");
        String online = resolveOnlineText(card.getOnline());
        JsonUtils.setString(data, "online", online);
        if (card.getExp() != null) {
            JsonUtils.setString(data, "expTotal", StringUtils.nullToEmpty(card.getExp().getTotal()));
            JsonUtils.setString(data, "expYet", StringUtils.nullToEmpty(card.getExp().getYet()));
            if (card.getExp().getRate() != null) {
                JsonUtils.setString(data, "expProgress", card.getExp().getRate() + "%");
            }
        }
        return render("user_card.html", data, 760, 400);
    }

    @Nullable
    public static String renderComment(@Nonnull EnumContentType commentType, @Nonnull String containerId,
                                       @Nonnull McModCommentRow comment, @Nullable String containerName) {
        JsonObject data = new JsonObject();
        JsonUtils.setString(data, "type", "comment");
        JsonUtils.setString(data, "commentType", McModCommentService.getCommentTypeName(commentType));
        JsonUtils.setString(data, "containerId", containerId);
        JsonUtils.setString(data, "containerName", StringUtils.isNotNullOrEmpty(containerName) ? containerName : containerId);
        JsonUtils.setString(data, "commentId", comment.getId());
        JsonUtils.setString(data, "floor", StringUtils.nullToEmpty(comment.getFloor()));
        JsonUtils.setString(data, "userName", comment.getUser() != null ? StringUtils.nullToEmpty(comment.getUser().getName()) : "");
        JsonUtils.setString(data, "userId", comment.getUser() != null ? StringUtils.nullToEmpty(comment.getUser().getId()) : "");
        JsonUtils.setString(data, "time", comment.getTime() != null ? StringUtils.nullToEmpty(comment.getTime().getSource()) : "");
        JsonUtils.setString(data, "content", McModCommentService.getPlainCommentText(comment));
        JsonUtils.setBoolean(data, "reply", comment.isReply());
        if (comment.getParentComment() != null && comment.getParentComment().getUser() != null) {
            JsonUtils.setString(data, "parentUser", StringUtils.nullToEmpty(comment.getParentComment().getUser().getName()));
            JsonUtils.setString(data, "parentSummary", McModCommentService.getCommentSummary(
                    McModCommentService.getPlainCommentText(comment.getParentComment())));
        }
        return render("comment.html", data, 760, comment.isReply() ? 480 : 420);
    }

    @Nonnull
    public static JsonObject toListItem(@Nonnull McModContent content) {
        JsonObject item = new JsonObject();
        JsonUtils.setString(item, "title", content.getFormattedName());
        JsonUtils.setString(item, "id", String.valueOf(content.getId()));
        JsonUtils.setString(item, "link", content.getDetailUrl());
        JsonUtils.setString(item, "cover", StringUtils.nullToEmpty(content.getCoverImageUrl()));
        JsonUtils.setString(item, "shortName", StringUtils.nullToEmpty(content.getShortName()));
        JsonUtils.setString(item, "abbrev", buildAbbrev(content.getShortName(), content.getFormattedName()));
        return item;
    }

    @Nonnull
    public static JsonObject toListItem(@Nonnull McModSearchResult result) {
        JsonObject item = new JsonObject();
        JsonUtils.setString(item, "title", StringUtils.nullToEmpty(result.getTitle()));
        JsonUtils.setString(item, "subtitle", StringUtils.nullToEmpty(result.getSubtitle()));
        JsonUtils.setString(item, "link", StringUtils.nullToEmpty(result.getLink()));
        JsonUtils.setString(item, "cover", StringUtils.nullToEmpty(result.getImageUrl()));
        JsonUtils.setString(item, "abbrev", buildAbbrev(null, result.getTitle()));
        return item;
    }

    // endregion 渲染入口

    // region private

    @Nullable
    private static String renderSimpleContent(@Nonnull McModContent content, @Nonnull String typeName) {
        JsonObject data = new JsonObject();
        JsonUtils.setString(data, "mode", "simple");
        JsonUtils.setString(data, "type", "content");
        JsonUtils.setString(data, "typeName", typeName);
        JsonUtils.setString(data, "title", content.getFormattedName());
        JsonUtils.setString(data, "shortName", StringUtils.nullToEmpty(content.getShortName()));
        JsonUtils.setString(data, "abbrev", buildAbbrev(content.getShortName(), content.getFormattedName()));
        JsonUtils.setString(data, "id", String.valueOf(content.getId()));
        JsonUtils.setString(data, "link", content.getDetailUrl());
        JsonUtils.setString(data, "cover", StringUtils.nullToEmpty(content.getCoverImageUrl()));
        return render("content.html", data, 800, 420);
    }

    @Nullable
    private static String renderSimpleSearchResult(@Nonnull McModSearchResult result, @Nonnull String typeName) {
        JsonObject data = new JsonObject();
        JsonUtils.setString(data, "mode", "simple");
        JsonUtils.setString(data, "type", "search");
        JsonUtils.setString(data, "typeName", typeName);
        JsonUtils.setString(data, "subtitle", StringUtils.nullToEmpty(result.getSubtitle()));
        JsonUtils.setString(data, "title", StringUtils.nullToEmpty(result.getTitle()));
        JsonUtils.setString(data, "summary", StringUtils.nullToEmpty(result.getSummary()));
        JsonUtils.setString(data, "link", StringUtils.nullToEmpty(result.getLink()));
        JsonUtils.setString(data, "cover", StringUtils.nullToEmpty(result.getImageUrl()));
        JsonUtils.setString(data, "abbrev", buildAbbrev(null, result.getTitle()));
        if (result.getSnapshotTime() != null) {
            JsonUtils.setString(data, "time", DateUtils.toString(result.getSnapshotTime()));
        }
        return render("content.html", data, 800, 460);
    }

    @Nullable
    private static String renderDetail(@Nonnull McModPageDetail detail) {
        JsonObject data = buildDetailData(detail);
        int height = estimateDetailHeight(detail);
        return render("content_detail.html", data, 780, height);
    }

    @Nonnull
    private static JsonObject buildDetailData(@Nonnull McModPageDetail detail) {
        JsonObject data = new JsonObject();
        JsonUtils.setString(data, "mode", "detail");
        JsonUtils.setString(data, "contentType", detail.contentType() != null
                ? detail.contentType().name().toLowerCase()
                : "mod");
        JsonUtils.setString(data, "descTitle", resolveDescTitle(detail));
        JsonUtils.setString(data, "typeName", StringUtils.nullToEmpty(detail.typeName()));
        JsonUtils.setString(data, "title", StringUtils.nullToEmpty(detail.title()));
        JsonUtils.setString(data, "subTitle", StringUtils.nullToEmpty(detail.subTitle()));
        JsonUtils.setString(data, "shortName", StringUtils.nullToEmpty(detail.shortName()));
        JsonUtils.setString(data, "abbrev", buildAbbrev(detail.shortName(), detail.title()));
        JsonUtils.setString(data, "id", StringUtils.nullToEmpty(detail.id()));
        JsonUtils.setString(data, "link", StringUtils.nullToEmpty(detail.link()));
        JsonUtils.setString(data, "cover", embedImageUrl(detail.coverUrl()));
        JsonUtils.setString(data, "icon", embedImageUrl(detail.getDisplayIcon()));
        JsonUtils.setString(data, "description", StringUtils.nullToEmpty(detail.description()));
        JsonUtils.setString(data, "score", StringUtils.nullToEmpty(detail.score()));
        JsonUtils.setString(data, "scoreComment", StringUtils.nullToEmpty(detail.scoreComment()));
        JsonUtils.setString(data, "yIndex", StringUtils.nullToEmpty(detail.yIndex()));
        JsonUtils.setString(data, "yIndexAvg", StringUtils.nullToEmpty(detail.yIndexAvg()));
        JsonUtils.setString(data, "viewNum", StringUtils.nullToEmpty(detail.viewNum()));
        JsonUtils.setString(data, "fillRate", StringUtils.nullToEmpty(detail.fillRate()));
        JsonUtils.setString(data, "pushNum", StringUtils.nullToEmpty(detail.pushNum()));
        JsonUtils.setString(data, "favNum", StringUtils.nullToEmpty(detail.favNum()));
        JsonUtils.setString(data, "redVote", StringUtils.nullToEmpty(detail.redVote()));
        JsonUtils.setString(data, "blackVote", StringUtils.nullToEmpty(detail.blackVote()));

        JsonArray tags = new JsonArray();
        detail.tags().forEach(tag -> {
            JsonObject item = new JsonObject();
            JsonUtils.setString(item, "t", tag.text());
            JsonUtils.setString(item, "bg", tag.bg());
            JsonUtils.setString(item, "c", tag.color());
            tags.add(item);
        });
        data.add("tags", tags);

        JsonArray categories = new JsonArray();
        detail.categories().forEach(categories::add);
        data.add("categories", categories);

        JsonArray metaItems = new JsonArray();
        detail.metaItems().forEach(prop -> {
            JsonObject item = new JsonObject();
            JsonUtils.setString(item, "l", prop.label());
            JsonUtils.setString(item, "v", prop.value());
            metaItems.add(item);
        });
        data.add("metaItems", metaItems);

        JsonArray props = new JsonArray();
        detail.props().forEach(prop -> {
            JsonObject item = new JsonObject();
            JsonUtils.setString(item, "l", prop.label());
            JsonUtils.setString(item, "v", prop.value());
            props.add(item);
        });
        data.add("props", props);

        JsonArray authorDetails = new JsonArray();
        detail.authorDetails().forEach(author -> {
            JsonObject item = new JsonObject();
            JsonUtils.setString(item, "n", author.name());
            JsonUtils.setString(item, "r", author.role());
            JsonUtils.setString(item, "i", embedImageUrl(author.avatarUrl()));
            JsonUtils.setString(item, "a", buildAbbrev(null, author.name()));
            authorDetails.add(item);
        });
        data.add("authorDetails", authorDetails);

        JsonArray authors = new JsonArray();
        detail.authors().forEach(authors::add);
        data.add("authors", authors);

        JsonArray versions = new JsonArray();
        detail.versions().forEach(version -> {
            JsonObject item = new JsonObject();
            JsonUtils.setString(item, "l", version.loader());
            JsonUtils.setString(item, "v", version.versions());
            JsonArray versionPills = new JsonArray();
            splitVersionPills(version.versions()).forEach(versionPills::add);
            item.add("vv", versionPills);
            versions.add(item);
        });
        data.add("versions", versions);

        JsonArray relatedMods = new JsonArray();
        detail.relatedMods().forEach(relatedMods::add);
        data.add("relatedMods", relatedMods);

        JsonArray links = new JsonArray();
        detail.links().forEach(links::add);
        data.add("links", links);
        return data;
    }

    @Nonnull
    private static JsonObject buildUserDetailData(@Nonnull McModUserPageDetail detail) {
        JsonObject data = new JsonObject();
        JsonUtils.setString(data, "mode", "user");
        JsonUtils.setString(data, "typeName", StringUtils.nullToEmpty(detail.typeName()));
        JsonUtils.setString(data, "username", StringUtils.nullToEmpty(detail.username()));
        JsonUtils.setString(data, "userId", StringUtils.nullToEmpty(detail.userId()));
        JsonUtils.setString(data, "link", StringUtils.nullToEmpty(detail.link()));
        JsonUtils.setString(data, "avatar", embedImageUrl(detail.avatarUrl()));
        JsonUtils.setString(data, "sign", StringUtils.nullToEmpty(detail.sign()));
        JsonUtils.setString(data, "rank", detail.rank() != null ? String.valueOf(detail.rank()) : "");
        JsonUtils.setString(data, "online", resolveOnlineText(detail.online()));
        JsonUtils.setString(data, "userGroup", StringUtils.nullToEmpty(detail.userGroup()));
        JsonUtils.setString(data, "registerTime", StringUtils.nullToEmpty(detail.registerTime()));
        JsonUtils.setString(data, "expTotal", StringUtils.nullToEmpty(detail.expTotal()));
        JsonUtils.setString(data, "expYet", StringUtils.nullToEmpty(detail.expYet()));
        if (detail.expRate() != null) {
            JsonUtils.setString(data, "expProgress", detail.expRate() + "%");
        }

        JsonArray stats = new JsonArray();
        detail.stats().forEach(prop -> {
            JsonObject item = new JsonObject();
            JsonUtils.setString(item, "l", prop.label());
            JsonUtils.setString(item, "v", prop.value());
            stats.add(item);
        });
        data.add("stats", stats);

        JsonArray developerMods = new JsonArray();
        detail.developerMods().forEach(developerMods::add);
        data.add("developerMods", developerMods);

        JsonArray badges = new JsonArray();
        detail.badges().forEach(badge -> {
            if (StringUtils.isNullOrEmpty(badge.getTitle())) {
                return;
            }
            JsonObject item = new JsonObject();
            JsonUtils.setString(item, "t", badge.getTitle());
            badges.add(item);
        });
        data.add("badges", badges);
        return data;
    }

    private static boolean isUserCenterType(@Nonnull String typeName, @Nullable String link) {
        return typeName.contains("用户")
                || (StringUtils.isNotNullOrEmpty(link) && link.contains("center.mcmod.cn"));
    }

    @Nonnull
    private static String resolveOnlineText(@Nullable Integer online) {
        if (online == null) {
            return "未知";
        }
        return switch (online) {
            case 1 -> "在线";
            case 0 -> "离线";
            case -1 -> "隐身";
            default -> "未知";
        };
    }

    private static int estimateUserDetailHeight(@Nonnull McModUserPageDetail detail) {
        int height = 320;
        if (StringUtils.isNotNullOrEmpty(detail.sign())) {
            height += 56;
        }
        height += (int) Math.ceil(Math.min(detail.stats().size(), 8) / 2.0) * 28;
        height += detail.developerMods().isEmpty() ? 0
                : (int) Math.ceil(Math.min(detail.developerMods().size() + 1, 21) / 4.0) * 28;
        height += detail.badges().isEmpty() ? 0 : 34;
        return Math.min(1100, Math.max(460, height));
    }

    @Nonnull
    private static String resolveDescTitle(@Nonnull McModPageDetail detail) {
        if (detail.contentType() == EnumContentType.AUTHOR) {
            return "作者简介";
        }
        if (detail.contentType() == EnumContentType.MODPACK) {
            return "整合包介绍";
        }
        return "模组介绍";
    }

    private static int estimateDetailHeight(@Nonnull McModPageDetail detail) {
        int height = 300;
        if (StringUtils.isNotNullOrEmpty(detail.description())) {
            height += 96;
        }
        height += (int) Math.ceil(detail.metaItems().size() / 2.0) * 28;
        height += detail.versions().size() * 42;
        height += detail.tags().isEmpty() ? 0 : 28;
        height += detail.authorDetails().isEmpty() ? 0 : (int) Math.ceil(detail.authorDetails().size() / 3.0) * 52;
        height += detail.relatedMods().isEmpty() ? 0 : (int) Math.ceil(Math.min(detail.relatedMods().size() + 1, 21) / 4.0) * 28;
        height += StringUtils.isNotNullOrEmpty(detail.redVote()) || StringUtils.isNotNullOrEmpty(detail.blackVote()) ? 28 : 0;
        return Math.min(1100, Math.max(460, height));
    }

    @Nonnull
    private static List<String> splitVersionPills(@Nullable String versions) {
        if (StringUtils.isNullOrEmpty(versions)) {
            return List.of();
        }
        LinkedHashSet<String> tokens = new LinkedHashSet<>();
        Matcher matcher = Pattern.compile("\\d+\\.\\d+(?:\\.\\d+)?(?:-[\\w.]+)?").matcher(versions);
        while (matcher.find()) {
            tokens.add(matcher.group());
        }
        if (!tokens.isEmpty()) {
            return new ArrayList<>(tokens);
        }
        for (String part : versions.split("[,，;；\\s]+")) {
            part = part.trim();
            if (StringUtils.isNotNullOrEmpty(part)) {
                tokens.add(part);
            }
        }
        return new ArrayList<>(tokens);
    }

    @Nullable
    private static String embedImageUrl(@Nullable String url) {
        if (StringUtils.isNullOrEmpty(url)) {
            return "";
        }
        if (url.startsWith("data:")) {
            return url;
        }
        byte[] bytes = HttpUtils.downloadBytes(url, MCMOD_UA, MCMOD_REFERER_HEADER);
        if (bytes == null || bytes.length == 0) {
            return url;
        }
        String mime = guessImageMime(url, bytes);
        return "data:" + mime + ";base64," + Base64.getEncoder().encodeToString(bytes);
    }

    @Nonnull
    private static String guessImageMime(@Nonnull String url, @Nonnull byte[] bytes) {
        if (bytes.length >= 8
                && bytes[0] == (byte) 0x89 && bytes[1] == 0x50 && bytes[2] == 0x4E && bytes[3] == 0x47) {
            return "image/png";
        }
        if (bytes.length >= 3
                && bytes[0] == (byte) 0xFF && bytes[1] == (byte) 0xD8 && bytes[2] == (byte) 0xFF) {
            return "image/jpeg";
        }
        if (bytes.length >= 6
                && bytes[0] == 'G' && bytes[1] == 'I' && bytes[2] == 'F') {
            return "image/gif";
        }
        String lower = url.toLowerCase();
        if (lower.contains(".png")) {
            return "image/png";
        }
        if (lower.contains(".webp")) {
            return "image/webp";
        }
        if (lower.contains(".gif")) {
            return "image/gif";
        }
        return "image/jpeg";
    }

    @Nonnull
    private static String buildAbbrev(@Nullable String shortName, @Nullable String title) {
        if (StringUtils.isNotNullOrEmpty(shortName)) {
            return shortName.length() > 6 ? shortName.substring(0, 6) : shortName;
        }
        if (StringUtils.isNotNullOrEmpty(title)) {
            Matcher matcher = SHORT_NAME_PATTERN.matcher(title);
            if (matcher.find()) {
                String extracted = matcher.group(1).trim();
                return extracted.length() > 6 ? extracted.substring(0, 6) : extracted;
            }
            String clean = title.replaceAll("^\\[[^\\]]+]\\s*", "").trim();
            if (StringUtils.isNotNullOrEmpty(clean)) {
                return clean.substring(0, Math.min(2, clean.length()));
            }
        }
        return "MC";
    }

    @Nullable
    private static String render(@Nonnull String templateName, @Nonnull JsonObject data, int width, int height) {
        try {
            ensureTemplate(templateName);
        } catch (Exception e) {
            LOGGER.error("Failed to copy mcmod template", e);
            return null;
        }

        File renderFile = null;
        try {
            renderFile = buildRenderFile(templateName, data);
            HtmlScreenshotResult render = HtmlScreenshotUtils.render(
                    new HtmlScreenshotConfig(renderFile)
                            .setContextOptions(new Browser.NewContextOptions()
                                    .setViewportSize(width, height)
                                    .setExtraHTTPHeaders(java.util.Map.of("Referer", MCMOD_REFERER)))
                            .setScreenshotOptions(new Page.ScreenshotOptions().setFullPage(false))
                            .setClipSelector(CLIP_SELECTOR)
                            .setReadyExpression(READY_EXPRESSION)
                            .setReadyTimeout(12000)
            );
            if (render.isEmpty()) {
                return null;
            }
            return MsgUtils.builder().img(render.getByte()).build();
        } catch (Exception e) {
            LOGGER.error("Failed to render mcmod html: {}", templateName, e);
            return null;
        } finally {
            HtmlScreenshotConfig.deleteQuietly(renderFile);
        }
    }

    @Nonnull
    private static File buildRenderFile(@Nonnull String templateName, @Nonnull JsonObject data) throws Exception {
        ensureTemplate(templateName);
        File templateFile = new File(TEMPLATE_DIR, templateName);
        return HtmlScreenshotConfig.buildInlineConfigRenderFile(
                templateFile, JsonUtils.PRETTY_GSON.toJson(data));
    }

    private static void ensureTemplate(@Nonnull String templateName) throws Exception {
        ResourceCopyUtils.copyResources("template/mcmod_plugin", TEMPLATE_DIR.getPath());
    }

    // endregion private
}
