package xin.vanilla.banira.util;

import com.google.gson.JsonObject;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import lombok.extern.slf4j.Slf4j;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.nodes.TextNode;
import org.jsoup.select.Elements;
import tools.jackson.core.type.TypeReference;
import xin.vanilla.banira.config.entity.extended.McModCookieConfig;
import xin.vanilla.banira.config.entity.group.McModGroupConfig;
import xin.vanilla.banira.domain.KeyValue;
import xin.vanilla.banira.util.http.HttpRequestBuilder;
import xin.vanilla.banira.util.http.HttpResponse;
import xin.vanilla.banira.util.mcmod.*;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * MCMod百科工具类
 */
@Slf4j
@SuppressWarnings("unused")
public final class McModUtils {

    private McModUtils() {
    }

    private static final Map<Long, ReentrantLock> groupLocks = new LinkedHashMap<>();
    private static final ReentrantLock locksMapLock = new ReentrantLock();

    // region private

    /**
     * 获取指定群号的锁
     */
    private static ReentrantLock getGroupLock(Long groupId) {
        locksMapLock.lock();
        try {
            return groupLocks.computeIfAbsent(groupId, k -> new ReentrantLock());
        } finally {
            locksMapLock.unlock();
        }
    }

    /**
     * 获取 Cookie，支持两种方式
     *
     * @param groupId 群号
     */
    private static @Nullable String getCookie(Long groupId) {
        ReentrantLock lock = getGroupLock(groupId);
        lock.lock();
        try {
            McModGroupConfig otherConfig = BaniraUtils.getGroupConfigForEdit(McModGroupConfig.class, groupId);
            if (otherConfig == null) {
                LOGGER.error("Failed to get config, groupId: {}", groupId);
                return null;
            }

            McModCookieConfig cookieConfig = otherConfig.mcModCookieConfig();
            if (cookieConfig == null) {
                cookieConfig = new McModCookieConfig();
                otherConfig.mcModCookieConfig(cookieConfig);
            }

            // 若配置中有 Cookie 且未过期，直接使用
            if (StringUtils.isNotNullOrEmpty(cookieConfig.cookie()) && !cookieConfig.isExpired()) {
                LOGGER.debug("Using cookie from config, groupId: {}", groupId);
                // 如果用户ID未缓存，尝试获取
                if (StringUtils.isNullOrEmpty(cookieConfig.userId())) {
                    String userId = getLoginUserId(cookieConfig.cookie());
                    if (userId != null) {
                        cookieConfig.userId(userId);
                        BaniraUtils.saveGroupConfig();
                    }
                }
                return cookieConfig.cookie();
            }

            // 通过用户名密码登录
            String username = cookieConfig.username();
            String password = cookieConfig.password();

            if (StringUtils.isNullOrEmpty(username) || StringUtils.isNullOrEmpty(password)) {
                LOGGER.error("Username or password not set in config, cannot login, groupId: {}", groupId);
                return null;
            }

            LOGGER.info("Cookie expired or not found, logging in with username and password, groupId: {}", groupId);
            KeyValue<String, String> cookie = login(username, password);

            if (cookie != null && cookie.getKey() != null) {
                // 登录成功，写回配置（默认25天过期）
                cookieConfig.setCookieWithExpire(cookie.getKey());
                // 获取并缓存当前登录用户ID
                String userId = getLoginUserId(cookie.getKey());
                if (userId != null) {
                    cookieConfig.userId(userId);
                }
                BaniraUtils.saveGroupConfig();
                LOGGER.info("Login successful, cookie saved to config, groupId: {}", groupId);
                return cookie.getKey();
            } else {
                LOGGER.error("Login failed, cannot get cookie, groupId: {}", groupId);
                clearCookieCache(groupId);
                return null;
            }
        } finally {
            lock.unlock();
        }
    }

    /**
     * 登录并获取 Cookie
     *
     * @param username 用户名
     * @param password 密码
     * @return KeyValue<cookies, state>
     */
    private static @Nullable KeyValue<String, String> login(String username, String password) {
        try {
            JsonObject loginData = new JsonObject();
            loginData.addProperty("username", username);
            loginData.addProperty("password", password);
            loginData.addProperty("remember", 1);
            loginData.addProperty("captcha", "");

            Map<String, String> formData = new LinkedHashMap<>();
            formData.put("data", JsonUtils.toJsonString(loginData));

            HttpResponse response = HttpUtils.post("https://www.mcmod.cn/action/doLogin/")
                    .formBody(formData)
                    .header("Referer", "https://www.mcmod.cn")
                    .execute();

            if (response == null) {
                LOGGER.warn("Login failed, response is null");
                return null;
            } else if (response.statusCode() != 200) {
                LOGGER.warn("Login failed, status code: {}", response.statusCode());
                return new KeyValue<>(null, String.valueOf(response.statusCode()));
            }

            String responseBody = response.getBodyAsString();
            if (responseBody == null) {
                LOGGER.warn("Login failed, response body is null");
                return null;
            }

            // 检查登录响应
            JsonObject json = JsonUtils.parseJsonObject(responseBody);
            if (json == null) {
                LOGGER.warn("Login failed, cannot parse response: {}", responseBody);
                return null;
            }

            // 检查 state 字段
            if (!json.has("state")) {
                LOGGER.warn("Login failed, response missing state field: {}", responseBody);
                return null;
            }

            // 提取 state 值
            String state = null;
            if (json.get("state").isJsonPrimitive()) {
                state = json.get("state").getAsJsonPrimitive().getAsString();
            }

            if (state == null) {
                LOGGER.warn("Login failed, cannot extract state from response: {}", responseBody);
                return null;
            }
            // 从响应头中获取 Set-Cookie
            List<String> setCookies = response.getHeaders("Set-Cookie");

            // 检查是否登录成功（state == "0"）
            boolean loginSuccess = "0".equals(state);
            if (!loginSuccess) {
                LOGGER.warn("Login failed, state: {}, cookies: {}", state, setCookies);
                // 即使失败也返回 state，但 cookie 为 null
                return new KeyValue<>(null, state);
            }

            if (setCookies == null || setCookies.isEmpty()) {
                LOGGER.warn("Login successful but no Set-Cookie header found");
                return new KeyValue<>(null, state);
            }

            // 合并所有 Cookie
            StringBuilder cookieBuilder = new StringBuilder();
            for (String setCookie : setCookies) {
                if (setCookie != null && !setCookie.isEmpty()) {
                    String[] parts = setCookie.split(";");
                    if (parts.length > 0) {
                        String[] kv = parts[0].split("=", 2);
                        if (kv.length == 2) {
                            if (!cookieBuilder.isEmpty()) {
                                cookieBuilder.append("; ");
                            }
                            cookieBuilder.append(kv[0].trim()).append("=").append(kv[1].trim());
                        }
                    }
                }
            }

            String cookie = !cookieBuilder.isEmpty() ? cookieBuilder.toString() : null;
            if (cookie == null) {
                LOGGER.warn("Login successful but cookie is empty");
                return new KeyValue<>(null, state);
            }

            return new KeyValue<>(cookie, state);
        } catch (Exception e) {
            LOGGER.error("Login exception", e);
            return null;
        }
    }

    private static final Pattern USER_CENTER_URL_PATTERN = Pattern.compile("//center\\.mcmod\\.cn/(?<userId>\\d+)/");
    private static final Pattern YXD_TOKEN_PATTERN = Pattern.compile("yxd_token=([a-f0-9]+)");
    private static volatile String cachedSearchYxdToken;

    /**
     * 解析搜索HTML结果
     *
     * @param html HTML内容
     * @param type 搜索类型
     * @return 搜索结果列表
     */
    private static List<McModContent> parseSearchHtml(String html, EnumSearchType type) {
        if (StringUtils.isNullOrEmpty(html)) {
            return List.of();
        }
        List<McModContent> results = new ArrayList<>();

        if (type == EnumSearchType.AUTHOR) {
            // 作者：<tr data-id=\"33928\"><td><b>TsukiMaaii</b> - <i class=\"text-muted\">TinyTsuki / 辉小月 / 辉月马以 / 银月马以</i></td></tr>
            Pattern rowPattern = Pattern.compile("<tr[^>]*data-id=\"(\\d+)\"[^>]*>\\s*<td>(.*?)</td>",
                    Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
            Matcher matcher = rowPattern.matcher(html);
            while (matcher.find()) {
                long authorId = Long.parseLong(matcher.group(1));
                String tdContent = matcher.group(2);

                // 提取 <b> 标签中的主要名称
                String mainName = null;
                Pattern boldPattern = Pattern.compile("<b>([^<]+)</b>");
                Matcher boldMatcher = boldPattern.matcher(tdContent);
                if (boldMatcher.find()) {
                    mainName = boldMatcher.group(1).trim();
                }

                // 提取 <i> 标签中的别名列表（作为次要名称）
                String secondaryName = null;
                Pattern italicPattern = Pattern.compile("<i[^>]*>([^<]+)</i>");
                Matcher italicMatcher = italicPattern.matcher(tdContent);
                if (italicMatcher.find()) {
                    secondaryName = italicMatcher.group(1).trim();
                }

                if (mainName != null) {
                    results.add(new McModContent(type.toContentType(), authorId, null, mainName, secondaryName));
                }
            }
        } else {
            // Mod/整合包：<tr data-id="18210"><td>ID:18210 [NF] 水仙辞 (Narcissus Farewell)</td></tr>
            Pattern rowPattern = Pattern.compile("<tr[^>]*data-id=\"(\\d+)\"[^>]*>\\s*<td>([^<]+)</td>",
                    Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
            Matcher matcher = rowPattern.matcher(html);
            while (matcher.find()) {
                long modId = Long.parseLong(matcher.group(1));
                String text = matcher.group(2);
                // 移除前缀 ID:xxxx
                String afterId = text.replaceFirst("^ID:\\d+\\s*", "").trim();

                String shortName = null;
                if (afterId.startsWith("[")) {
                    int end = afterId.indexOf(']');
                    if (end > 1) {
                        shortName = afterId.substring(1, end).trim();
                        afterId = afterId.substring(end + 1).trim();
                    }
                }

                String mainName = afterId;
                String secondaryName = null;
                int parenStart = afterId.lastIndexOf('(');
                if (parenStart >= 0 && afterId.endsWith(")")) {
                    secondaryName = afterId.substring(parenStart + 1, afterId.length() - 1).trim();
                    mainName = afterId.substring(0, parenStart).trim();
                }

                results.add(new McModContent(type.toContentType(), modId, shortName, mainName, secondaryName));
            }
        }
        return results;
    }

    /**
     * 写入 multipart 字段
     */
    private static void writeMultipartField(ByteArrayOutputStream outputStream, String boundary, String fieldName,
                                            byte[] fieldValue, String fileName, String contentType) throws IOException {
        outputStream.write(("--" + boundary + "\r\n").getBytes(StandardCharsets.UTF_8));
        if (fileName != null) {
            // 文件字段
            String header = String.format("Content-Disposition: form-data; name=\"%s\"; filename=\"%s\"\r\n",
                    fieldName, fileName);
            outputStream.write(header.getBytes(StandardCharsets.UTF_8));
            if (contentType != null) {
                outputStream.write(("Content-Type: " + contentType + "\r\n").getBytes(StandardCharsets.UTF_8));
            }
        } else {
            // 普通字段
            String header = String.format("Content-Disposition: form-data; name=\"%s\"\r\n", fieldName);
            outputStream.write(header.getBytes(StandardCharsets.UTF_8));
        }
        outputStream.write("\r\n".getBytes(StandardCharsets.UTF_8));
        outputStream.write(fieldValue);
        outputStream.write("\r\n".getBytes(StandardCharsets.UTF_8));
    }

    /**
     * 解析首页分类 HTML
     */
    private static McModIndexCategoryResult parseIndexCategoryHtml(String html) {
        if (StringUtils.isNullOrEmpty(html)) {
            return null;
        }

        try {
            Document doc = Jsoup.parse(html);
            Element classBlock = doc.selectFirst(".class_block");
            if (classBlock == null) {
                LOGGER.warn("Failed to find .class_block element");
                return null;
            }

            Element leftElement = classBlock.selectFirst(".left");
            Element rightElement = classBlock.selectFirst(".right");

            if (leftElement == null || rightElement == null) {
                LOGGER.warn("Failed to separate left and right parts of HTML");
                return null;
            }

            McModIndexCategoryLeft left = parseIndexCategoryLeftHtml(leftElement);
            McModIndexCategoryRight right = parseIndexCategoryRightHtml(rightElement);

            return new McModIndexCategoryResult(left, right);
        } catch (Exception e) {
            LOGGER.error("Exception parsing index category HTML", e);
            return null;
        }
    }

    /**
     * 解析左侧 HTML
     */
    private static McModIndexCategoryLeft parseIndexCategoryLeftHtml(Element leftElement) {
        // 提取标题和链接
        String categoryUrl = null;
        String title = null;
        Element titleElement = leftElement.selectFirst(".title a");
        if (titleElement != null) {
            categoryUrl = titleElement.attr("href");
            title = titleElement.text().trim();
        }

        // 提取描述
        String description = null;
        Element descElement = leftElement.selectFirst(".text span.i, .text span.t");
        if (descElement != null) {
            description = descElement.text().trim();
        }

        // 提取模组卡片列表
        List<McModIndexCategoryModFrame> modFrames = new ArrayList<>();
        Elements frameElements = leftElement.select(".list .frame");
        for (Element frameElement : frameElements) {
            McModIndexCategoryModFrame frame = parseIndexCategoryModFrameHtml(frameElement);
            if (frame != null) {
                modFrames.add(frame);
            }
        }

        return new McModIndexCategoryLeft(title, description, categoryUrl, modFrames);
    }

    /**
     * 解析模组卡片 HTML
     */
    private static McModIndexCategoryModFrame parseIndexCategoryModFrameHtml(Element frameElement) {
        try {
            Element blockElement = frameElement.selectFirst(".block");
            if (blockElement == null) {
                return null;
            }

            // 提取模组ID和链接
            long modId = 0;
            Element modLink = blockElement.selectFirst("a[href^=/class/]");
            if (modLink != null) {
                String href = modLink.attr("href");
                Pattern idPattern = Pattern.compile("/class/(\\d+)\\.html");
                Matcher idMatcher = idPattern.matcher(href);
                if (idMatcher.find()) {
                    modId = Long.parseLong(idMatcher.group(1));
                }
            }

            // 提取模组名称
            String shortName = null;
            String mainName = null;
            String secondaryName = null;
            Element nameElement = blockElement.selectFirst(".name a");
            if (nameElement != null) {
                String titleAttr = nameElement.attr("title");
                String nameText = nameElement.text().trim();
                // 处理 <br/> 标签
                nameText = nameText.replace("\n", " ").replaceAll("\\s+", " ").trim();

                // 优先使用 title 属性
                String nameToParse = StringUtils.isNotNullOrEmpty(titleAttr) ? titleAttr : nameText;

                if (nameToParse.startsWith("[")) {
                    int end = nameToParse.indexOf(']');
                    if (end > 1) {
                        shortName = nameToParse.substring(1, end).trim();
                        nameToParse = nameToParse.substring(end + 1).trim();
                    }
                }

                mainName = nameToParse;
                int parenStart = nameToParse.lastIndexOf('(');
                if (parenStart >= 0 && nameToParse.endsWith(")")) {
                    secondaryName = nameToParse.substring(parenStart + 1, nameToParse.length() - 1).trim();
                    mainName = nameToParse.substring(0, parenStart).trim();
                }
            }

            // 提取封面图片
            String coverImageUrl = null;
            Element coverImg = blockElement.selectFirst("img[type-original]");
            if (coverImg != null) {
                coverImageUrl = coverImg.attr("data-original");
                if (StringUtils.isNullOrEmpty(coverImageUrl)) {
                    coverImageUrl = coverImg.attr("src");
                }
                if (StringUtils.isNotNullOrEmpty(coverImageUrl)) {
                    coverImageUrl = fixUrl(coverImageUrl);
                }
            }

            // 提取浏览数、推荐数、收藏数
            String viewCount = null;
            String recommendCount = null;
            String favoriteCount = null;
            Elements infoElements = blockElement.select(".info .info > div");
            for (Element infoElement : infoElements) {
                String title = infoElement.attr("title");
                String value = infoElement.text().trim();
                if (title.contains("浏览")) {
                    viewCount = value;
                } else if (title.contains("推荐")) {
                    recommendCount = value;
                } else if (title.contains("收藏")) {
                    favoriteCount = value;
                }
            }

            // 提取物品列表
            List<McModIndexCategoryItem> items = new ArrayList<>();
            Element itemsElement = frameElement.selectFirst(".items");
            if (itemsElement != null) {
                Elements itemElements = itemsElement.select("li[iid]");
                for (Element itemElement : itemElements) {
                    try {
                        String iidStr = itemElement.attr("iid");
                        if (StringUtils.isNullOrEmpty(iidStr)) {
                            continue;
                        }
                        long itemId = Long.parseLong(iidStr);

                        Element itemLink = itemElement.selectFirst("a");
                        if (itemLink == null) {
                            continue;
                        }
                        String itemUrl = fixUrl(itemLink.attr("href"));

                        Element itemImg = itemElement.selectFirst("img");
                        String itemName = itemImg != null ? itemImg.attr("alt") : null;
                        String iconUrl = itemImg != null ? fixUrl(itemImg.attr("src")) : null;

                        if (itemId > 0 && StringUtils.isNotNullOrEmpty(itemName) && StringUtils.isNotNullOrEmpty(itemUrl)) {
                            items.add(new McModIndexCategoryItem(itemId, itemName, iconUrl, itemUrl));
                        }
                    } catch (Exception e) {
                        LOGGER.warn("Exception parsing item: {}", e.getMessage());
                    }
                }
            }

            return new McModIndexCategoryModFrame(modId, shortName, mainName, secondaryName, coverImageUrl,
                    viewCount, recommendCount, favoriteCount, items);
        } catch (Exception e) {
            LOGGER.error("Exception parsing mod card HTML", e);
            return null;
        }
    }

    /**
     * 解析右侧排行榜 HTML
     */
    private static McModIndexCategoryRight parseIndexCategoryRightHtml(Element rightElement) {
        List<McModIndexCategoryRankItem> dayRank = parseIndexCategoryRankList(rightElement, "day");
        List<McModIndexCategoryRankItem> weekRank = parseIndexCategoryRankList(rightElement, "week");
        List<McModIndexCategoryRankItem> monthRank = parseIndexCategoryRankList(rightElement, "moon");

        return new McModIndexCategoryRight(dayRank, weekRank, monthRank);
    }

    /**
     * 解析排行榜列表
     */
    private static List<McModIndexCategoryRankItem> parseIndexCategoryRankList(Element rightElement, String rankType) {
        List<McModIndexCategoryRankItem> rankList = new ArrayList<>();
        Element rankListElement = rightElement.selectFirst("ul#" + rankType);
        if (rankListElement == null) {
            return rankList;
        }

        // 匹配所有带图片的排行榜项（class="no1"）
        Elements rankItems = rankListElement.select("li.no1");
        for (Element rankItem : rankItems) {
            try {
                // 提取排名
                int rank = 0;
                Element rankIcon = rankItem.selectFirst("i.n1, i.n2, i.n3, i.n4, i.n5, i.n6, i.n7, i.n8, i.n9, i.n10, i.n11, i.n12");
                if (rankIcon != null) {
                    String rankText = rankIcon.text().trim();
                    if (StringUtils.isNotNullOrEmpty(rankText)) {
                        try {
                            rank = Integer.parseInt(rankText);
                        } catch (NumberFormatException e) {
                            // 尝试从 class 中提取
                            String className = rankIcon.className();
                            Pattern rankPattern = Pattern.compile("n(\\d+)");
                            Matcher rankMatcher = rankPattern.matcher(className);
                            if (rankMatcher.find()) {
                                rank = Integer.parseInt(rankMatcher.group(1));
                            }
                        }
                    }
                }

                // 提取模组ID
                long modId = 0;
                Element modLink = rankItem.selectFirst("a[href^=/class/]");
                if (modLink != null) {
                    String href = modLink.attr("href");
                    Pattern idPattern = Pattern.compile("/class/(\\d+)\\.html");
                    Matcher idMatcher = idPattern.matcher(href);
                    if (idMatcher.find()) {
                        modId = Long.parseLong(idMatcher.group(1));
                    }
                }

                // 提取封面图片
                String coverImageUrl = null;
                Element coverImg = rankItem.selectFirst("img");
                if (coverImg != null) {
                    coverImageUrl = coverImg.attr("data-original");
                    if (StringUtils.isNullOrEmpty(coverImageUrl)) {
                        coverImageUrl = coverImg.attr("src");
                    }
                    if (StringUtils.isNotNullOrEmpty(coverImageUrl)) {
                        coverImageUrl = fixUrl(coverImageUrl);
                    }
                }

                // 提取模组名称
                String shortName = null;
                String mainName = null;
                String secondaryName = null;
                if (modLink != null) {
                    String titleAttr = modLink.attr("title");
                    String nameText = modLink.text().trim();
                    // 处理 <br/> 标签
                    nameText = nameText.replace("\n", " ").replaceAll("\\s+", " ").trim();

                    // 优先使用 title 属性
                    String nameToParse = StringUtils.isNotNullOrEmpty(titleAttr) ? titleAttr : nameText;

                    if (nameToParse.startsWith("[")) {
                        int end = nameToParse.indexOf(']');
                        if (end > 1) {
                            shortName = nameToParse.substring(1, end).trim();
                            nameToParse = nameToParse.substring(end + 1).trim();
                        }
                    }

                    mainName = nameToParse;
                    int parenStart = nameToParse.lastIndexOf('(');
                    if (parenStart >= 0 && nameToParse.endsWith(")")) {
                        secondaryName = nameToParse.substring(parenStart + 1, nameToParse.length() - 1).trim();
                        mainName = nameToParse.substring(0, parenStart).trim();
                    }
                }

                // 提取指数值
                String indexValue = null;
                Element rankSpan = rankItem.selectFirst("span.rank");
                if (rankSpan != null) {
                    indexValue = rankSpan.text().trim();
                    // 移除"今日指数："等前缀
                    if (indexValue.contains("指数：")) {
                        indexValue = indexValue.substring(indexValue.indexOf("指数：") + 3).trim();
                    }
                }

                if (modId > 0 && mainName != null) {
                    rankList.add(new McModIndexCategoryRankItem(rank, modId, shortName, mainName, secondaryName, coverImageUrl, indexValue));
                }
            } catch (Exception e) {
                LOGGER.warn("Exception parsing rank item: {}", e.getMessage());
            }
        }

        return rankList;
    }

    /**
     * 解析随机模组HTML
     */
    private static List<McModContent> parseRandomModsHtml(String html) {
        List<McModContent> items = new ArrayList<>();
        if (StringUtils.isNullOrEmpty(html)) {
            return items;
        }

        try {
            Document doc = Jsoup.parse(html);
            Elements blockElements = doc.select(".block");
            for (Element blockElement : blockElements) {
                try {
                    McModContent item = parseRandomModBlock(blockElement);
                    if (item != null) {
                        items.add(item);
                    }
                } catch (Exception e) {
                    LOGGER.warn("Exception parsing random mod block: {}", e.getMessage());
                }
            }
        } catch (Exception e) {
            LOGGER.warn("Exception parsing random mods HTML: {}", e.getMessage());
        }

        return items;
    }

    /**
     * 解析单个随机模组块
     */
    private static @Nullable McModContent parseRandomModBlock(Element blockElement) {
        try {
            // 提取链接和ID
            Element linkElement = blockElement.selectFirst("a[href]");
            if (linkElement == null) {
                return null;
            }

            String href = linkElement.attr("href");
            EnumContentType type;
            long id = 0;

            // 判断类型并提取ID
            if (href.startsWith("/modpack/")) {
                type = EnumContentType.MODPACK;
                Pattern idPattern = Pattern.compile("/modpack/(\\d+)\\.html");
                Matcher idMatcher = idPattern.matcher(href);
                if (idMatcher.find()) {
                    id = Long.parseLong(idMatcher.group(1));
                }
            } else if (href.startsWith("/class/")) {
                type = EnumContentType.MOD;
                Pattern idPattern = Pattern.compile("/class/(\\d+)\\.html");
                Matcher idMatcher = idPattern.matcher(href);
                if (idMatcher.find()) {
                    id = Long.parseLong(idMatcher.group(1));
                }
            } else {
                return null;
            }

            if (id == 0) {
                return null;
            }

            // 提取封面图片
            String coverImageUrl = null;
            Element imgElement = blockElement.selectFirst("img");
            if (imgElement != null) {
                coverImageUrl = imgElement.attr("data-original");
                if (StringUtils.isNullOrEmpty(coverImageUrl)) {
                    coverImageUrl = imgElement.attr("src");
                }
                if (StringUtils.isNotNullOrEmpty(coverImageUrl)) {
                    coverImageUrl = fixUrl(coverImageUrl);
                }
            }

            // 提取主名称（第一个 <div class="name"> 中的 <a> 标签）
            String mainName = null;
            Elements nameElements = blockElement.select(".info .name");
            if (!nameElements.isEmpty()) {
                Element firstNameElement = nameElements.getFirst();
                Element nameLink = firstNameElement.selectFirst("a");
                if (nameLink != null) {
                    String titleAttr = nameLink.attr("title");
                    String content = nameLink.text().trim();
                    mainName = StringUtils.isNotNullOrEmpty(titleAttr) ? titleAttr : content;
                }
            }

            // 提取次要名称（第二个 <div class="name"> 中的 <div class="enname"> 内的 <a> 标签文本）
            String secondaryName = null;
            if (nameElements.size() > 1) {
                Element secondNameElement = nameElements.get(1);
                Element ennameElement = secondNameElement.selectFirst(".enname a");
                if (ennameElement != null) {
                    String enname = ennameElement.text().trim();
                    // 移除括号
                    if (enname.startsWith("(") && enname.endsWith(")")) {
                        enname = enname.substring(1, enname.length() - 1).trim();
                    }
                    if (StringUtils.isNotNullOrEmpty(enname)) {
                        secondaryName = enname;
                    }
                }
            }

            // 构建详情页URL
            String detailUrl = fixUrl(href);
            if (detailUrl == null) {
                detailUrl = href.startsWith("/") ? "https://www.mcmod.cn" + href : href;
            }

            return new McModContent(type, id, null, mainName, secondaryName, coverImageUrl, detailUrl);
        } catch (Exception e) {
            LOGGER.warn("Exception parsing random mod block: {}", e.getMessage());
            return null;
        }
    }

    // endregion private

    public static @Nonnull String getUrl(EnumContentType type, String containerId) {
        if (type == null || containerId == null) {
            return "";
        }
        return switch (type) {
            case MOD -> getModUrl(containerId);
            case MODPACK -> getModpackUrl(containerId);
            case AUTHOR -> getAuthorUrl(containerId);
            case USER_CENTER -> getUserCenterUrl(containerId);
        };
    }

    public static String getModUrl(String modId) {
        return String.format("https://www.mcmod.cn/class/%s.html", modId);
    }

    public static String getModpackUrl(String modpackId) {
        return String.format("https://www.mcmod.cn/modpack/%s.html", modpackId);
    }

    public static String getAuthorUrl(String authorId) {
        return String.format("https://www.mcmod.cn/author/%s.html", authorId);
    }

    public static String getUserCenterUrl(String uid) {
        return String.format("https://center.mcmod.cn/%s/", uid);
    }


    public static String getLoginUserId(String cookie) {
        String redirectedUrl = HttpUtils.getRedirectedUrl("https://www.mcmod.cn/login/", new KeyValue<>("Referer", "https://www.mcmod.cn"), new KeyValue<>("Cookie", cookie));
        Matcher matcher = USER_CENTER_URL_PATTERN.matcher(redirectedUrl);
        if (matcher.find()) {
            return matcher.group("userId");
        }
        return null;
    }

    /**
     * 获取当前登录用户ID
     *
     * @param groupId 群号
     */
    public static @Nullable String getLoginUserId(@Nullable Long groupId) {
        ReentrantLock lock = getGroupLock(groupId);
        lock.lock();
        try {
            McModGroupConfig otherConfig = BaniraUtils.getGroupConfigOrGlobal(McModGroupConfig.class, groupId);
            if (otherConfig == null || otherConfig.mcModCookieConfig() == null) {
                return null;
            }

            McModCookieConfig cookieConfig = otherConfig.mcModCookieConfig();
            if (StringUtils.isNotNullOrEmpty(cookieConfig.userId())
                    && StringUtils.isNotNullOrEmpty(cookieConfig.cookie())
                    && !cookieConfig.isExpired()) {
                return cookieConfig.userId();
            }

            // 尝试从cookie获取
            String cookie = cookieConfig.cookie();
            if (StringUtils.isNotNullOrEmpty(cookie) && !cookieConfig.isExpired()) {
                String userId = getLoginUserId(cookie);
                if (userId != null) {
                    cookieConfig.userId(userId);
                    BaniraUtils.saveGroupConfig();
                    return userId;
                }
            }

            return null;
        } finally {
            lock.unlock();
        }
    }

    /**
     * 搜索
     *
     * @param type 搜索类型
     * @param key  搜索关键词
     * @return 搜索结果列表
     */
    public static @Nonnull List<McModContent> search(EnumSearchType type, String key) {
        try {
            JsonObject requestData = new JsonObject();
            requestData.addProperty("key", key);
            requestData.addProperty("type", type.value());

            Map<String, String> formData = new LinkedHashMap<>();
            formData.put("data", JsonUtils.toJsonString(requestData));

            HttpResponse response = HttpUtils.post("https://www.mcmod.cn/object/CommonSelect/")
                    .formBody(formData)
                    .header("Referer", "https://www.mcmod.cn")
                    .execute();

            if (response == null || response.getBodyAsString() == null) {
                return List.of();
            }

            JsonObject json = JsonUtils.parseJsonObject(response.getBodyAsString());
            if (json == null || json.get("state").getAsInt() != 0 || !json.has("html")) {
                return List.of();
            }
            String html = json.get("html").getAsString();
            return parseSearchHtml(html, type);
        } catch (Exception e) {
            LOGGER.error("Search exception, type: {}, keyword: {}", type, key, e);
            return List.of();
        }
    }

    /**
     * 搜索mod
     *
     * @param key 关键词
     * @return 搜索结果列表
     */
    public static @Nonnull List<McModContent> searchMod(String key) {
        return search(EnumSearchType.MOD, key);
    }

    /**
     * 搜索整合包
     *
     * @param key 关键词
     * @return 搜索结果列表
     */
    public static @Nonnull List<McModContent> searchModpack(String key) {
        return search(EnumSearchType.MODPACK, key);
    }

    /**
     * 搜索作者
     *
     * @param key 关键词
     * @return 搜索结果列表
     */
    public static @Nonnull List<McModContent> searchAuthor(String key) {
        return search(EnumSearchType.AUTHOR, key);
    }

    public static McModContent getModName(String modId) {
        List<McModContent> result = searchMod(modId);
        return result.size() == 1 && String.valueOf(result.getFirst().getId()).equals(modId) ? result.getFirst() : null;
    }

    public static McModContent getAuthorName(String authorId) {
        List<McModContent> result = searchAuthor(authorId);
        return result.size() == 1 && String.valueOf(result.getFirst().getId()).equals(authorId) ? result.getFirst() : null;
    }

    public static McModContent getModpackName(String modpackId) {
        List<McModContent> result = searchModpack(modpackId);
        return result.size() == 1 && String.valueOf(result.getFirst().getId()).equals(modpackId) ? result.getFirst() : null;
    }

    /**
     * 获取首页模组列表
     *
     * @param category 模组分类
     * @return 首页分类模组列表结果
     */
    public static @Nullable McModIndexCategoryResult getIndexCategory(EnumModCategory category) {
        try {
            Map<String, String> formData = new LinkedHashMap<>();
            formData.put("categoryID", String.valueOf(category.value()));

            HttpResponse response = HttpUtils.post("https://www.mcmod.cn/object/index/v3_IndexCategory/")
                    .formBody(formData)
                    .header("Referer", "https://www.mcmod.cn")
                    .execute();

            if (response == null || response.getBodyAsString() == null) {
                return null;
            }

            JsonObject json = JsonUtils.parseJsonObject(response.getBodyAsString());
            if (json == null || json.get("state").getAsInt() != 0 || !json.has("html")) {
                return null;
            }
            String html = json.get("html").getAsString();
            return parseIndexCategoryHtml(html);
        } catch (Exception e) {
            LOGGER.error("Exception getting index mod list, category: {}", category, e);
            return null;
        }
    }

    /**
     * 随便看看
     */
    public static @Nonnull List<McModContent> getRandomMods() {
        try {
            HttpResponse response = HttpUtils.post("https://www.mcmod.cn/ajax/index/ajax___index_random.php")
                    .header("Referer", "https://www.mcmod.cn")
                    .execute();

            if (response == null || response.getBodyAsString() == null) {
                return new ArrayList<>();
            }

            String html = response.getBodyAsString();
            return parseRandomModsHtml(html);
        } catch (Exception e) {
            LOGGER.error("Exception getting random mod list", e);
            return new ArrayList<>();
        }
    }

    /**
     * 获取评论列表
     *
     * @param type 类型
     * @param doid 对象ID
     * @param page 页码, 从1开始
     */
    public static McModCommentResult getComments(EnumContentType type, String doid, Integer page) {
        return getComments(type, null, doid, page, false);
    }

    /**
     * 获取评论列表
     *
     * @param type    类型
     * @param channel 频道
     * @param doid    对象ID
     * @param page    页码, 从1开始
     * @param self    是否只看自己
     */
    public static McModCommentResult getComments(EnumContentType type, String channel, String doid, Integer page, boolean self) {
        try {
            JsonObject requestData = new JsonObject();
            requestData.addProperty("type", type.value());
            requestData.addProperty("channel", StringUtils.isNullOrEmpty(channel) ? "1" : channel);
            requestData.addProperty("doid", doid);
            requestData.addProperty("page", page != null ? page : 1);
            requestData.addProperty("selfonly", self ? 1 : 0);

            Map<String, String> formData = new LinkedHashMap<>();
            formData.put("data", JsonUtils.toJsonString(requestData));

            HttpResponse response = HttpUtils.post("https://www.mcmod.cn/frame/comment/CommentRow/")
                    .formBody(formData)
                    .header("Referer", "https://www.mcmod.cn")
                    .execute();

            if (response == null || response.getBodyAsString() == null) {
                return null;
            }

            JsonObject json = JsonUtils.parseJsonObject(response.getBodyAsString());
            if (json == null || json.get("state").getAsInt() != 0 || !json.has("data")) {
                return null;
            }

            JsonObject data = json.getAsJsonObject("data");
            McModCommentResult result = com.mikuac.shiro.common.utils.JsonUtils.readValue(JsonUtils.toJsonString(data), new TypeReference<>() {
            });
            return result != null ? result.set(type, doid, null) : null;
        } catch (Exception e) {
            LOGGER.error("Exception getting comments", e);
            return null;
        }
    }

    /**
     * 获取评论回复列表
     *
     * @param replyId 评论ID
     * @param page    页码, 从1开始
     */
    public static @Nullable McModCommentResult getCommentReplies(String replyId, Integer page) {
        try {
            JsonObject requestData = new JsonObject();
            requestData.addProperty("replyID", replyId);
            requestData.addProperty("page", page != null ? page : 1);

            Map<String, String> formData = new LinkedHashMap<>();
            formData.put("data", JsonUtils.toJsonString(requestData));

            HttpResponse response = HttpUtils.post("https://www.mcmod.cn/frame/comment/CommentReply/")
                    .formBody(formData)
                    .header("Referer", "https://www.mcmod.cn")
                    .execute();

            if (response == null || response.getBodyAsString() == null) {
                return null;
            }

            JsonObject json = JsonUtils.parseJsonObject(response.getBodyAsString());
            if (json == null || json.get("state").getAsInt() != 0 || !json.has("data")) {
                return null;
            }

            JsonObject data = json.getAsJsonObject("data");
            McModCommentResult result = com.mikuac.shiro.common.utils.JsonUtils.readValue(JsonUtils.toJsonString(data), new TypeReference<>() {
            });
            return result != null ? result.setReplyId(replyId) : null;
        } catch (Exception e) {
            LOGGER.error("Exception getting comment replies", e);
            return null;
        }
    }

    /**
     * 评论
     *
     * @param groupId   群号
     * @param type      类型
     * @param container 容器ID
     * @param text      评论内容（HTML格式，如 "&lt;p&gt;测试&lt;/p&gt;"）
     */
    public static McModCommentResponse comment(Long groupId, EnumContentType type, String container, String text) {
        return comment(groupId, type, null, container, text, 0);
    }

    /**
     * 评论
     *
     * @param groupId   群号
     * @param type      类型
     * @param channel   频道
     * @param container 容器ID
     * @param text      评论内容（HTML格式，如 "&lt;p&gt;测试&lt;/p&gt;"）
     * @param quote     引用ID, 默认0
     */
    public static McModCommentResponse comment(Long groupId, EnumContentType type, String channel, String container, String text, Integer quote) {
        try {
            String cookie = getCookie(groupId);
            if (cookie == null) {
                LOGGER.error("Cannot get cookie, comment failed, groupId: {}", groupId);
                return null;
            }

            JsonObject requestData = new JsonObject();
            requestData.addProperty("todo", "submit");
            requestData.addProperty("type", type.value());
            requestData.addProperty("channel", StringUtils.isNullOrEmpty(channel) ? "1" : channel);
            requestData.addProperty("container", container);
            requestData.addProperty("quote", quote != null ? quote : 0);
            requestData.addProperty("text", text);

            Map<String, String> formData = new LinkedHashMap<>();
            formData.put("data", JsonUtils.toJsonString(requestData));

            HttpResponse response = HttpUtils.post("https://www.mcmod.cn/action/doComment/")
                    .formBody(formData)
                    .header("Referer", "https://www.mcmod.cn")
                    .header("Cookie", cookie)
                    .execute();

            if (response == null || response.getBodyAsString() == null) {
                return null;
            }

            JsonObject json = JsonUtils.parseJsonObject(response.getBodyAsString());
            if (json == null) {
                return null;
            }

            return com.mikuac.shiro.common.utils.JsonUtils.readValue(xin.vanilla.banira.util.JsonUtils.toJsonString(json), new TypeReference<>() {
            });
        } catch (Exception e) {
            LOGGER.error("Comment exception", e);
            return null;
        }
    }

    /**
     * 回复评论
     *
     * @param groupId   群号
     * @param type      类型
     * @param container 容器ID
     * @param commentId 回复的评论ID
     * @param text      回复内容
     */
    public static McModCommentResponse replyComment(Long groupId, EnumContentType type, String container, String commentId, String text) {
        return replyComment(groupId, type, "1", container, commentId, text);
    }

    /**
     * 回复评论
     *
     * @param groupId   群号
     * @param type      类型
     * @param channel   频道
     * @param container 容器ID
     * @param commentId 回复的评论ID
     * @param text      回复内容
     */
    public static McModCommentResponse replyComment(Long groupId, EnumContentType type, String channel, String container, String commentId, String text) {
        try {
            String cookie = getCookie(groupId);
            if (cookie == null) {
                LOGGER.error("Cannot get cookie, reply comment failed, groupId: {}", groupId);
                return null;
            }

            JsonObject requestData = new JsonObject();
            requestData.addProperty("todo", "submit");
            requestData.addProperty("type", type.value());
            requestData.addProperty("channel", StringUtils.isNullOrEmpty(channel) ? "1" : channel);
            requestData.addProperty("container", container);
            requestData.addProperty("reply", commentId);
            requestData.addProperty("text", text);

            Map<String, String> formData = new LinkedHashMap<>();
            formData.put("data", JsonUtils.toJsonString(requestData));

            HttpResponse response = HttpUtils.post("https://www.mcmod.cn/action/doComment/")
                    .formBody(formData)
                    .header("Referer", "https://www.mcmod.cn")
                    .header("Cookie", cookie)
                    .execute();

            if (response == null || response.getBodyAsString() == null) {
                return null;
            }

            JsonObject json = JsonUtils.parseJsonObject(response.getBodyAsString());
            if (json == null) {
                return null;
            }

            return com.mikuac.shiro.common.utils.JsonUtils.readValue(xin.vanilla.banira.util.JsonUtils.toJsonString(json), new TypeReference<>() {
            });
        } catch (Exception e) {
            LOGGER.error("Reply comment exception", e);
            return null;
        }
    }

    /**
     * 删除评论
     *
     * @param groupId   群号
     * @param commentId 要删除的评论ID
     */
    public static McModCommentResponse deleteComment(Long groupId, String commentId) {
        try {
            String cookie = getCookie(groupId);
            if (cookie == null) {
                LOGGER.error("Cannot get cookie, delete comment failed, groupId: {}", groupId);
                return null;
            }

            JsonObject requestData = new JsonObject();
            requestData.addProperty("todo", "delete");
            requestData.addProperty("delete_id", commentId);

            Map<String, String> formData = new LinkedHashMap<>();
            formData.put("data", JsonUtils.toJsonString(requestData));

            HttpResponse response = HttpUtils.post("https://www.mcmod.cn/action/doComment/")
                    .formBody(formData)
                    .header("Referer", "https://www.mcmod.cn")
                    .header("Cookie", cookie)
                    .execute();

            if (response == null || response.getBodyAsString() == null) {
                return null;
            }

            JsonObject json = JsonUtils.parseJsonObject(response.getBodyAsString());
            if (json == null) {
                return null;
            }

            return com.mikuac.shiro.common.utils.JsonUtils.readValue(xin.vanilla.banira.util.JsonUtils.toJsonString(json), new TypeReference<>() {
            });
        } catch (Exception e) {
            LOGGER.error("Delete comment exception", e);
            return null;
        }
    }

    /**
     * 上传文件
     *
     * @param groupId      群号
     * @param filePath     文件路径
     * @param classID      modId
     * @param mcverList    MC版本
     * @param platformList 平台
     * @param apiList      API
     * @param tagList      标签
     * @return 文件上传响应对象
     */
    public static McModFileUploadResponse uploadFile(Long groupId, String filePath, String classID, String mcverList, List<EnumPlatformType> platformList,
                                                     List<EnumApiType> apiList, List<EnumModFileTag> tagList) {
        try {
            String cookie = getCookie(groupId);
            if (cookie == null) {
                LOGGER.error("Cannot get cookie, upload file failed, groupId: {}", groupId);
                return null;
            }

            File file = new File(filePath);
            if (!file.exists() || !file.isFile()) {
                LOGGER.error("File does not exist: {}", filePath);
                return null;
            }

            // 构建 multipart/form-data 请求体
            String boundary = "----WebKitFormBoundary" + System.currentTimeMillis();
            ByteArrayOutputStream outputStream = new ByteArrayOutputStream();

            // 文件字段
            String fileName = file.getName();
            byte[] fileBytes = Files.readAllBytes(file.toPath());
            writeMultipartField(outputStream, boundary, "0", fileBytes, fileName, "application/octet-stream");

            // classID
            if (classID != null && !classID.isEmpty()) {
                writeMultipartField(outputStream, boundary, "classID", classID.getBytes(StandardCharsets.UTF_8), null, null);
            }

            // mcverList
            if (mcverList != null && !mcverList.isEmpty()) {
                writeMultipartField(outputStream, boundary, "mcverList", mcverList.getBytes(StandardCharsets.UTF_8), null, null);
            }

            // platformList
            if (CollectionUtils.isNotNullOrEmpty(platformList)) {
                String listString = platformList.stream().map(EnumPlatformType::value).map(String::valueOf).collect(Collectors.joining(", "));
                writeMultipartField(outputStream, boundary, "platformList", listString.getBytes(StandardCharsets.UTF_8), null, null);
            }

            // apiList
            if (CollectionUtils.isNotNullOrEmpty(apiList)) {
                String listString = apiList.stream().map(EnumApiType::value).map(String::valueOf).collect(Collectors.joining(", "));
                writeMultipartField(outputStream, boundary, "apiList", listString.getBytes(StandardCharsets.UTF_8), null, null);
            }

            // tagList
            if (CollectionUtils.isNotNullOrEmpty(tagList)) {
                String listString = tagList.stream().map(EnumModFileTag::name).collect(Collectors.joining(", "));
                writeMultipartField(outputStream, boundary, "tagList", listString.getBytes(StandardCharsets.UTF_8), null, null);
            }

            // 结束边界
            outputStream.write(("--" + boundary + "--\r\n").getBytes(StandardCharsets.UTF_8));

            byte[] bodyBytes = outputStream.toByteArray();

            // 使用 HttpRequestBuilder 构建请求
            HttpResponse response = HttpUtils.post("https://modfile-dl.mcmod.cn/action/upload/")
                    .header("Referer", "https://modfile-dl.mcmod.cn")
                    .header("Cookie", cookie)
                    .contentType("multipart/form-data; boundary=" + boundary)
                    .body(bodyBytes)
                    .execute();

            if (response == null || response.getBodyAsString() == null) {
                return null;
            }

            JsonObject json = JsonUtils.parseJsonObject(response.getBodyAsString());
            if (json == null) {
                return null;
            }

            return com.mikuac.shiro.common.utils.JsonUtils.readValue(xin.vanilla.banira.util.JsonUtils.toJsonString(json), new TypeReference<>() {
            });
        } catch (Exception e) {
            LOGGER.error("Upload file exception", e);
            return null;
        }
    }

    /**
     * 删除文件
     *
     * @param groupId 群号
     * @param fileId  文件ID
     * @return 删除响应对象
     */
    public static McModCommentResponse deleteFile(Long groupId, String fileId) {
        try {
            String cookie = getCookie(groupId);
            if (cookie == null) {
                LOGGER.error("Cannot get cookie, delete file failed, groupId: {}", groupId);
                return null;
            }

            // 构建 multipart/form-data 请求体
            String boundary = "----WebKitFormBoundary" + System.currentTimeMillis();
            ByteArrayOutputStream outputStream = new ByteArrayOutputStream();

            // modfileID
            writeMultipartField(outputStream, boundary, "modfileID", fileId.getBytes(StandardCharsets.UTF_8), null, null);

            // 结束边界
            outputStream.write(("--" + boundary + "--\r\n").getBytes(StandardCharsets.UTF_8));

            byte[] bodyBytes = outputStream.toByteArray();

            HttpResponse response = HttpUtils.post("https://modfile-dl.mcmod.cn/action/delete/")
                    .header("Referer", "https://modfile-dl.mcmod.cn")
                    .header("Cookie", cookie)
                    .contentType("multipart/form-data; boundary=" + boundary)
                    .body(bodyBytes)
                    .execute();

            if (response == null || response.getBodyAsString() == null) {
                return null;
            }

            JsonObject json = JsonUtils.parseJsonObject(response.getBodyAsString());
            if (json == null) {
                return null;
            }

            return com.mikuac.shiro.common.utils.JsonUtils.readValue(xin.vanilla.banira.util.JsonUtils.toJsonString(json), new TypeReference<>() {
            });
        } catch (Exception e) {
            LOGGER.error("Delete file exception", e);
            return null;
        }
    }

    /**
     * 获取用户卡片
     *
     * @param uid 用户ID
     */
    public static McModUserCardResult getUserCard(String uid) {
        try {
            Map<String, String> formData = new LinkedHashMap<>();
            formData.put("uid", uid);

            HttpResponse response = HttpUtils.post("https://www.mcmod.cn/object/UserCard/")
                    .formBody(formData)
                    .header("Referer", "https://www.mcmod.cn")
                    .execute();

            if (response == null || response.getBodyAsString() == null) {
                return null;
            }

            JsonObject json = JsonUtils.parseJsonObject(response.getBodyAsString());
            if (json == null || json.get("state").getAsInt() != 0 || !json.has("data")) {
                return null;
            }

            JsonObject data = json.getAsJsonObject("data");
            McModUserCardResult result = com.mikuac.shiro.common.utils.JsonUtils.readValue(JsonUtils.toJsonString(data), new TypeReference<>() {
            });
            return result == null ? null : result.setAvatar(fixUrl(result.getAvatar()));
        } catch (Exception e) {
            LOGGER.error("Exception getting user card", e);
            return null;
        }
    }

    /**
     * 清除Cookie缓存
     *
     * @param groupId 群号
     */
    public static void clearCookieCache(@Nullable Long groupId) {
        ReentrantLock lock = getGroupLock(groupId);
        lock.lock();
        try {
            McModGroupConfig otherConfig = BaniraUtils.getGroupConfigOrGlobal(McModGroupConfig.class, groupId);
            if (otherConfig != null && otherConfig.mcModCookieConfig() != null) {
                otherConfig.mcModCookieConfig().cookie(null).expireTime(null).userId(null);
                BaniraUtils.saveGroupConfig();
                LOGGER.info("Cookie cache cleared, groupId: {}", groupId);
            }
            clearVoteCache(groupId);
        } finally {
            lock.unlock();
        }
    }

    /**
     * 清除投票内存缓存
     */
    public static void clearVoteCache(@Nullable Long groupId) {
        String prefix = resolveVoteAccountKey(groupId) + ":";
        VOTE_STATE_CACHE.keySet().removeIf(key -> key.startsWith(prefix));
        VOTE_RESPONSE_CACHE.keySet().removeIf(key -> key.startsWith(prefix));
        VOTE_COOLDOWN_CACHE.keySet().removeIf(key -> key.startsWith(prefix));
    }

    // region 模组/整合包互动

    private static final String DO_CLASS_URL = "https://www.mcmod.cn/action/doClass/";
    private static final String DO_MODPACK_URL = "https://www.mcmod.cn/action/doModpack/";
    private static final long VOTE_COOLDOWN_MS = 60_000;

    /**
     * 投票状态缓存：account:target:type -> 状态
     */
    private static final Map<String, EnumVoteState> VOTE_STATE_CACHE = new ConcurrentHashMap<>();
    /**
     * 投票响应缓存，用于命中缓存时展示票数
     */
    private static final Map<String, McModCardVoteResponse> VOTE_RESPONSE_CACHE = new ConcurrentHashMap<>();
    /**
     * 投票冷却：account:target -> 上次实际请求时间
     */
    private static final Map<String, Long> VOTE_COOLDOWN_CACHE = new ConcurrentHashMap<>();

    /**
     * 给模组点推荐
     *
     * @param groupId 群号，用于读取已配置的 Cookie（可选）
     * @param modId   模组 ID
     */
    public static @Nullable McModPushResponse pushMod(@Nullable Long groupId, @Nonnull String modId) {
        return pushMod(groupId, modId, resolveOptionalCookie(groupId));
    }

    /**
     * 给模组点推荐
     *
     * @param groupId 群号
     * @param modId   模组 ID
     * @param cookie  Cookie（可选）
     */
    public static @Nullable McModPushResponse pushMod(@Nullable Long groupId, @Nonnull String modId, @Nullable String cookie) {
        Long cid = parsePositiveId(modId, "mod");
        if (cid == null) {
            return null;
        }
        JsonObject requestData = new JsonObject();
        requestData.addProperty("todo", "push");
        requestData.addProperty("cid", cid);
        return postDoClassPushAction(requestData, getModUrl(modId), cookie);
    }

    /**
     * 给模组投红票/黑票，重复请求会取消投票
     *
     * @param groupId 群号，用于读取已配置的 Cookie（可选）
     * @param modId   模组 ID
     * @param type    投票类型
     */
    public static @Nullable McModCardVoteResponse voteMod(@Nullable Long groupId, @Nonnull String modId, @Nonnull EnumCardVoteType type) {
        return voteMod(groupId, modId, type, resolveOptionalCookie(groupId));
    }

    /**
     * 给模组投红票
     */
    public static @Nullable McModCardVoteResponse voteModRed(@Nullable Long groupId, @Nonnull String modId) {
        return voteMod(groupId, modId, EnumCardVoteType.RED);
    }

    /**
     * 给模组投黑票
     */
    public static @Nullable McModCardVoteResponse voteModBlack(@Nullable Long groupId, @Nonnull String modId) {
        return voteMod(groupId, modId, EnumCardVoteType.BLACK);
    }

    /**
     * 给模组投红票/黑票，重复请求会取消投票
     *
     * @param groupId 群号
     * @param modId   模组 ID
     * @param type    投票类型
     * @param cookie  Cookie（可选）
     */
    public static @Nullable McModCardVoteResponse voteMod(@Nullable Long groupId, @Nonnull String modId,
                                                          @Nonnull EnumCardVoteType type, @Nullable String cookie) {
        Long cid = parsePositiveId(modId, "mod");
        if (cid == null) {
            return null;
        }
        JsonObject requestData = new JsonObject();
        requestData.addProperty("todo", "cardvote");
        requestData.addProperty("cid", cid);
        requestData.addProperty("type", type.value());
        return postDoClassVoteAction(requestData, getModUrl(modId), cookie);
    }

    /**
     * 给整合包点推荐
     *
     * @param groupId   群号，用于读取已配置的 Cookie（可选）
     * @param modpackId 整合包 ID
     */
    public static @Nullable McModPushResponse pushModpack(@Nullable Long groupId, @Nonnull String modpackId) {
        return pushModpack(groupId, modpackId, resolveOptionalCookie(groupId));
    }

    /**
     * 给整合包点推荐
     *
     * @param groupId   群号
     * @param modpackId 整合包 ID
     * @param cookie    Cookie（可选）
     */
    public static @Nullable McModPushResponse pushModpack(@Nullable Long groupId, @Nonnull String modpackId, @Nullable String cookie) {
        Long pid = parsePositiveId(modpackId, "modpack");
        if (pid == null) {
            return null;
        }
        JsonObject requestData = new JsonObject();
        requestData.addProperty("todo", "push");
        requestData.addProperty("pid", pid);
        return postDoModpackPushAction(requestData, getModpackUrl(modpackId), cookie);
    }

    /**
     * 给整合包投红票/黑票，重复请求会取消投票
     *
     * @param groupId   群号，用于读取已配置的 Cookie（可选）
     * @param modpackId 整合包 ID
     * @param type      投票类型
     */
    public static @Nullable McModCardVoteResponse voteModpack(@Nullable Long groupId, @Nonnull String modpackId,
                                                              @Nonnull EnumCardVoteType type) {
        return voteModpack(groupId, modpackId, type, resolveOptionalCookie(groupId));
    }

    /**
     * 给整合包投红票
     */
    public static @Nullable McModCardVoteResponse voteModpackRed(@Nullable Long groupId, @Nonnull String modpackId) {
        return voteModpack(groupId, modpackId, EnumCardVoteType.RED);
    }

    /**
     * 给整合包投黑票
     */
    public static @Nullable McModCardVoteResponse voteModpackBlack(@Nullable Long groupId, @Nonnull String modpackId) {
        return voteModpack(groupId, modpackId, EnumCardVoteType.BLACK);
    }

    /**
     * 给整合包投红票/黑票，重复请求会取消投票
     *
     * @param groupId   群号
     * @param modpackId 整合包 ID
     * @param type      投票类型
     * @param cookie    Cookie（可选）
     */
    public static @Nullable McModCardVoteResponse voteModpack(@Nullable Long groupId, @Nonnull String modpackId,
                                                              @Nonnull EnumCardVoteType type, @Nullable String cookie) {
        Long pid = parsePositiveId(modpackId, "modpack");
        if (pid == null) {
            return null;
        }
        JsonObject requestData = new JsonObject();
        requestData.addProperty("todo", "cardvote");
        requestData.addProperty("pid", pid);
        requestData.addProperty("type", type.value());
        return postDoModpackVoteAction(requestData, getModpackUrl(modpackId), cookie);
    }

    /**
     * 确保模组已投指定类型票。
     */
    public static @Nullable McModCardVoteEnsureResult ensureModVote(@Nullable Long groupId, @Nonnull String modId,
                                                                    @Nonnull EnumCardVoteType type) {
        return ensureVoteState(groupId, modId, true, type, true);
    }

    /**
     * 确保模组未投指定类型票。
     */
    public static @Nullable McModCardVoteEnsureResult ensureModUnvote(@Nullable Long groupId, @Nonnull String modId,
                                                                      @Nonnull EnumCardVoteType type) {
        return ensureVoteState(groupId, modId, true, type, false);
    }

    /**
     * 确保整合包已投指定类型票。
     */
    public static @Nullable McModCardVoteEnsureResult ensureModpackVote(@Nullable Long groupId, @Nonnull String modpackId,
                                                                        @Nonnull EnumCardVoteType type) {
        return ensureVoteState(groupId, modpackId, false, type, true);
    }

    /**
     * 确保整合包未投指定类型票。
     */
    public static @Nullable McModCardVoteEnsureResult ensureModpackUnvote(@Nullable Long groupId, @Nonnull String modpackId,
                                                                         @Nonnull EnumCardVoteType type) {
        return ensureVoteState(groupId, modpackId, false, type, false);
    }

    // endregion 模组/整合包互动


    /**
     * 搜索
     */
    public static @Nonnull List<McModSearchResult> searchBySearchPage(EnumSearchType type, String query) {
        try {
            String encodedQuery = URLEncoder.encode(query, StandardCharsets.UTF_8);
            String searchUrl = String.format("https://search.mcmod.cn/s?key=%s&filter=%d&mold=1", encodedQuery, type.filter());
            String html = requestSearchPageHtml(searchUrl);
            if (StringUtils.isNullOrEmpty(html)) {
                LOGGER.warn("Search page fetch failed, filter: {}, query: {}", type.filter(), query);
                return List.of();
            }
            return parseSearchPageHtml(html);
        } catch (Exception e) {
            LOGGER.error("Search by search page exception, filter: {}, query: {}", type, query, e);
            return List.of();
        }
    }

    @Nullable
    private static String requestSearchPageHtml(@Nonnull String searchUrl) {
        String html = requestSearchPage(searchUrl, cachedSearchYxdToken);
        if (isSearchChallengeHtml(html)) {
            String token = extractSearchYxdToken(html);
            if (StringUtils.isNullOrEmpty(token)) {
                token = bootstrapSearchYxdToken();
            }
            if (StringUtils.isNotNullOrEmpty(token)) {
                cachedSearchYxdToken = token;
                html = requestSearchPage(searchUrl, token);
            }
        }
        return isSearchChallengeHtml(html) ? null : html;
    }

    @Nullable
    private static String requestSearchPage(@Nonnull String searchUrl, @Nullable String yxdToken) {
        HttpRequestBuilder builder = HttpUtils.get(searchUrl)
                .header("User-Agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
                .header("Referer", "https://search.mcmod.cn/")
                .header("Accept-Language", "zh-CN,zh;q=0.9")
                .header("X-Requested-With", "XMLHttpRequest");
        if (StringUtils.isNotNullOrEmpty(yxdToken)) {
            builder.header("Cookie", "yxd_token=" + yxdToken);
        }
        HttpResponse response = builder.execute();
        return response == null ? null : response.getBodyAsString();
    }

    private static boolean isSearchChallengeHtml(@Nullable String html) {
        if (StringUtils.isNullOrEmpty(html) || html.length() < 500) {
            return true;
        }
        return html.contains("yxd_token=") && html.contains("window.location.href");
    }

    @Nullable
    private static String extractSearchYxdToken(@Nullable String html) {
        if (StringUtils.isNullOrEmpty(html)) {
            return null;
        }
        Matcher matcher = YXD_TOKEN_PATTERN.matcher(html);
        return matcher.find() ? matcher.group(1) : null;
    }

    @Nullable
    private static String bootstrapSearchYxdToken() {
        if (StringUtils.isNotNullOrEmpty(cachedSearchYxdToken)) {
            return cachedSearchYxdToken;
        }
        String[] bootstrapUrls = {
                "https://www.mcmod.cn/",
                "https://search.mcmod.cn/"
        };
        for (String bootstrapUrl : bootstrapUrls) {
            HttpResponse response = HttpUtils.get(bootstrapUrl,
                    new KeyValue<>("User-Agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"),
                    new KeyValue<>("Referer", "https://www.mcmod.cn"));
            String token = extractSearchYxdToken(response == null ? null : response.getBodyAsString());
            if (StringUtils.isNotNullOrEmpty(token)) {
                cachedSearchYxdToken = token;
                return token;
            }
        }
        return null;
    }

    /**
     * 搜索模组
     */
    public static @Nonnull List<McModSearchResult> searchModBySearchPage(String query) {
        return searchBySearchPage(EnumSearchType.MOD, query);
    }

    /**
     * 搜索整合包
     */
    public static @Nonnull List<McModSearchResult> searchModpackBySearchPage(String query) {
        return searchBySearchPage(EnumSearchType.MODPACK, query);
    }

    /**
     * 搜索资料
     */
    public static @Nonnull List<McModSearchResult> searchDataBySearchPage(String query) {
        return searchBySearchPage(EnumSearchType.ITEM, query);
    }

    /**
     * 搜索教程
     */
    public static @Nonnull List<McModSearchResult> searchTutorialBySearchPage(String query) {
        return searchBySearchPage(EnumSearchType.TUTORIAL, query);
    }

    /**
     * 搜索作者
     */
    public static @Nonnull List<McModSearchResult> searchAuthorBySearchPage(String query) {
        return searchBySearchPage(EnumSearchType.AUTHOR, query);
    }

    /**
     * 搜索用户
     */
    public static @Nonnull List<McModSearchResult> searchUserBySearchPage(String query) {
        return searchBySearchPage(EnumSearchType.USER, query);
    }

    /**
     * 解析搜索页面返回的HTML
     */
    private static List<McModSearchResult> parseSearchPageHtml(String html) {
        List<McModSearchResult> results = new ArrayList<>();
        if (StringUtils.isNullOrEmpty(html)) {
            return results;
        }

        try {
            Document doc = Jsoup.parse(html);

            // 匹配搜索结果项：.result-item, .media, .search-list .item, .user-list .row, .list .row
            Elements items = doc.select(".result-item, .media, .search-list .item, .user-list .row, .list .row");

            for (Element item : items) {
                McModSearchResult result = parseSearchPageItem(item);
                if (result != null && result.getTitle() != null && result.getLink() != null) {
                    results.add(result);
                }
            }
        } catch (Exception e) {
            LOGGER.warn("Exception parsing search page HTML: {}", e.getMessage());
        }

        return results;
    }

    /**
     * 解析单个搜索结果项
     */
    private static @Nullable McModSearchResult parseSearchPageItem(Element item) {
        try {
            // 提取副标题
            String subtitle = null;
            Element subtitleItem = item.selectFirst(".head .class-category a");
            if (subtitleItem != null) {
                String categoryClass = subtitleItem.attr("class");
                if (StringUtils.isNotNullOrEmpty(categoryClass) && categoryClass.startsWith("c_")) {
                    EnumModCategory category = EnumModCategory.valueOfEx(categoryClass);
                    subtitle = category == null ? null : category.name();
                }
            } else {
                Element headItem = item.selectFirst(".head");
                if (headItem != null && headItem.childNodeSize() > 1 && headItem.childNode(0) instanceof TextNode node) {
                    subtitle = cleanText(node.text());
                }
            }

            String title = null;
            String link = null;
            // 尝试多种选择器：.head > a, .media-heading a, 或第一个有文本的 a 标签
            Element titleLink = item.selectFirst(".head > a");
            if (titleLink == null) {
                titleLink = item.selectFirst(".media-heading a");
            }
            if (titleLink == null) {
                // 查找第一个有文本的 a 标签
                Elements links = item.select("a");
                for (Element a : links) {
                    String text = a.text().trim();
                    if (!text.isEmpty()) {
                        titleLink = a;
                        break;
                    }
                }
            }

            if (titleLink != null) {
                link = titleLink.attr("href");
                title = cleanText(titleLink.text());
            }

            if (title == null || title.isEmpty() || link == null || link.isEmpty()) {
                return null;
            }

            if (link.contains("center.mcmod.cn")) {
                title = title.replaceAll("\\(QQ[^)]*\\)", "").trim();
            }

            // 修复链接URL
            link = fixUrl(link);
            if (link == null || link.contains("target=") || Pattern.matches("^\\d+$", title)) {
                return null;
            }

            // 提取模组名称
            String modName = null;
            Element modNameEl = item.selectFirst(".meta span, .source, .media-body .text-muted");
            if (modNameEl != null) {
                modName = cleanText(modNameEl.text());
            }

            // 提取摘要
            String summary = null;
            Element bodyEl = item.selectFirst(".body, .media-body");
            if (bodyEl != null) {
                String bodyText = cleanText(bodyEl.text());
                if (!bodyText.isEmpty()) {
                    // 移除标题和模组名称
                    summary = bodyText.replace(title, "").replace(modName != null ? modName : "", "").trim();
                }
            }

            // 提取封面图
            String imageUrl = null;
            Element coverImg = item.selectFirst(".media-object, .cover img, img");
            if (coverImg != null) {
                imageUrl = coverImg.attr("data-original");
                if (StringUtils.isNullOrEmpty(imageUrl)) {
                    imageUrl = coverImg.attr("src");
                }
                imageUrl = fixUrl(imageUrl);
            }

            // 提取时间
            Date snapshotTime = null;
            Elements footNodes = item.select(".foot .info .value");
            for (Element footNode : footNodes) {
                snapshotTime = DateUtils.format(footNode.text());
                if (snapshotTime != null) break;
            }

            return new McModSearchResult()
                    .setSubtitle(subtitle)
                    .setTitle(title)
                    .setSummary(summary != null ? summary : "")
                    .setLink(link)
                    .setSnapshotTime(snapshotTime)
                    .setImageUrl(imageUrl);
        } catch (Exception e) {
            LOGGER.warn("Exception parsing search page item: {}", e.getMessage());
            return null;
        }
    }

    /**
     * 清理文本
     */
    private static String cleanText(String text) {
        if (text == null) {
            return "";
        }
        return text.replaceAll("[\\r\\n\\t]+", "").trim();
    }

    /**
     * 修复URL
     */
    public static @Nullable String fixUrl(String url) {
        if (StringUtils.isNullOrEmptyEx(url)) {
            return null;
        }
        if (url.startsWith("//")) {
            return "https:" + url;
        }
        if (url.startsWith("/")) {
            return "https://mcmod.cn" + url;
        }
        if (!url.startsWith("http")) {
            return "https://mcmod.cn/" + url;
        }
        return url;
    }

    /**
     * 读取已配置且未过期的 Cookie，不触发登录
     */
    private static @Nullable String resolveOptionalCookie(@Nullable Long groupId) {
        if (groupId == null) {
            return null;
        }
        ReentrantLock lock = getGroupLock(groupId);
        lock.lock();
        try {
            McModGroupConfig config = BaniraUtils.getGroupConfigOrGlobal(McModGroupConfig.class, groupId);
            if (config == null || config.mcModCookieConfig() == null) {
                return null;
            }
            McModCookieConfig cookieConfig = config.mcModCookieConfig();
            if (StringUtils.isNotNullOrEmpty(cookieConfig.cookie()) && !cookieConfig.isExpired()) {
                return cookieConfig.cookie();
            }
            return null;
        } finally {
            lock.unlock();
        }
    }

    private static @Nullable Long parsePositiveId(@Nonnull String id, @Nonnull String fieldName) {
        if (!id.matches("\\d+")) {
            LOGGER.warn("Invalid {} id: {}", fieldName, id);
            return null;
        }
        long value = StringUtils.toLong(id);
        if (value <= 0) {
            LOGGER.warn("Invalid {} id: {}", fieldName, id);
            return null;
        }
        return value;
    }

    private static @Nullable JsonObject postMcModAction(@Nonnull String url, @Nonnull JsonObject requestData,
                                                        @Nonnull String referer, @Nullable String cookie) {
        try {
            Map<String, String> formData = new LinkedHashMap<>();
            formData.put("data", JsonUtils.toJsonString(requestData));

            HttpRequestBuilder builder = HttpUtils.post(url)
                    .formBody(formData)
                    .header("Referer", referer);
            if (StringUtils.isNotNullOrEmpty(cookie)) {
                builder.header("Cookie", cookie);
            }

            HttpResponse response = builder.execute();
            if (response == null || response.getBodyAsString() == null) {
                return null;
            }

            return JsonUtils.parseJsonObject(response.getBodyAsString());
        } catch (Exception e) {
            LOGGER.error("MCMod action exception, url: {}, data: {}", url, requestData, e);
            return null;
        }
    }

    private static @Nullable McModCardVoteEnsureResult ensureVoteState(@Nullable Long groupId, @Nonnull String targetId,
                                                                       boolean isMod, @Nonnull EnumCardVoteType type,
                                                                       boolean wantVoted) {
        if (parsePositiveId(targetId, isMod ? "mod" : "modpack") == null) {
            return null;
        }

        String cacheKey = buildVoteCacheKey(groupId, isMod, targetId, type);
        String cooldownKey = buildVoteCooldownKey(groupId, isMod, targetId);
        EnumVoteState desired = wantVoted ? EnumVoteState.VOTED : EnumVoteState.NOT_VOTED;

        EnumVoteState cachedState = VOTE_STATE_CACHE.get(cacheKey);
        if (cachedState == desired) {
            return new McModCardVoteEnsureResult()
                    .setStateBefore(cachedState)
                    .setStateAfter(cachedState)
                    .setResponse(VOTE_RESPONSE_CACHE.get(cacheKey))
                    .setApiCalls(0)
                    .setFromCache(true);
        }

        long now = System.currentTimeMillis();
        Long lastAttempt = VOTE_COOLDOWN_CACHE.get(cooldownKey);
        if (lastAttempt != null && now - lastAttempt < VOTE_COOLDOWN_MS) {
            return new McModCardVoteEnsureResult().setCooldownBlocked(true).setApiCalls(0);
        }

        String cookie = resolveOptionalCookie(groupId);
        McModCardVoteResponse first = invokeVote(groupId, targetId, isMod, type, cookie);
        McModCardVoteResponse second = invokeVote(groupId, targetId, isMod, type, cookie);
        if (!isVoteResponseValid(first) || !isVoteResponseValid(second)) {
            return null;
        }

        EnumVoteState stateBefore = detectVoteStateBefore(first, second, type);
        McModCardVoteEnsureResult result;
        if (stateBefore == desired) {
            result = new McModCardVoteEnsureResult()
                    .setStateBefore(stateBefore)
                    .setStateAfter(stateBefore)
                    .setResponse(second)
                    .setApiCalls(2);
        } else {
            McModCardVoteResponse third = invokeVote(groupId, targetId, isMod, type, cookie);
            if (!isVoteResponseValid(third)) {
                return null;
            }
            result = new McModCardVoteEnsureResult()
                    .setStateBefore(stateBefore)
                    .setStateAfter(desired)
                    .setResponse(third)
                    .setApiCalls(3);
        }

        rememberVoteState(cacheKey, cooldownKey, result.getStateAfter(), result.getResponse());
        return result;
    }

    @Nonnull
    private static String resolveVoteAccountKey(@Nullable Long groupId) {
        String userId = getLoginUserId(groupId);
        if (StringUtils.isNotNullOrEmpty(userId)) {
            return "u:" + userId;
        }
        if (groupId != null) {
            return "g:" + groupId;
        }
        return "anon";
    }

    @Nonnull
    private static String buildVoteCacheKey(@Nullable Long groupId, boolean isMod, @Nonnull String targetId,
                                          @Nonnull EnumCardVoteType type) {
        return resolveVoteAccountKey(groupId) + ":" + (isMod ? "mod" : "modpack") + ":" + targetId + ":" + type.value();
    }

    @Nonnull
    private static String buildVoteCooldownKey(@Nullable Long groupId, boolean isMod, @Nonnull String targetId) {
        return resolveVoteAccountKey(groupId) + ":" + (isMod ? "mod" : "modpack") + ":" + targetId;
    }

    private static void rememberVoteState(@Nonnull String cacheKey, @Nonnull String cooldownKey,
                                          @Nonnull EnumVoteState state, @Nullable McModCardVoteResponse response) {
        VOTE_STATE_CACHE.put(cacheKey, state);
        if (response != null) {
            VOTE_RESPONSE_CACHE.put(cacheKey, response);
        }
        VOTE_COOLDOWN_CACHE.put(cooldownKey, System.currentTimeMillis());
    }

    @Nullable
    private static McModCardVoteResponse invokeVote(@Nullable Long groupId, @Nonnull String targetId, boolean isMod,
                                                    @Nonnull EnumCardVoteType type, @Nullable String cookie) {
        return isMod
                ? voteMod(groupId, targetId, type, cookie)
                : voteModpack(groupId, targetId, type, cookie);
    }

    private static boolean isVoteResponseValid(@Nullable McModCardVoteResponse response) {
        return response != null && response.isSuccess();
    }

    /**
     * 根据探测前后两次返回值推断投票前状态。
     * 第一次调用若使目标类型票数上升，说明原先未投；下降则说明原先已投。
     */
    @Nonnull
    private static EnumVoteState detectVoteStateBefore(@Nonnull McModCardVoteResponse first,
                                                       @Nonnull McModCardVoteResponse second,
                                                       @Nonnull EnumCardVoteType type) {
        int firstCount = getVoteCount(first, type);
        int secondCount = getVoteCount(second, type);
        if (firstCount > secondCount) {
            return EnumVoteState.NOT_VOTED;
        }
        if (firstCount < secondCount) {
            return EnumVoteState.VOTED;
        }
        LOGGER.warn("Unable to detect vote state from identical counts, first={}, second={}", firstCount, secondCount);
        return EnumVoteState.NOT_VOTED;
    }

    private static int getVoteCount(@Nonnull McModCardVoteResponse response, @Nonnull EnumCardVoteType type) {
        if (response.getNum() == null) {
            return 0;
        }
        String raw = type == EnumCardVoteType.RED
                ? response.getNum().getRed()
                : response.getNum().getBlack();
        return (int) StringUtils.toLong(StringUtils.nullToEmpty(raw), 0);
    }

    private static @Nullable McModPushResponse postDoClassPushAction(@Nonnull JsonObject requestData, @Nonnull String referer,
                                                                     @Nullable String cookie) {
        JsonObject json = postMcModAction(DO_CLASS_URL, requestData, referer, cookie);
        if (json == null) {
            return null;
        }
        return com.mikuac.shiro.common.utils.JsonUtils.readValue(JsonUtils.toJsonString(json), new TypeReference<>() {
        });
    }

    private static @Nullable McModCardVoteResponse postDoClassVoteAction(@Nonnull JsonObject requestData, @Nonnull String referer,
                                                                         @Nullable String cookie) {
        JsonObject json = postMcModAction(DO_CLASS_URL, requestData, referer, cookie);
        if (json == null) {
            return null;
        }
        return com.mikuac.shiro.common.utils.JsonUtils.readValue(JsonUtils.toJsonString(json), new TypeReference<>() {
        });
    }

    private static @Nullable McModPushResponse postDoModpackPushAction(@Nonnull JsonObject requestData, @Nonnull String referer,
                                                                       @Nullable String cookie) {
        JsonObject json = postMcModAction(DO_MODPACK_URL, requestData, referer, cookie);
        if (json == null) {
            return null;
        }
        return com.mikuac.shiro.common.utils.JsonUtils.readValue(JsonUtils.toJsonString(json), new TypeReference<>() {
        });
    }

    private static @Nullable McModCardVoteResponse postDoModpackVoteAction(@Nonnull JsonObject requestData, @Nonnull String referer,
                                                                         @Nullable String cookie) {
        JsonObject json = postMcModAction(DO_MODPACK_URL, requestData, referer, cookie);
        if (json == null) {
            return null;
        }
        return com.mikuac.shiro.common.utils.JsonUtils.readValue(JsonUtils.toJsonString(json), new TypeReference<>() {
        });
    }


    public static void main(String[] args) {
        System.out.println(getRandomMods());
    }
}
