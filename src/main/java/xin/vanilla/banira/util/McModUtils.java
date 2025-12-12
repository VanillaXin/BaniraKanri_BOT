package xin.vanilla.banira.util;

import com.fasterxml.jackson.core.type.TypeReference;
import com.google.gson.JsonObject;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import lombok.extern.slf4j.Slf4j;
import xin.vanilla.banira.config.entity.basic.OtherConfig;
import xin.vanilla.banira.config.entity.extended.McModCookieConfig;
import xin.vanilla.banira.domain.KeyValue;
import xin.vanilla.banira.util.http.HttpResponse;
import xin.vanilla.banira.util.mcmod.*;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
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
            OtherConfig otherConfig = BaniraUtils.getOthersConfig(groupId);
            if (otherConfig == null) {
                LOGGER.error("无法获取配置，groupId: {}", groupId);
                return null;
            }

            McModCookieConfig cookieConfig = otherConfig.mcModCookieConfig();
            if (cookieConfig == null) {
                cookieConfig = new McModCookieConfig();
                otherConfig.mcModCookieConfig(cookieConfig);
            }

            // 若配置中有 Cookie 且未过期，直接使用
            if (StringUtils.isNotNullOrEmpty(cookieConfig.cookie()) && !cookieConfig.isExpired()) {
                LOGGER.debug("使用配置中的 Cookie，groupId: {}", groupId);
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
                LOGGER.error("配置中未设置用户名或密码，无法登录，groupId: {}", groupId);
                return null;
            }

            LOGGER.info("Cookie 已过期或不存在，使用用户名密码登录，groupId: {}", groupId);
            String cookie = login(username, password);

            if (cookie != null) {
                // 登录成功，写回配置（默认25天过期）
                cookieConfig.setCookieWithExpire(cookie);
                // 获取并缓存当前登录用户ID
                String userId = getLoginUserId(cookie);
                if (userId != null) {
                    cookieConfig.userId(userId);
                }
                BaniraUtils.saveGroupConfig();
                LOGGER.info("登录成功，Cookie 已保存到配置，groupId: {}", groupId);
                return cookie;
            } else {
                LOGGER.error("登录失败，无法获取 Cookie，groupId: {}", groupId);
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
     */
    private static @Nullable String login(String username, String password) {
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

            if (response != null && response.statusCode() == 200) {
                // 从响应头中获取 Set-Cookie
                List<String> setCookies = response.getHeaders("Set-Cookie");
                if (setCookies != null && !setCookies.isEmpty()) {
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
                    return !cookieBuilder.isEmpty() ? cookieBuilder.toString() : null;
                }
            }
            LOGGER.warn("登录失败，状态码: {}", response != null ? response.statusCode() : "null");
            return null;
        } catch (Exception e) {
            LOGGER.error("登录异常", e);
            return null;
        }
    }

    private static final Pattern USER_CENTER_URL_PATTERN = Pattern.compile("//center\\.mcmod\\.cn/(?<userId>\\d+)/");

    /**
     * 解析搜索HTML结果
     *
     * @param html HTML内容
     * @param type 搜索类型
     * @return 搜索结果列表
     */
    private static List<McModSearchResult> parseSearchHtml(String html, EnumSearchType type) {
        if (StringUtils.isNullOrEmpty(html)) {
            return List.of();
        }
        List<McModSearchResult> results = new ArrayList<>();

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
                    results.add(new McModSearchResult(authorId, null, mainName, secondaryName));
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

                results.add(new McModSearchResult(modId, shortName, mainName, secondaryName));
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
            // 分离左右两部分
            Pattern leftRightPattern = Pattern.compile("<div class=\"left\">(.*?)</div>\\s*</div>\\s*<div class=\"right\">(.*?)</div>\\s*</div>",
                    Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
            Matcher leftRightMatcher = leftRightPattern.matcher(html);
            if (!leftRightMatcher.find()) {
                leftRightPattern = Pattern.compile("<div class=\"left\">(.*?)<div class=\"right\">(.*?)</div>\\s*</div>",
                        Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
                leftRightMatcher = leftRightPattern.matcher(html);
                if (!leftRightMatcher.find()) {
                    LOGGER.warn("无法分离左右两部分 HTML");
                    return null;
                }
            }

            String leftHtml = leftRightMatcher.group(1);
            String rightHtml = leftRightMatcher.group(2);

            McModIndexCategoryLeft left = parseIndexCategoryLeftHtml(leftHtml);
            McModIndexCategoryRight right = parseIndexCategoryRightHtml(rightHtml);

            return new McModIndexCategoryResult(left, right);
        } catch (Exception e) {
            LOGGER.error("解析首页分类 HTML 异常", e);
            return null;
        }
    }

    /**
     * 解析左侧 HTML
     */
    private static McModIndexCategoryLeft parseIndexCategoryLeftHtml(String html) {
        // 提取标题和链接
        Pattern titlePattern = Pattern.compile("<div class=\"title\">.*?<a[^>]*href=\"([^\"]+)\"[^>]*>([^<]+)</a>",
                Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
        Matcher titleMatcher = titlePattern.matcher(html);
        String categoryUrl = null;
        String title = null;
        if (titleMatcher.find()) {
            categoryUrl = titleMatcher.group(1);
            title = titleMatcher.group(2).trim();
        }

        // 提取描述
        Pattern descPattern = Pattern.compile("<span class=\"[it]\">([^<]+)</span>");
        Matcher descMatcher = descPattern.matcher(html);
        String description = null;
        if (descMatcher.find()) {
            description = descMatcher.group(1).trim();
        }

        // 提取模组卡片列表
        List<McModIndexCategoryModFrame> modFrames = new ArrayList<>();

        // <div class="frame">...<div class="block">...</div><div class="items">...</div></div>
        Pattern framePattern = Pattern.compile("<div class=\"frame\"[^>]*>(.*?)(?=<div class=\"frame\"|<div class=\"list\"|$)",
                Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
        Matcher frameMatcher = framePattern.matcher(html);
        while (frameMatcher.find()) {
            String frameHtml = frameMatcher.group(1);
            // 确保包含 items 块
            if (!frameHtml.contains("class=\"items\"")) {
                LOGGER.debug("Frame HTML 不包含 items 块，跳过");
                continue;
            }
            McModIndexCategoryModFrame frame = parseIndexCategoryModFrameHtml(frameHtml);
            if (frame != null) {
                modFrames.add(frame);
            }
        }

        return new McModIndexCategoryLeft(title, description, categoryUrl, modFrames);
    }

    /**
     * 解析模组卡片 HTML
     */
    private static McModIndexCategoryModFrame parseIndexCategoryModFrameHtml(String html) {
        try {
            // 提取模组ID和链接
            Pattern modLinkPattern = Pattern.compile("<a[^>]*href=\"/class/(\\d+)\\.html\"[^>]*>",
                    Pattern.CASE_INSENSITIVE);
            Matcher modLinkMatcher = modLinkPattern.matcher(html);
            long modId = 0;
            if (modLinkMatcher.find()) {
                modId = Long.parseLong(modLinkMatcher.group(1));
            }

            // 提取模组名称
            Pattern namePattern = Pattern.compile("<div class=\"name[^\"]*\">.*?<a[^>]*title=\"([^\"]+)\"[^>]*>(?<name>([^<]+)(?:<br/>)?([^>]+))</a>",
                    Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
            Matcher nameMatcher = namePattern.matcher(html);
            String shortName = null;
            String mainName = null;
            String secondaryName = null;
            if (nameMatcher.find()) {
                String afterId = nameMatcher.group("name").trim();
                afterId = afterId.replace("<br/>", " ");
                if (afterId.startsWith("[")) {
                    int end = afterId.indexOf(']');
                    if (end > 1) {
                        shortName = afterId.substring(1, end).trim();
                        afterId = afterId.substring(end + 1).trim();
                    }
                }

                mainName = afterId;
                int parenStart = afterId.lastIndexOf('(');
                if (parenStart >= 0 && afterId.endsWith(")")) {
                    secondaryName = afterId.substring(parenStart + 1, afterId.length() - 1).trim();
                    mainName = afterId.substring(0, parenStart).trim();
                }
            }

            // 提取封面图片
            Pattern coverPattern = Pattern.compile("<img[^>]*src=\"([^\"]+)\"[^>]*type-original",
                    Pattern.CASE_INSENSITIVE);
            Matcher coverMatcher = coverPattern.matcher(html);
            String coverImageUrl = null;
            if (coverMatcher.find()) {
                coverImageUrl = coverMatcher.group(1);
                if (coverImageUrl.startsWith("//")) {
                    coverImageUrl = "https:" + coverImageUrl;
                }
            }

            // 提取浏览数、推荐数、收藏数
            Pattern numPattern = Pattern.compile("<div[^>]*title=\"([^\"]+)\"[^>]*>.*?<i></i>([^<]+)</div>",
                    Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
            Matcher numMatcher = numPattern.matcher(html);
            String viewCount = null;
            String recommendCount = null;
            String favoriteCount = null;
            while (numMatcher.find()) {
                String title = numMatcher.group(1);
                String value = numMatcher.group(2).trim();
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
            Pattern itemsPattern = Pattern.compile("<div class=\"items\">((?:<li[^>]*>.*?</li>)*)</div>",
                    Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
            Matcher itemsMatcher = itemsPattern.matcher(html);
            if (itemsMatcher.find()) {
                String itemsHtml = itemsMatcher.group(1);
                // 匹配 <li iid="xxx"> 格式的物品
                Pattern itemPattern = Pattern.compile("<li[^>]*iid=\"(\\d+)\"[^>]*>.*?<a[^>]*href=\"([^\"]+)\"[^>]*>.*?<img[^>]*alt=\"([^\"]+)\"[^>]*src=\"([^\"]+)\"",
                        Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
                Matcher itemMatcher = itemPattern.matcher(itemsHtml);
                while (itemMatcher.find()) {
                    try {
                        long itemId = Long.parseLong(itemMatcher.group(1));
                        String itemUrl = itemMatcher.group(2);
                        String itemName = itemMatcher.group(3);
                        String iconUrl = itemMatcher.group(4);

                        // 处理相对 URL
                        if (iconUrl.startsWith("//")) {
                            iconUrl = "https:" + iconUrl;
                        } else if (iconUrl.startsWith("/")) {
                            iconUrl = "https://www.mcmod.cn" + iconUrl;
                        }

                        if (itemUrl.startsWith("//")) {
                            itemUrl = "https:" + itemUrl;
                        } else if (itemUrl.startsWith("/")) {
                            itemUrl = "https://www.mcmod.cn" + itemUrl;
                        }

                        items.add(new McModIndexCategoryItem(itemId, itemName, iconUrl, itemUrl));
                    } catch (Exception e) {
                        LOGGER.warn("解析物品项异常: {}", e.getMessage());
                    }
                }
            }

            return new McModIndexCategoryModFrame(modId, shortName, mainName, secondaryName, coverImageUrl,
                    viewCount, recommendCount, favoriteCount, items);
        } catch (Exception e) {
            LOGGER.error("解析模组卡片 HTML 异常", e);
            return null;
        }
    }

    /**
     * 解析右侧排行榜 HTML
     */
    private static McModIndexCategoryRight parseIndexCategoryRightHtml(String html) {
        List<McModIndexCategoryRankItem> dayRank = parseIndexCategoryRankList(html, "day");
        List<McModIndexCategoryRankItem> weekRank = parseIndexCategoryRankList(html, "week");
        List<McModIndexCategoryRankItem> monthRank = parseIndexCategoryRankList(html, "moon");

        return new McModIndexCategoryRight(dayRank, weekRank, monthRank);
    }

    /**
     * 解析排行榜列表
     */
    private static List<McModIndexCategoryRankItem> parseIndexCategoryRankList(String html, String rankType) {
        List<McModIndexCategoryRankItem> rankList = new ArrayList<>();
        Pattern rankListPattern = Pattern.compile("<ul id=\"" + rankType + "\"[^>]*>(.*?)</ul>",
                Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
        Matcher rankListMatcher = rankListPattern.matcher(html);
        if (!rankListMatcher.find()) {
            return rankList;
        }

        String rankListHtml = rankListMatcher.group(1);

        // 先匹配所有带图片的排行榜项（class="no1"）
        Pattern rankItemPattern = Pattern.compile("<li[^>]*class=\"no1\"[^>]*>(.*?)</li>",
                Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
        Matcher rankItemMatcher = rankItemPattern.matcher(rankListHtml);

        while (rankItemMatcher.find()) {
            try {
                String itemHtml = rankItemMatcher.group(1);

                // 提取排名
                Pattern rankPattern = Pattern.compile("<i class=\"n\\d+\">(\\d+)</i>");
                Matcher rankMatcher = rankPattern.matcher(itemHtml);
                int rank = 0;
                if (rankMatcher.find()) {
                    rank = Integer.parseInt(rankMatcher.group(1));
                }

                // 提取模组ID
                Pattern modIdPattern = Pattern.compile("<a[^>]*href=\"/class/(\\d+)\\.html\"");
                Matcher modIdMatcher = modIdPattern.matcher(itemHtml);
                long modId = 0;
                if (modIdMatcher.find()) {
                    modId = Long.parseLong(modIdMatcher.group(1));
                }

                // 提取封面图片
                Pattern coverPattern = Pattern.compile("<img[^>]*src=\"([^\"]+)\"");
                Matcher coverMatcher = coverPattern.matcher(itemHtml);
                String coverImageUrl = null;
                if (coverMatcher.find()) {
                    coverImageUrl = coverMatcher.group(1);
                    if (coverImageUrl.startsWith("//")) {
                        coverImageUrl = "https:" + coverImageUrl;
                    }
                }

                // 提取模组名称（主名称和英文名）
                Pattern namePattern = Pattern.compile("<a[^>]*title=\"([^\"]+)\"[^>]*>(?<name>([^<]+)(?:<br/>)?([^>]+))</a>");
                Matcher nameMatcher = namePattern.matcher(itemHtml);
                String shortName = null;
                String mainName = null;
                String secondaryName = null;
                if (nameMatcher.find()) {
                    String afterId = nameMatcher.group("name").trim();
                    afterId = afterId.replace("<br/>", " ");
                    if (afterId.startsWith("[")) {
                        int end = afterId.indexOf(']');
                        if (end > 1) {
                            shortName = afterId.substring(1, end).trim();
                            afterId = afterId.substring(end + 1).trim();
                        }
                    }

                    mainName = afterId;
                    int parenStart = afterId.lastIndexOf('(');
                    if (parenStart >= 0 && afterId.endsWith(")")) {
                        secondaryName = afterId.substring(parenStart + 1, afterId.length() - 1).trim();
                        mainName = afterId.substring(0, parenStart).trim();
                    }
                }

                // 提取指数值
                Pattern indexPattern = Pattern.compile("<span[^>]*title=\"([^\"]+)\"[^>]*>.*?<i></i>([^<]+)</span>",
                        Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
                Matcher indexMatcher = indexPattern.matcher(itemHtml);
                String indexValue = null;
                if (indexMatcher.find()) {
                    indexValue = indexMatcher.group(2).trim();
                }

                if (modId > 0 && mainName != null) {
                    rankList.add(new McModIndexCategoryRankItem(rank, modId, shortName, mainName, secondaryName, coverImageUrl, indexValue));
                }
            } catch (Exception e) {
                LOGGER.warn("解析排行榜项异常: {}", e.getMessage());
            }
        }

        return rankList;
    }

    // endregion private

    public static @Nonnull String getUrl(EnumCommentType type, String containerId) {
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
            OtherConfig otherConfig = BaniraUtils.getOthersConfig(groupId);
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
    public static @Nonnull List<McModSearchResult> search(EnumSearchType type, String key) {
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
            LOGGER.error("搜索异常，类型: {}, 关键词: {}", type, key, e);
            return List.of();
        }
    }

    /**
     * 搜索mod
     *
     * @param key 关键词
     * @return 搜索结果列表
     */
    public static @Nonnull List<McModSearchResult> searchMod(String key) {
        return search(EnumSearchType.MOD, key);
    }

    /**
     * 搜索整合包
     *
     * @param key 关键词
     * @return 搜索结果列表
     */
    public static @Nonnull List<McModSearchResult> searchModpack(String key) {
        return search(EnumSearchType.MODPACK, key);
    }

    /**
     * 搜索作者
     *
     * @param key 关键词
     * @return 搜索结果列表
     */
    public static @Nonnull List<McModSearchResult> searchAuthor(String key) {
        return search(EnumSearchType.AUTHOR, key);
    }

    public static McModSearchResult getModName(String modId) {
        List<McModSearchResult> result = searchMod(modId);
        return result.size() == 1 && String.valueOf(result.getFirst().getModId()).equals(modId) ? result.getFirst() : null;
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
            LOGGER.error("获取首页模组列表异常，分类: {}", category, e);
            return null;
        }
    }

    /**
     * 获取评论列表
     *
     * @param type 类型
     * @param doid 对象ID
     * @param page 页码, 从1开始
     */
    public static McModCommentResult getComments(EnumCommentType type, String doid, Integer page) {
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
    public static McModCommentResult getComments(EnumCommentType type, String channel, String doid, Integer page, boolean self) {
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
            LOGGER.error("获取评论异常", e);
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
            LOGGER.error("获取评论回复异常", e);
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
    public static McModCommentResponse comment(Long groupId, EnumCommentType type, String container, String text) {
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
    public static McModCommentResponse comment(Long groupId, EnumCommentType type, String channel, String container, String text, Integer quote) {
        try {
            String cookie = getCookie(groupId);
            if (cookie == null) {
                LOGGER.error("无法获取 Cookie，评论失败，groupId: {}", groupId);
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
            LOGGER.error("评论异常", e);
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
    public static McModCommentResponse replyComment(Long groupId, EnumCommentType type, String container, String commentId, String text) {
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
    public static McModCommentResponse replyComment(Long groupId, EnumCommentType type, String channel, String container, String commentId, String text) {
        try {
            String cookie = getCookie(groupId);
            if (cookie == null) {
                LOGGER.error("无法获取 Cookie，回复评论失败，groupId: {}", groupId);
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
            LOGGER.error("回复评论异常", e);
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
                LOGGER.error("无法获取 Cookie，删除评论失败，groupId: {}", groupId);
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
            LOGGER.error("删除评论异常", e);
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
                LOGGER.error("无法获取 Cookie，上传文件失败，groupId: {}", groupId);
                return null;
            }

            File file = new File(filePath);
            if (!file.exists() || !file.isFile()) {
                LOGGER.error("文件不存在: {}", filePath);
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
            LOGGER.error("上传文件异常", e);
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
                LOGGER.error("无法获取 Cookie，删除文件失败，groupId: {}", groupId);
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
            LOGGER.error("删除文件异常", e);
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
            return com.mikuac.shiro.common.utils.JsonUtils.readValue(xin.vanilla.banira.util.JsonUtils.toJsonString(data), new TypeReference<>() {
            });
        } catch (Exception e) {
            LOGGER.error("获取用户卡片异常", e);
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
            OtherConfig otherConfig = BaniraUtils.getOthersConfig(groupId);
            if (otherConfig != null && otherConfig.mcModCookieConfig() != null) {
                otherConfig.mcModCookieConfig().cookie(null).expireTime(null).userId(null);
                BaniraUtils.saveGroupConfig();
                LOGGER.info("Cookie 缓存已清除，groupId: {}", groupId);
            }
        } finally {
            lock.unlock();
        }
    }

}
