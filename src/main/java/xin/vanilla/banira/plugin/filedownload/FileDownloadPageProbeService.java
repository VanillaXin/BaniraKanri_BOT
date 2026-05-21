package xin.vanilla.banira.plugin.filedownload;

import com.google.gson.JsonElement;
import com.mikuac.shiro.common.utils.ShiroUtils;
import com.mikuac.shiro.dto.action.common.ActionData;
import com.mikuac.shiro.dto.action.common.MsgId;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import xin.vanilla.banira.config.other.OtherConfigRegistry;
import xin.vanilla.banira.domain.KeyValue;
import xin.vanilla.banira.plugin.common.BaniraBot;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.JsonUtils;
import xin.vanilla.banira.util.StringUtils;

import java.net.URI;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 下载页探测与选择列表发送
 */
@Slf4j
@Service
public class FileDownloadPageProbeService {

    private static final String MODE_JSON_API = "json-api";
    private static final String MODE_HTML_LINK = "html-link";

    private final OtherConfigRegistry otherConfigRegistry;
    private final FileDownloadSelectionStore selectionStore;
    private final FileDownloadService fileDownloadService;
    private final FileDownloadHttpSupport fileDownloadHttpSupport;

    public FileDownloadPageProbeService(OtherConfigRegistry otherConfigRegistry,
                                        FileDownloadSelectionStore selectionStore,
                                        FileDownloadService fileDownloadService,
                                        FileDownloadHttpSupport fileDownloadHttpSupport) {
        this.otherConfigRegistry = otherConfigRegistry;
        this.selectionStore = selectionStore;
        this.fileDownloadService = fileDownloadService;
        this.fileDownloadHttpSupport = fileDownloadHttpSupport;
    }

    /**
     * 判断 URL 是否命中任一启用的下载页规则
     */
    public boolean matchesPage(String url) {
        return findRule(resolvePageUrl(url)) != null;
    }

    /**
     * 按配置规则规范化下载页 URL
     */
    public String resolvePageUrl(String url) {
        if (StringUtils.isNullOrEmptyEx(url)) {
            return url;
        }
        String current = url.trim();
        for (FileDownloadUrlRewriteRule rule : settings().urlRewriteRules()) {
            if (rule == null || !rule.enabled() || StringUtils.isNullOrEmptyEx(rule.matchPattern())) {
                continue;
            }
            Matcher matcher = Pattern.compile(rule.matchPattern()).matcher(current);
            if (!matcher.matches() || StringUtils.isNullOrEmptyEx(rule.replaceTemplate())) {
                continue;
            }
            current = applyCaptureTemplate(rule.replaceTemplate(), matcher);
        }
        return current;
    }

    /**
     * 异步探测下载页并发送选择列表
     *
     * @return true 表示已受理
     */
    public boolean submitProbe(BaniraBot bot, AnyMessageEvent event, String pageUrl) {
        String resolvedUrl = resolvePageUrl(pageUrl);
        return fileDownloadService.runWithTaskSlot(() -> probeAndSendSelection(bot, event, resolvedUrl));
    }

    // region 探测流程

    private void probeAndSendSelection(BaniraBot bot, AnyMessageEvent event, String pageUrl) {
        FileDownloadSettings settings = settings();
        try {
            KeyValue<FileDownloadPageRule, Matcher> matched = findRuleWithMatcher(pageUrl);
            if (matched == null) {
                notifyFail(bot, event, "未找到匹配的下载页规则");
                return;
            }
            List<FileDownloadCandidate> candidates = probeCandidates(pageUrl, matched.getKey(), matched.getValue());
            candidates = distinctCandidates(candidates);
            if (CollectionUtils.isNullOrEmpty(candidates)) {
                notifyFail(bot, event, "未能从该下载页解析到可下载文件");
                return;
            }
            if (candidates.size() == 1) {
                fileDownloadService.downloadInCurrentTask(bot, event, candidates.getFirst().url());
                return;
            }
            sendSelectionForward(bot, event, pageUrl, candidates, settings.selectionExpireSeconds());
        } catch (Exception e) {
            LOGGER.error("Failed to probe download page: {}", pageUrl, e);
            notifyFail(bot, event, "解析下载页失败，" + e.getMessage());
        }
    }

    private List<FileDownloadCandidate> probeCandidates(String pageUrl, FileDownloadPageRule rule, Matcher matcher) {
        String mode = normalizeMode(rule.mode());
        if (MODE_HTML_LINK.equals(mode)) {
            return probeHtmlLinks(pageUrl, rule);
        }
        return probeJsonApi(pageUrl, rule, matcher);
    }

    // endregion 探测流程

    // region JSON API

    private List<FileDownloadCandidate> probeJsonApi(String pageUrl, FileDownloadPageRule rule, Matcher matcher) {
        String apiUrl = resolveApiUrl(rule, matcher);
        if (StringUtils.isNullOrEmptyEx(apiUrl)) {
            return List.of();
        }
        KeyValue<String, String>[] headers = toHeaderArray(rule.headers());
        String response = fileDownloadHttpSupport.getString(apiUrl, headers);
        JsonElement json = JsonUtils.parseJson(response);
        if (json == null || json.isJsonNull()) {
            return List.of();
        }
        List<FileDownloadCandidate> candidates = new ArrayList<>();
        if (StringUtils.isNotNullOrEmpty(rule.itemsPath())) {
            JsonElement itemsElement = JsonUtils.getJsonElement(json, rule.itemsPath());
            if (itemsElement != null && itemsElement.isJsonArray()) {
                candidates.addAll(parseJsonItemArray(pageUrl, rule, itemsElement.getAsJsonArray()));
            }
        }
        if (CollectionUtils.isNotNullOrEmpty(rule.extraItems())) {
            for (FileDownloadJsonItemMapping extraItem : rule.extraItems()) {
                FileDownloadCandidate candidate = parseJsonExtraItem(pageUrl, json, extraItem);
                if (candidate != null) {
                    candidates.add(candidate);
                }
            }
        }
        return candidates;
    }

    private List<FileDownloadCandidate> parseJsonItemArray(String pageUrl, FileDownloadPageRule rule,
                                                           com.google.gson.JsonArray items) {
        List<FileDownloadCandidate> candidates = new ArrayList<>();
        for (JsonElement itemElement : items) {
            if (itemElement == null || itemElement.isJsonNull()) {
                continue;
            }
            FileDownloadCandidate candidate = parseJsonItem(pageUrl, rule.urlPath(), rule.namePath(),
                    rule.sizePath(), itemElement);
            if (candidate != null) {
                candidates.add(candidate);
            }
        }
        return candidates;
    }

    private FileDownloadCandidate parseJsonExtraItem(String pageUrl, JsonElement json, FileDownloadJsonItemMapping mapping) {
        if (mapping == null || StringUtils.isNullOrEmptyEx(mapping.urlPath())) {
            return null;
        }
        String url = JsonUtils.getString(json, mapping.urlPath(), "");
        if (StringUtils.isNullOrEmptyEx(url)) {
            return null;
        }
        url = resolveAbsoluteUrl(pageUrl, url);
        String name = mapping.name();
        if (StringUtils.isNullOrEmptyEx(name)) {
            name = fileDownloadService.extractDisplayNameFromUrl(url);
        }
        long size = StringUtils.isNullOrEmptyEx(mapping.sizePath())
                ? -1L
                : JsonUtils.getLong(json, mapping.sizePath(), -1L);
        return new FileDownloadCandidate()
                .name(name)
                .url(url)
                .size(size);
    }

    private FileDownloadCandidate parseJsonItem(String pageUrl, String urlPath, String namePath, String sizePath,
                                                JsonElement itemElement) {
        String url = JsonUtils.getString(itemElement, urlPath, "");
        if (StringUtils.isNullOrEmptyEx(url)) {
            return null;
        }
        url = resolveAbsoluteUrl(pageUrl, url);
        String name = JsonUtils.getString(itemElement, namePath, "");
        if (StringUtils.isNullOrEmptyEx(name)) {
            name = fileDownloadService.extractDisplayNameFromUrl(url);
        }
        long size = StringUtils.isNullOrEmptyEx(sizePath)
                ? -1L
                : JsonUtils.getLong(itemElement, sizePath, -1L);
        return new FileDownloadCandidate()
                .name(name)
                .url(url)
                .size(size);
    }

    private String resolveApiUrl(FileDownloadPageRule rule, Matcher matcher) {
        if (shouldUseFallbackApi(rule, matcher)) {
            return applyCaptureTemplate(rule.apiUrlTemplateFallback(), matcher);
        }
        return applyCaptureTemplate(rule.apiUrlTemplate(), matcher);
    }

    private boolean shouldUseFallbackApi(FileDownloadPageRule rule, Matcher matcher) {
        if (StringUtils.isNullOrEmptyEx(rule.apiUrlTemplateFallback())) {
            return false;
        }
        int groupIndex = rule.fallbackWhenEmptyGroup();
        if (groupIndex <= 0 || groupIndex > matcher.groupCount()) {
            return false;
        }
        return StringUtils.isNullOrEmptyEx(matcher.group(groupIndex));
    }

    // endregion JSON API

    // region HTML

    private List<FileDownloadCandidate> probeHtmlLinks(String pageUrl, FileDownloadPageRule rule) {
        if (StringUtils.isNullOrEmptyEx(rule.htmlLinkPattern())) {
            return List.of();
        }
        KeyValue<String, String>[] headers = toHeaderArray(rule.headers());
        String html = fileDownloadHttpSupport.getString(pageUrl, headers);
        if (StringUtils.isNullOrEmptyEx(html)) {
            return List.of();
        }
        Pattern pattern = Pattern.compile(rule.htmlLinkPattern(), Pattern.CASE_INSENSITIVE);
        Matcher matcher = pattern.matcher(html);
        List<FileDownloadCandidate> candidates = new ArrayList<>();
        while (matcher.find()) {
            if (matcher.groupCount() < 1) {
                continue;
            }
            String url = matcher.group(1);
            String name = matcher.groupCount() >= 2 ? matcher.group(2) : "";
            if (StringUtils.isNullOrEmptyEx(url)) {
                continue;
            }
            url = resolveAbsoluteUrl(pageUrl, url.trim());
            name = StringUtils.isNullOrEmptyEx(name) ? fileDownloadService.extractDisplayNameFromUrl(url) : name.trim();
            candidates.add(new FileDownloadCandidate()
                    .name(name)
                    .url(url)
                    .size(-1L));
        }
        return candidates;
    }

    // endregion HTML

    // region 选择列表

    private void sendSelectionForward(BaniraBot bot, AnyMessageEvent event, String pageUrl,
                                      List<FileDownloadCandidate> candidates, long expireSeconds) {
        List<Map<String, Object>> forwardMsg = buildSelectionForward(bot, pageUrl, candidates);
        ActionData<MsgId> msgIdData = bot.sendForwardMsg(event, forwardMsg);
        if (!bot.isActionDataMsgIdNotEmpty(msgIdData)) {
            notifyFail(bot, event, "发送文件选择列表失败");
            return;
        }
        Integer messageId = bot.getActionDataMsgId(msgIdData);
        long expireAt = System.currentTimeMillis() + TimeUnit.SECONDS.toMillis(Math.max(30L, expireSeconds));
        selectionStore.save(new FileDownloadSelectionSession()
                .messageId(messageId.longValue())
                .userId(event.getUserId())
                .groupId(event.getGroupId())
                .sourceUrl(pageUrl)
                .candidates(new ArrayList<>(candidates))
                .expireAt(expireAt));
        FileDownloadFeedback.accept(bot, event.getMessageId());
    }

    private List<Map<String, Object>> buildSelectionForward(BaniraBot bot, String pageUrl, List<FileDownloadCandidate> candidates) {
        Long botId = bot.getSelfId();
        String botName = bot.getLoginInfoEx().getNickname();
        List<Map<String, Object>> forwardMsg = new ArrayList<>();
        forwardMsg.add(ShiroUtils.generateSingleMsg(botId, botName,
                "下载页文件列表\n" + pageUrl + "\n\n回复本消息并发送序号以下载对应文件。"));
        int index = 1;
        for (FileDownloadCandidate candidate : candidates) {
            forwardMsg.add(ShiroUtils.generateSingleMsg(botId, botName, formatCandidateLine(index++, candidate)));
        }
        return forwardMsg;
    }

    private String formatCandidateLine(int index, FileDownloadCandidate candidate) {
        StringBuilder sb = new StringBuilder();
        sb.append(index).append(". ").append(candidate.name());
        if (candidate.size() > 0) {
            sb.append(" (").append(fileDownloadService.formatSize(candidate.size())).append(")");
        }
        sb.append("\n").append(candidate.url());
        return sb.toString();
    }

    // endregion 选择列表

    // region 规则匹配

    private FileDownloadPageRule findRule(String url) {
        KeyValue<FileDownloadPageRule, Matcher> matched = findRuleWithMatcher(url);
        return matched != null ? matched.getKey() : null;
    }

    private KeyValue<FileDownloadPageRule, Matcher> findRuleWithMatcher(String url) {
        if (StringUtils.isNullOrEmptyEx(url)) {
            return null;
        }
        for (FileDownloadPageRule rule : settings().pageRules()) {
            if (rule == null || !rule.enabled() || StringUtils.isNullOrEmptyEx(rule.pageUrlPattern())) {
                continue;
            }
            Matcher matcher = Pattern.compile(rule.pageUrlPattern()).matcher(url.trim());
            if (matcher.matches()) {
                return new KeyValue<>(rule, matcher);
            }
        }
        return null;
    }

    private String normalizeMode(String mode) {
        if (StringUtils.isNullOrEmptyEx(mode)) {
            return MODE_JSON_API;
        }
        return mode.trim().toLowerCase();
    }

    // endregion 规则匹配

    // region 工具方法

    private FileDownloadSettings settings() {
        return otherConfigRegistry.getShared(FileDownloadSettings.class);
    }

    private List<FileDownloadCandidate> distinctCandidates(List<FileDownloadCandidate> candidates) {
        Map<String, FileDownloadCandidate> unique = new LinkedHashMap<>();
        for (FileDownloadCandidate candidate : candidates) {
            if (candidate == null || StringUtils.isNullOrEmptyEx(candidate.url())) {
                continue;
            }
            unique.putIfAbsent(candidate.url(), candidate);
        }
        return new ArrayList<>(unique.values());
    }

    private String applyCaptureTemplate(String template, Matcher matcher) {
        String result = template;
        for (int i = 1; i <= matcher.groupCount(); i++) {
            String value = matcher.group(i);
            result = result.replace("$" + i, value == null ? "" : value);
        }
        return result;
    }

    private String resolveAbsoluteUrl(String baseUrl, String link) {
        if (StringUtils.isNullOrEmptyEx(link)) {
            return link;
        }
        if (link.startsWith("http://") || link.startsWith("https://")) {
            return link;
        }
        try {
            return URI.create(baseUrl).resolve(link).toString();
        } catch (Exception e) {
            return link;
        }
    }

    private KeyValue<String, String>[] toHeaderArray(Map<String, String> headers) {
        if (headers == null || headers.isEmpty()) {
            return null;
        }
        List<KeyValue<String, String>> list = new ArrayList<>();
        headers.forEach((key, value) -> {
            if (StringUtils.isNotNullOrEmpty(key) && value != null) {
                list.add(new KeyValue<>(key, value));
            }
        });
        return list.isEmpty() ? null : list.toArray(new KeyValue[0]);
    }

    private void notifyFail(BaniraBot bot, AnyMessageEvent event, String message) {
        FileDownloadFeedback.fail(bot, event, message);
    }

    // endregion 工具方法

}
