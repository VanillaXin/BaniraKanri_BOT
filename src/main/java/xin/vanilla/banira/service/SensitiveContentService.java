package xin.vanilla.banira.service;

import com.github.houbb.sensitive.word.bs.SensitiveWordBs;
import com.github.houbb.sensitive.word.support.check.WordChecks;
import com.mikuac.shiro.common.utils.MessageConverser;
import com.mikuac.shiro.enums.MsgTypeEnum;
import com.mikuac.shiro.model.ArrayMsg;
import jakarta.annotation.PostConstruct;
import org.springframework.stereotype.Service;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.node.ObjectNode;
import xin.vanilla.banira.service.support.PlantCipherWordReplace;
import xin.vanilla.banira.service.support.SensitiveTextProtector;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * 出站敏感内容处理：结构感知 + 占位符保护，替换仍使用 PlantCipher。
 */
@Service
public class SensitiveContentService {

    private static volatile SensitiveContentService instance;

    private final SensitiveWordBs wordBs;

    public SensitiveContentService() {
        this.wordBs = buildWordBs();
    }

    public static SensitiveContentService getInstance() {
        SensitiveContentService ref = instance;
        return ref != null ? ref : Holder.FALLBACK;
    }

    @PostConstruct
    void registerInstance() {
        instance = this;
    }

    // region 对外 API

    /**
     * 纯文本脱敏（保护 URL / CQ / Plant 密文后再扫描）。
     */
    public String replacePlainText(String text) {
        if (StringUtils.isNullOrEmpty(text)) {
            return text;
        }
        SensitiveTextProtector.ProtectResult protectedResult = SensitiveTextProtector.protect(text);
        String replaced = wordBs.replace(protectedResult.protectedText());
        return SensitiveTextProtector.restore(replaced, protectedResult.segments());
    }

    /**
     * CQ 消息字符串：解析为消息链后仅处理 text 段。
     */
    public String replaceMessageString(String message) {
        if (StringUtils.isNullOrEmpty(message)) {
            return message;
        }
        List<ArrayMsg> segments = MessageConverser.stringToArray(message);
        replaceArrayMsg(segments);
        return MessageConverser.arraysToString(segments);
    }

    public List<ArrayMsg> replaceArrayMsg(List<ArrayMsg> arrayMsgList) {
        if (CollectionUtils.isNullOrEmpty(arrayMsgList)) {
            return arrayMsgList;
        }
        arrayMsgList.forEach(this::replaceTextSegment);
        return arrayMsgList;
    }

    /**
     * 合并转发节点列表：仅处理 node.data.content。
     */
    public List<Map<String, Object>> replaceForwardNodes(List<Map<String, Object>> nodes) {
        if (CollectionUtils.isNullOrEmpty(nodes)) {
            return nodes;
        }
        List<Map<String, Object>> result = new ArrayList<>(nodes.size());
        for (Map<String, Object> node : nodes) {
            result.add(replaceForwardNode(node));
        }
        return result;
    }

    /**
     * 合并转发外显摘要：仅处理 text 字段。
     */
    public List<Map<String, String>> replaceForwardNews(List<Map<String, String>> news) {
        if (CollectionUtils.isNullOrEmpty(news)) {
            return news;
        }
        List<Map<String, String>> result = new ArrayList<>(news.size());
        for (Map<String, String> item : news) {
            Map<String, String> copy = new LinkedHashMap<>(item);
            if (copy.containsKey("text")) {
                copy.put("text", replacePlainText(copy.get("text")));
            }
            result.add(copy);
        }
        return result;
    }

    /**
     * 上传文件名不做敏感替换，避免破坏路径与扩展名。
     */
    public String replaceFileName(String fileName) {
        return fileName;
    }

    @SuppressWarnings("unchecked")
    public Object replaceObject(Object obj) {
        if (obj == null) {
            return null;
        }
        if (obj instanceof String str) {
            return replacePlainText(str);
        }
        if (obj instanceof List<?> list) {
            if (list.isEmpty()) {
                return list;
            }
            Object first = list.get(0);
            if (first instanceof Map<?, ?> map && isForwardNode(map)) {
                return replaceForwardNodes((List<Map<String, Object>>) obj);
            }
            if (first instanceof Map<?, ?>) {
                return replaceForwardNews((List<Map<String, String>>) obj);
            }
            return list.stream()
                    .map(this::replaceObject)
                    .collect(Collectors.toList());
        }
        if (obj instanceof ArrayMsg arrayMsg) {
            replaceTextSegment(arrayMsg);
            return arrayMsg;
        }
        return obj;
    }

    // endregion 对外 API

    // region 内部实现

    private Map<String, Object> replaceForwardNode(Map<String, Object> node) {
        if (node == null || node.isEmpty()) {
            return node;
        }
        Map<String, Object> copy = new LinkedHashMap<>(node);
        if (!"node".equalsIgnoreCase(String.valueOf(copy.get("type")))) {
            return copy;
        }
        Object dataObj = copy.get("data");
        if (!(dataObj instanceof Map<?, ?> rawData)) {
            return copy;
        }
        Map<String, Object> data = new LinkedHashMap<>();
        rawData.forEach((key, value) -> {
            if ("content".equalsIgnoreCase(String.valueOf(key)) && value instanceof String content) {
                data.put(String.valueOf(key), replaceMessageString(content));
            } else {
                data.put(String.valueOf(key), value);
            }
        });
        copy.put("data", data);
        return copy;
    }

    private void replaceTextSegment(ArrayMsg arrayMsg) {
        if (arrayMsg == null || arrayMsg.getType() != MsgTypeEnum.text) {
            return;
        }
        String text = arrayMsg.getStringData(MsgTypeEnum.text.toString());
        arrayMsg.setData(java.util.Map.of(MsgTypeEnum.text.toString(), replacePlainText(text)));
        JsonNode node = arrayMsg.getData();
        if (node instanceof ObjectNode objectNode) {
            JsonNode jsonNode = node.get(MsgTypeEnum.text.toString());
            if (jsonNode != null && jsonNode.isNumber()) {
                objectNode.put(MsgTypeEnum.text.toString(), jsonNode.asText());
            }
        }
    }

    private static boolean isForwardNode(Map<?, ?> map) {
        return map.containsKey("type") && map.containsKey("data");
    }

    private static SensitiveWordBs buildWordBs() {
        return SensitiveWordBs.newInstance()
                .ignoreCase(true)
                .ignoreWidth(true)
                .ignoreNumStyle(true)
                .ignoreChineseStyle(true)
                .ignoreEnglishStyle(true)
                .ignoreRepeat(false)
                .enableNumCheck(false)
                .enableEmailCheck(false)
                .enableUrlCheck(false)
                .enableIpv4Check(false)
                .enableWordCheck(true)
                .wordFailFast(true)
                .wordCheckNum(WordChecks.num())
                .wordCheckEmail(WordChecks.email())
                .wordCheckUrl(WordChecks.url())
                .wordCheckIpv4(WordChecks.ipv4())
                .wordCheckWord(WordChecks.word())
                .wordReplace(new PlantCipherWordReplace())
                .init();
    }

    private static final class Holder {
        private static final SensitiveContentService FALLBACK = new SensitiveContentService();

        private Holder() {
        }
    }

    // endregion 内部实现

}
