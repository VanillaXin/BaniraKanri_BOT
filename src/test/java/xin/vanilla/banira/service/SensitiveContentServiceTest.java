package xin.vanilla.banira.service;

import com.mikuac.shiro.common.utils.MessageConverser;
import com.mikuac.shiro.model.ArrayMsg;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import xin.vanilla.banira.util.PlantCipher;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

class SensitiveContentServiceTest {

    private final SensitiveContentService service = SensitiveContentService.getInstance();

    @Test
    void shouldNotModifyUrlInPlainText() {
        String url = "https://example.com/path/to/resource";
        String result = service.replacePlainText("下载链接 " + url + " 请查看");
        Assertions.assertTrue(result.contains(url), "URL 应原样保留: " + result);
    }

    @Test
    void shouldNotModifyImageCqInMessageString() {
        String cq = "[CQ:image,file=abc123.jpg,url=https://cdn.example.com/a.jpg]";
        String message = "看图 " + cq;
        String result = service.replaceMessageString(message);
        Assertions.assertEquals(message, result);
    }

    @Test
    void shouldOnlyProcessForwardNodeContent() {
        String imageCq = "[CQ:image,file=test.png,url=https://cdn.example.com/test.png]";
        Map<String, Object> node = new LinkedHashMap<>();
        node.put("type", "node");
        Map<String, Object> data = new LinkedHashMap<>();
        data.put("uin", "12345");
        data.put("name", "测试");
        data.put("content", imageCq);
        node.put("data", data);

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> result = service.replaceForwardNodes(List.of(node));
        Map<String, Object> outData = (Map<String, Object>) result.get(0).get("data");
        Assertions.assertEquals(imageCq, outData.get("content"));
        Assertions.assertEquals("12345", outData.get("uin"));
    }

    @Test
    void shouldEncodeSensitiveWordInTextSegment() {
        String sensitive = SensitiveContentService.getInstance()
                .replacePlainText("这是一段测试敏感词08宪章的文字");
        Assertions.assertFalse(sensitive.contains("08宪"));
        Assertions.assertTrue(sensitive.length() > 10);
    }

    @Test
    void shouldKeepDailyProfanityUntouched() {
        String text = "他妈的这也太离谱了，卧槽";
        Assertions.assertEquals(text, service.replacePlainText(text));
    }

    @Test
    void shouldKeepFileNameUntouched() {
        Assertions.assertEquals("report.pdf", service.replaceFileName("report.pdf"));
    }

    @Test
    void shouldPreserveExistingPlantToken() {
        String token = PlantCipher.encode("hello");
        String wrapped = "阁下请喝" + token + "茶";
        String result = service.replacePlainText(wrapped);
        Assertions.assertEquals(wrapped, result);
    }

    @Test
    void shouldProcessArrayMsgTextOnly() {
        String cq = "[CQ:image,file=x.png,url=https://a.com/x.png]";
        List<ArrayMsg> segments = MessageConverser.stringToArray("前缀 " + cq);
        service.replaceArrayMsg(segments);
        String rebuilt = MessageConverser.arraysToString(segments);
        Assertions.assertTrue(rebuilt.contains(cq));
    }

}
