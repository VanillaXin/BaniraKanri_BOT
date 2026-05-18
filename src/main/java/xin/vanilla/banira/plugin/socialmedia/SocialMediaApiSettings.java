package xin.vanilla.banira.plugin.socialmedia;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.Map;

/**
 * 社交媒体接口配置
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class SocialMediaApiSettings {

    /**
     * 解析器类型（如 bilibili、douyin）
     */
    private String parser;

    /**
     * 请求地址，支持 %s 占位符（会填入 URL 编码后的目标链接）
     */
    private String api;

    /**
     * 返回 JSON 字段映射
     */
    private SocialMediaApiFieldSettings fields;
    /**
     * 请求头
     */
    private Map<String, String> headers;
    /**
     * 请求目标转换模式：url | awemeId
     */
    private String targetMode;

    {
        this.parser = "";
        this.api = "";
        this.fields = new SocialMediaApiFieldSettings();
        this.headers = BaniraUtils.mutableMapOf();
        this.targetMode = "url";
    }
}
