package xin.vanilla.banira.plugin.socialmedia;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

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

    {
        this.parser = "";
        this.api = "";
        this.fields = new SocialMediaApiFieldSettings();
    }
}
