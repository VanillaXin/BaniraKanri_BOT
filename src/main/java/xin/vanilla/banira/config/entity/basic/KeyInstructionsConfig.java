package xin.vanilla.banira.config.entity.basic;


import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import xin.vanilla.banira.domain.KeyValue;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.List;

/**
 * 关键词指令配置
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class KeyInstructionsConfig {

    /**
     * 前缀 与 后缀
     */
    private List<KeyValue<String, String>> locator;
    /**
     * 精确匹配
     */
    private List<String> exactly;
    /**
     * 包含匹配
     */
    private List<String> contain;
    /**
     * 拼音匹配
     */
    private List<String> pinyin;
    /**
     * 正则匹配
     */
    private List<String> regex;


    {
        this.locator = BaniraUtils.mutableListOf(
                new KeyValue<>("关键词", "回复")
                , new KeyValue<>("key", "rep")
                , new KeyValue<>("keyword", "reply")
        );
        this.exactly = BaniraUtils.mutableListOf("完全", "精准", "exactly", "perfect", "per");
        this.contain = BaniraUtils.mutableListOf("包含", "contain", "include", "inc");
        this.pinyin = BaniraUtils.mutableListOf("拼音", "pinyin", "pin");
        this.regex = BaniraUtils.mutableListOf("正则", "regex", "reg", "regexp");
    }

}
