package xin.vanilla.banira.config.entity.basic;


import lombok.experimental.Accessors;
import xin.vanilla.banira.domain.KeyValue;

import java.util.Arrays;
import java.util.List;

/**
 * 关键词指令配置
 *
 * @param locator 前缀 与 后缀
 * @param exactly 精确匹配
 * @param contain 包含匹配
 * @param pinyin  拼音匹配
 * @param regex   正则匹配
 */
@Accessors(chain = true)
public record KeyInstructionsConfig(
        List<KeyValue<String, String>> locator,
        List<String> exactly,
        List<String> contain,
        List<String> pinyin,
        List<String> regex
) {

    public static KeyInstructionsConfig preset() {
        return new KeyInstructionsConfig(
                Arrays.asList(
                        new KeyValue<>("关键词", "回复")
                        , new KeyValue<>("start", "end")
                        , new KeyValue<>("key", "rep")
                        , new KeyValue<>("keyword", "reply")
                ),
                Arrays.asList("完全", "精准", "exactly", "perfect", "per"),
                Arrays.asList("包含", "contain", "include", "inc"),
                Arrays.asList("拼音", "pinyin", "pin"),
                Arrays.asList("正则", "regex", "reg", "regexp")
        );
    }

}
