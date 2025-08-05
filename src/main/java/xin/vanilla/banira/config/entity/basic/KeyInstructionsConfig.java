package xin.vanilla.banira.config.entity.basic;


import lombok.experimental.Accessors;
import xin.vanilla.banira.domain.KeyValue;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.Set;

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
        Set<KeyValue<String, String>> locator,
        Set<String> exactly,
        Set<String> contain,
        Set<String> pinyin,
        Set<String> regex
) {

    public static KeyInstructionsConfig preset() {
        return new KeyInstructionsConfig(
                BaniraUtils.mutableSetOf(
                        new KeyValue<>("关键词", "回复")
                        , new KeyValue<>("start", "end")
                        , new KeyValue<>("key", "rep")
                        , new KeyValue<>("keyword", "reply")
                ),
                BaniraUtils.mutableSetOf("完全", "精准", "exactly", "perfect", "per"),
                BaniraUtils.mutableSetOf("包含", "contain", "include", "inc"),
                BaniraUtils.mutableSetOf("拼音", "pinyin", "pin"),
                BaniraUtils.mutableSetOf("正则", "regex", "reg", "regexp")
        );
    }

}
