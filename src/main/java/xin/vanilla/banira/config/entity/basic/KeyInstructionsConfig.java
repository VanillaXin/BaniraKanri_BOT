package xin.vanilla.banira.config.entity.basic;


import lombok.experimental.Accessors;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.Set;

/**
 * 关键词指令配置
 *
 * @param prefix  前缀，与 后缀 配对使用
 * @param suffix  后缀，与 前缀 配对使用
 * @param exactly 精确匹配
 * @param contain 包含匹配
 * @param pinyin  拼音匹配
 * @param regex   正则匹配
 */
@Accessors(chain = true)
public record KeyInstructionsConfig(
        Set<String> prefix,
        Set<String> suffix,
        Set<String> exactly,
        Set<String> contain,
        Set<String> pinyin,
        Set<String> regex
) {

    public static KeyInstructionsConfig preset() {
        return new KeyInstructionsConfig(
                BaniraUtils.mutableSetOf("关键词", "start", "key", "keyword"),
                BaniraUtils.mutableSetOf("回复", "end", "rep", "reply"),
                BaniraUtils.mutableSetOf("完全", "精准", "exactly", "perfect", "per"),
                BaniraUtils.mutableSetOf("包含", "contain", "include", "inc"),
                BaniraUtils.mutableSetOf("拼音", "pinyin", "pin"),
                BaniraUtils.mutableSetOf("正则", "regex", "reg", "regexp")
        );
    }

}
