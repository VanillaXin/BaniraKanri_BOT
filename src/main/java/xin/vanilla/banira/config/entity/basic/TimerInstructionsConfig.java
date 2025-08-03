package xin.vanilla.banira.config.entity.basic;

import lombok.experimental.Accessors;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.Set;

/**
 * 定时任务指令配置
 *
 * @param prefix 前缀
 * @param suffix 后缀
 */
@Accessors(chain = true)
public record TimerInstructionsConfig(
        Set<String> prefix,
        Set<String> suffix
) {

    public static TimerInstructionsConfig preset() {
        return new TimerInstructionsConfig(
                BaniraUtils.mutableSetOf("定时", "timer"),
                BaniraUtils.mutableSetOf("回复", "reply", "replay", "rep")
        );
    }

}
