package xin.vanilla.banira.config.entity.basic;

import lombok.experimental.Accessors;
import xin.vanilla.banira.domain.KeyValue;

import java.util.Arrays;
import java.util.List;

/**
 * 定时任务指令配置
 *
 * @param locator 前缀 与 后缀
 */
@Accessors(chain = true)
public record TimerInstructionsConfig(
        List<KeyValue<String, String>> locator
) {

    public static TimerInstructionsConfig preset() {
        return new TimerInstructionsConfig(
                Arrays.asList(
                        new KeyValue<>("定时", "回复")
                        , new KeyValue<>("time", "rep")
                        , new KeyValue<>("timer", "reply")
                )
        );
    }

}
