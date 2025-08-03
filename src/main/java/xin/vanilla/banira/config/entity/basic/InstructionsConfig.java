package xin.vanilla.banira.config.entity.basic;

import lombok.experimental.Accessors;

/**
 * 指令配置
 *
 * @param prefix 前缀
 * @param base   基础指令
 * @param key    关键词指令
 * @param timer  定时任务指令
 * @param kanri  群管指令
 */
@Accessors(chain = true)
public record InstructionsConfig(
        String prefix,
        BaseInstructionsConfig base,
        KeyInstructionsConfig key,
        TimerInstructionsConfig timer,
        KanriInstructionsConfig kanri
) {

    public static InstructionsConfig preset() {
        return new InstructionsConfig(
                "/bk",
                BaseInstructionsConfig.preset(),
                KeyInstructionsConfig.preset(),
                TimerInstructionsConfig.preset(),
                KanriInstructionsConfig.preset()
        );
    }

}
