package xin.vanilla.banira.config.entity;

import jakarta.annotation.Nonnull;
import lombok.experimental.Accessors;
import xin.vanilla.banira.config.entity.basic.BaseInstructionsConfig;
import xin.vanilla.banira.config.entity.basic.KanriInstructionsConfig;
import xin.vanilla.banira.config.entity.basic.KeyInstructionsConfig;
import xin.vanilla.banira.config.entity.basic.TimerInstructionsConfig;

import java.util.Arrays;
import java.util.List;

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
        @Nonnull String prefix,
        @Nonnull BaseInstructionsConfig base,
        @Nonnull KeyInstructionsConfig key,
        @Nonnull TimerInstructionsConfig timer,
        @Nonnull KanriInstructionsConfig kanri,
        @Nonnull List<String> imageFaceToImage,
        @Nonnull List<String> wife,
        @Nonnull List<String> mcQuery
) {

    public static InstructionsConfig preset() {
        return new InstructionsConfig(
                "/bk",
                BaseInstructionsConfig.preset(),
                KeyInstructionsConfig.preset(),
                TimerInstructionsConfig.preset(),
                KanriInstructionsConfig.preset()

                , Arrays.asList("获取表情", "获取图片", "获取表情图片", "getface", "getimage", "getimg")

                , Arrays.asList("老婆", "wife")

                , Arrays.asList("我的世界", "麦块", "mc")

        );
    }

}
