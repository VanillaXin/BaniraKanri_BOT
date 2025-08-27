package xin.vanilla.banira.config.entity.basic;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import xin.vanilla.banira.domain.KeyValue;

import java.util.Arrays;
import java.util.List;

/**
 * 定时任务指令配置
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class TimerInstructionsConfig {

    /**
     * 前缀 与 后缀
     */
    private List<KeyValue<String, String>> locator;


    {
        this.locator = Arrays.asList(
                new KeyValue<>("定时", "回复")
                , new KeyValue<>("time", "rep")
                , new KeyValue<>("timer", "reply")
        );
    }

}
