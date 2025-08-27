package xin.vanilla.banira.config.entity.extended;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * 抽老婆配置
 */
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class WifeConfig {

    /**
     * 触发表达式
     */
    private String reg;
    /**
     * 昵称
     */
    private String nick;
    /**
     * 成功提示
     */
    private String success;
    /**
     * 失败提示
     */
    private String fail;

}
