package xin.vanilla.banira.config.entity;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.util.ArrayList;
import java.util.List;

/**
 * 全局配置
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class GlobalConfig {

    /**
     * 机器人TOKEN
     */
    private String token;
    /**
     * ws正向/反向地址
     */
    private String wsUrl;
    /**
     * 运行环境
     */
    private String env;
    /**
     * 主人
     */
    private Long owner;
    /**
     * 机器人昵称
     */
    private String botNick;
    /**
     * 后台群
     */
    private List<Long> backGroup;


    {
        this.token = "";
        this.wsUrl = "ws://127.0.0.1:8080";
        this.env = "prod";
        this.owner = 0L;
        this.botNick = "香草酱";
        this.backGroup = new ArrayList<>();
    }

}
