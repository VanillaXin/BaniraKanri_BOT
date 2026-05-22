package xin.vanilla.banira.config.entity;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonFormat;
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
     * 机器人昵称。第一个作为主昵称，旧版单字符串配置也会被兼容为单元素列表。
     */
    @JsonFormat(with = JsonFormat.Feature.ACCEPT_SINGLE_VALUE_AS_ARRAY)
    private List<String> botNick;
    /**
     * 主人昵称。非空时 AI 应优先用这个称呼 owner 账号，而不是直接叫“主人”。
     */
    private String ownerNick;
    /**
     * 后台群
     */
    private List<Long> backGroup;


    {
        this.token = "";
        this.wsUrl = "ws://127.0.0.1:8080";
        this.env = "prod";
        this.owner = 0L;
        this.botNick = new ArrayList<>(List.of("香草酱"));
        this.ownerNick = "";
        this.backGroup = new ArrayList<>();
    }

}
