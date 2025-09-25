package xin.vanilla.banira.config.entity.extended;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.util.List;

@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class McConfig {

    /**
     * 查询成功提示
     * [name]: 服务器名称
     * [motd]: 服务器描述
     * [host]: 服务器地址
     * [port]: 服务器端口
     * [version]: 服务器版本
     * [players]: 在线玩家名称列表
     * [online]: 在线玩家数量
     * [max]: 最大玩家数量
     */
    private String success;

    /**
     * 异常提示: 服务器没人在线
     */
    private List<String> none;

    /**
     * 错误提示: 未知的主机
     */
    private List<String> unknownHost;

    /**
     * 错误提示: 连接失败
     */
    private List<String> connectFailed;

    /**
     * 错误提示: 密码错误
     */
    private List<String> pswError;

    /**
     * 错误提示: 未知的响应
     */
    private List<String> unknownResponse;


    {
        this.success = "[name]有[online]/[max]名玩家在线:\n[players]";
        this.none = Arrays.asList("%s一片死寂.");
        this.unknownHost = Arrays.asList("无法定位%s.");
        this.connectFailed = Arrays.asList("%s一片混沌.");
        this.pswError = Arrays.asList("%s.需要..密码...");
        this.unknownResponse = Arrays.asList("%s发回了一串未知的信号.");
    }

}
