package xin.vanilla.banira.config.entity.extended;

import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

@Data
@Accessors(chain = true)
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
    String success;

    /**
     * 异常提示: 服务器没人在线
     */
    List<String> none;

    /**
     * 错误提示: 未知的主机
     */
    List<String> unknownHost;

    /**
     * 错误提示: 连接失败
     */
    List<String> connectFailed;

    /**
     * 错误提示: 密码错误
     */
    List<String> pswError;

    /**
     * 错误提示: 未知的响应
     */
    List<String> unknownResponse;

    public static McConfig empty() {
        return new McConfig();
    }

    public static McConfig preset() {
        return new McConfig()
                .setSuccess("[name]有[online]/[max]名玩家在线:\n[players]")
                .setNone(List.of("%s一片死寂."))
                .setUnknownHost(List.of("无法定位%s."))
                .setConnectFailed(List.of("%s一片混沌."))
                .setPswError(List.of("%s.需要..密码..."))
                .setUnknownResponse(List.of("%s发回了一串未知的信号."))
                ;
    }

}
