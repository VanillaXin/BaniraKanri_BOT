package xin.vanilla.banira.domain;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * MC服务器记录
 */
@Data
@Accessors(chain = true)
public class MinecraftRecord {
    /**
     * MC服务器ID
     */
    @TableId(type = IdType.AUTO)
    private Long id;
    /**
     * 机器人ID
     */
    private Long botId;
    /**
     * 群组ID
     */
    private Long groupId = 0L;
    /**
     * 添加者ID
     */
    private Long creatorId;
    /**
     * 添加时间
     */
    private Long time;
    /**
     * 服务器名称
     */
    private String name;
    /**
     * 查询地址
     */
    private String queryIp;
    /**
     * 查询端口
     */
    private Integer queryPort = 25565;
    /**
     * RCON地址
     */
    private String rconIp;
    /**
     * RCON端口
     */
    private Integer rconPort = 0;
    /**
     * RCON密码
     */
    private String rconPsw;
    /**
     * 启用状态
     */
    private Boolean enable = true;

}
