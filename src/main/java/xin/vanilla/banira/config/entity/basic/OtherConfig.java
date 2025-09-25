package xin.vanilla.banira.config.entity.basic;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import xin.vanilla.banira.config.entity.extended.McConfig;
import xin.vanilla.banira.config.entity.extended.WifeConfig;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class OtherConfig {

    /**
     * 随机图片路径
     */
    private List<String> randomImgPath;

    /**
     * 抽老婆配置
     */
    private List<WifeConfig> wifeConfig;

    /**
     * 状态查询背景图地址
     */
    private String statusBgUrl;

    /**
     * 我的世界服务器状态查询配置
     */
    private McConfig mcConfig;


    {
        this.randomImgPath = new ArrayList<>();
        this.wifeConfig = BaniraUtils.mutableListOf(new WifeConfig("^抽(?<nick>.{2,5})$"
                , "$nick"
                , "$atUser 今天你的群友$wifeNick是\n$wifeHead『$wifeName』($wifeId) 喵！"
                , "$atUser 今天你已经有$wifeNick了喵！")
        );
        this.statusBgUrl = "bg.png";
        this.mcConfig = new McConfig();

    }

}
