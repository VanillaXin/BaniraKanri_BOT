package xin.vanilla.banira.config.entity.basic;

import lombok.experimental.Accessors;
import xin.vanilla.banira.config.entity.extended.WifeConfig;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.Set;

@Accessors(chain = true)
public record OtherConfig(
        Set<String> hentaiPath,
        Set<WifeConfig> wifeConfig,
        Set<String> wifeInsConfig,
        Set<String> imageFaceToImage,
        String statusBgUrl
) {

    public static OtherConfig empty() {
        return new OtherConfig(
                BaniraUtils.mutableSetOf()
                , BaniraUtils.mutableSetOf()
                , BaniraUtils.mutableSetOf()
                , BaniraUtils.mutableSetOf()
                , ""
        );
    }

    public static OtherConfig preset() {
        return new OtherConfig(
                BaniraUtils.mutableSetOf("")

                , BaniraUtils.mutableSetOf(new WifeConfig("^抽(?<nick>.{2,5})$"
                , "$nick"
                , "$atUser 今天你的群友$wifeNick是\n$wifeHead『$wifeName』($wifeId) 喵！"
                , "$atUser 今天你已经有$wifeNick了喵！"))

                , BaniraUtils.mutableSetOf("wife", "老婆")

                , BaniraUtils.mutableSetOf("获取表情", "获取图片", "获取表情图片", "getface", "getimage", "getimg")

                , "bg.png"
        );
    }

}
