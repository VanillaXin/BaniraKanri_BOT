package xin.vanilla.banira.config.entity.basic;

import lombok.experimental.Accessors;
import xin.vanilla.banira.config.entity.extended.WifeConfig;

import java.util.Arrays;
import java.util.List;

@Accessors(chain = true)
public record OtherConfig(
        List<String> hentaiPath,
        List<WifeConfig> wifeConfig,
        List<String> wifeInsConfig,
        List<String> imageFaceToImage,
        String statusBgUrl
) {

    public static OtherConfig empty() {
        return new OtherConfig(
                Arrays.asList()
                , Arrays.asList()
                , Arrays.asList()
                , Arrays.asList()
                , ""
        );
    }

    public static OtherConfig preset() {
        return new OtherConfig(
                Arrays.asList("")

                , Arrays.asList(new WifeConfig("^抽(?<nick>.{2,5})$"
                , "$nick"
                , "$atUser 今天你的群友$wifeNick是\n$wifeHead『$wifeName』($wifeId) 喵！"
                , "$atUser 今天你已经有$wifeNick了喵！"))

                , Arrays.asList("wife", "老婆")

                , Arrays.asList("获取表情", "获取图片", "获取表情图片", "getface", "getimage", "getimg")

                , "bg.png"
        );
    }

}
