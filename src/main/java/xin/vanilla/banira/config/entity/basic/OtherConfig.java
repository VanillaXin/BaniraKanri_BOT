package xin.vanilla.banira.config.entity.basic;

import lombok.experimental.Accessors;
import xin.vanilla.banira.config.entity.extended.McConfig;
import xin.vanilla.banira.config.entity.extended.WifeConfig;

import java.util.Arrays;
import java.util.List;

@Accessors(chain = true)
public record OtherConfig(
        List<String> hentaiPath,
        List<WifeConfig> wifeConfig,
        String statusBgUrl,
        McConfig mcConfig
) {

    public static OtherConfig preset() {
        return new OtherConfig(
                Arrays.asList("")

                , Arrays.asList(new WifeConfig("^抽(?<nick>.{2,5})$"
                , "$nick"
                , "$atUser 今天你的群友$wifeNick是\n$wifeHead『$wifeName』($wifeId) 喵！"
                , "$atUser 今天你已经有$wifeNick了喵！"))

                , "bg.png"

                , McConfig.preset()
        );
    }

}
