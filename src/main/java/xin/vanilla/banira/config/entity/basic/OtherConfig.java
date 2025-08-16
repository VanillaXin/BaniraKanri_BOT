package xin.vanilla.banira.config.entity.basic;

import lombok.experimental.Accessors;
import xin.vanilla.banira.config.entity.extended.WifeConfig;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.Set;

@Accessors(chain = true)
public record OtherConfig(
        Set<String> hentaiPath,
        Set<WifeConfig> wifeConfig,
        Set<String> imageFaceToImage
) {

    public static OtherConfig preset() {
        return new OtherConfig(
                BaniraUtils.mutableSetOf("")
                , BaniraUtils.mutableSetOf(new WifeConfig("^抽(<nick>.{2,5})$", "$nick"))
                , BaniraUtils.mutableSetOf("getface", "getimage", "getimg")
        );
    }

}
