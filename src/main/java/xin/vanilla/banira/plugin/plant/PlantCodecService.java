package xin.vanilla.banira.plugin.plant;

import jakarta.annotation.Nonnull;
import org.springframework.stereotype.Service;
import xin.vanilla.banira.domain.KeyValue;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.PlantCipher;
import xin.vanilla.banira.util.StringUtils;

/**
 * 花言草语编解码，供插件指令与 AI 能力共用
 */
@Service
public class PlantCodecService {

    @Nonnull
    public String transform(@Nonnull String content) {
        if (StringUtils.isNullOrEmptyEx(content)) {
            return "内容不能为空";
        }
        if (PlantCipher.isAroundLocator(content)) {
            return PlantCipher.decode(content);
        }
        KeyValue<String, String> locator = CollectionUtils.getRandomElement(PlantCipher.LOCATOR);
        return locator.getKey() + PlantCipher.encode(content) + locator.getValue();
    }

}
