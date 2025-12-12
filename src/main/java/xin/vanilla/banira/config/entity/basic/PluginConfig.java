package xin.vanilla.banira.config.entity.basic;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import xin.vanilla.banira.plugin.*;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * 基础配置
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class PluginConfig {

    /**
     * 插件启用状态
     */
    private Map<String, Integer> capability;


    {
        this.capability = new LinkedHashMap<>();

        this.capability.put(KanriPlugin.class.getName(), 1);
        this.capability.put(HelpPlugin.class.getName(), 2);
        this.capability.put(TimerPlugin.class.getName(), 2);
        this.capability.put(StatusPlugin.class.getName(), 2);
        this.capability.put(KeywordPlugin.class.getName(), 2);

        this.capability.put(ExamplePlugin.class.getName(), 0);
        this.capability.put(WifePlugin.class.getName(), 99);
        this.capability.put(McQueryPlugin.class.getName(), 99);
        this.capability.put(DownloadMediaPlugin.class.getName(), 99);
        this.capability.put(TapPlugin.class.getName(), 99);
        this.capability.put(PlantPlugin.class.getName(), 99);
        this.capability.put(AIChatPlugin.class.getName(), 100);
        this.capability.put(SocialMediaPlugin.class.getName(), 99);
        this.capability.put(McModPlugin.class.getName(), 99);

    }

}
