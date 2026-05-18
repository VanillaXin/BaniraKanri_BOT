package xin.vanilla.banira.event;

import lombok.Getter;
import org.springframework.context.ApplicationEvent;
import xin.vanilla.banira.config.entity.basic.PluginConfig;

@Getter
public class PluginConfigReloadedEvent extends ApplicationEvent {
    private final PluginConfig newConfig;
    private final String configName;

    public PluginConfigReloadedEvent(Object source, String configName, PluginConfig newConfig) {
        super(source);
        this.newConfig = newConfig;
        this.configName = configName;
    }

}
