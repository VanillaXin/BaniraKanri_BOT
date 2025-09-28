package xin.vanilla.banira.event;

import lombok.Getter;
import org.springframework.context.ApplicationEvent;
import xin.vanilla.banira.config.entity.GlobalConfig;

@Getter
public class GlobalConfigReloadedEvent extends ApplicationEvent {
    private final GlobalConfig newConfig;
    private final String configName;

    public GlobalConfigReloadedEvent(Object source, String configName, GlobalConfig newConfig) {
        super(source);
        this.newConfig = newConfig;
        this.configName = configName;
    }

}
