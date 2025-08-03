package xin.vanilla.banira.config;

import lombok.Getter;
import org.springframework.context.ApplicationEvent;

@Getter
public class ConfigReloadedEvent<T> extends ApplicationEvent {
    private final T newConfig;
    private final Class<T> configClass;
    private final String configName;

    public ConfigReloadedEvent(Object source, String configName, Class<T> configClass, T newConfig) {
        super(source);
        this.newConfig = newConfig;
        this.configClass = configClass;
        this.configName = configName;
    }

}
