package xin.vanilla.banira.event;

import lombok.Getter;
import org.springframework.context.ApplicationEvent;
import xin.vanilla.banira.config.entity.GroupConfig;

@Getter
public class GroupConfigReloadedEvent extends ApplicationEvent {
    private final GroupConfig newConfig;
    private final String configName;

    public GroupConfigReloadedEvent(Object source, String configName, GroupConfig newConfig) {
        super(source);
        this.newConfig = newConfig;
        this.configName = configName;
    }

}
