package xin.vanilla.banira.event;

import lombok.Getter;
import org.springframework.context.ApplicationEvent;
import xin.vanilla.banira.config.entity.InstructionsConfig;

@Getter
public class InstructionsConfigReloadedEvent extends ApplicationEvent {
    private final InstructionsConfig newConfig;
    private final String configName;

    public InstructionsConfigReloadedEvent(Object source, String configName, InstructionsConfig newConfig) {
        super(source);
        this.newConfig = newConfig;
        this.configName = configName;
    }

}
