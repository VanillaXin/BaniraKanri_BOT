package xin.vanilla.banira.event;

import org.springframework.context.ApplicationEvent;

public class DatabaseInitializedEvent extends ApplicationEvent {

    public DatabaseInitializedEvent(Object source) {
        super(source);
    }

}
