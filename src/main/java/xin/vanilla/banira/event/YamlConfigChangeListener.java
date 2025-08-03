package xin.vanilla.banira.event;

import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.ConfigReloadedEvent;
import xin.vanilla.banira.config.entity.GlobalConfig;
import xin.vanilla.banira.config.entity.GroupConfig;

@Component
public class YamlConfigChangeListener {

    @EventListener
    public void onAppConfigReloaded(ConfigReloadedEvent<GlobalConfig> event) {

    }

    @EventListener
    public void onDbConfigReloaded(ConfigReloadedEvent<GroupConfig> event) {

    }
}

