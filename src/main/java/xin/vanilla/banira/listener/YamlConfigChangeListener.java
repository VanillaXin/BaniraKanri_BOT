package xin.vanilla.banira.listener;

import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.ConfigReloadedEvent;
import xin.vanilla.banira.config.entity.GlobalConfig;
import xin.vanilla.banira.config.entity.GroupConfig;

@Component
public class YamlConfigChangeListener {

    @EventListener
    public void onGlobalConfigReloaded(ConfigReloadedEvent<GlobalConfig> event) {

    }

    @EventListener
    public void onGroupConfigReloaded(ConfigReloadedEvent<GroupConfig> event) {

    }
}
