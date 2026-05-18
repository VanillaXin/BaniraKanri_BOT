package xin.vanilla.banira.listener;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.GlobalConfig;
import xin.vanilla.banira.event.GlobalConfigReloadedEvent;
import xin.vanilla.banira.event.GroupConfigReloadedEvent;

import java.util.function.Supplier;

@Slf4j
@Component
public class YamlConfigChangeListener {

    private final GlobalConfig globalConfig;
    private final ConfigurableApplicationContext applicationContext;
    @Value("${spring.profiles.active}")
    private String env;

    public YamlConfigChangeListener(Supplier<GlobalConfig> globalConfig, ConfigurableApplicationContext applicationContext) {
        this.globalConfig = globalConfig.get();
        this.applicationContext = applicationContext;
    }

    @EventListener
    public void onGlobalConfigReloaded(GlobalConfigReloadedEvent event) {
        if ((event.getNewConfig().token() != null
                && !event.getNewConfig().token().equals(globalConfig.token()))
                || (event.getNewConfig().wsUrl() != null
                && !event.getNewConfig().wsUrl().equals(globalConfig.wsUrl()))
                || (event.getNewConfig().env() != null
                && !event.getNewConfig().env().equals(env))
        ) {
            LOGGER.warn("Core connection config changed, shutting down application context gracefully.");
            Thread shutdownThread = new Thread(() -> applicationContext.close(), "config-reload-shutdown");
            shutdownThread.setDaemon(false);
            shutdownThread.start();
        }
    }

    @EventListener
    public void onGroupConfigReloaded(GroupConfigReloadedEvent event) {

    }
}
