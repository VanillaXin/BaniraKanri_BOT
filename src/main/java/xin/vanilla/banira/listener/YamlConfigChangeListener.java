package xin.vanilla.banira.listener;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.GlobalConfig;
import xin.vanilla.banira.config.entity.GroupConfig;
import xin.vanilla.banira.event.ConfigReloadedEvent;

import java.util.function.Supplier;

@Component
public class YamlConfigChangeListener {

    private final GlobalConfig globalConfig;
    @Value("${spring.profiles.active}")
    private String env;

    public YamlConfigChangeListener(Supplier<GlobalConfig> globalConfig) {
        this.globalConfig = globalConfig.get();
    }

    @EventListener
    public void onGlobalConfigReloaded(ConfigReloadedEvent<GlobalConfig> event) {
        if ((event.getNewConfig().token() != null
                && !event.getNewConfig().token().equals(globalConfig.token()))
                || (event.getNewConfig().wsUrl() != null
                && !event.getNewConfig().wsUrl().equals(globalConfig.wsUrl()))
                || (event.getNewConfig().env() != null
                && !event.getNewConfig().env().equals(env))
        ) {
            // 使程序异常退出
            Runtime.getRuntime().halt(1);
        }
    }

    @EventListener
    public void onGroupConfigReloaded(ConfigReloadedEvent<GroupConfig> event) {

    }
}
