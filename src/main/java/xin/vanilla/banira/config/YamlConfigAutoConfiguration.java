package xin.vanilla.banira.config;

import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import xin.vanilla.banira.config.entity.GlobalConfig;
import xin.vanilla.banira.config.entity.GroupConfig;

import java.nio.file.Path;
import java.nio.file.Paths;

@Configuration
public class YamlConfigAutoConfiguration {

    @Bean
    public YamlConfigManager<GlobalConfig> globalConfigManager(YamlConfigWatcherService watcherService,
                                                               ApplicationEventPublisher publisher
    ) throws Exception {
        Path path = Paths.get("./config/global-config.yml");
        GlobalConfig defaults = GlobalConfig.preset();
        return new YamlConfigManager<>(path, defaults, GlobalConfig.class, "global-config", watcherService, publisher);
    }

    @Bean
    public GlobalConfig globalConfig(YamlConfigManager<GlobalConfig> manager) {
        return manager.getCurrent();
    }

    @Bean
    public YamlConfigManager<GroupConfig> groupConfigManager(YamlConfigWatcherService watcherService,
                                                             ApplicationEventPublisher publisher
    ) throws Exception {
        Path path = Paths.get("./config/group-config.yml");
        GroupConfig defaults = GroupConfig.preset();
        return new YamlConfigManager<>(path, defaults, GroupConfig.class, "group-config", watcherService, publisher);
    }

    @Bean
    public GroupConfig groupConfig(YamlConfigManager<GroupConfig> manager) {
        return manager.getCurrent();
    }
}
