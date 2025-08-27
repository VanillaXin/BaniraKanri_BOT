package xin.vanilla.banira.config;

import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import xin.vanilla.banira.config.entity.GlobalConfig;
import xin.vanilla.banira.config.entity.GroupConfig;
import xin.vanilla.banira.config.entity.InstructionsConfig;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.function.Supplier;

@Configuration
public class YamlConfigAutoConfiguration {

    @Bean
    public YamlConfigManager<GlobalConfig> globalConfigManager(YamlConfigWatcherService watcherService,
                                                               ApplicationEventPublisher publisher
    ) throws Exception {
        Path path = Paths.get("./config/global-config.yml");
        GlobalConfig defaults = new GlobalConfig();
        return new YamlConfigManager<>(path, defaults, GlobalConfig.class, "global-config", watcherService, publisher);
    }

    @Bean
    public Supplier<GlobalConfig> globalConfig(YamlConfigManager<GlobalConfig> manager) {
        return manager::getCurrent;
    }

    @Bean
    public YamlConfigManager<GroupConfig> groupConfigManager(YamlConfigWatcherService watcherService,
                                                             ApplicationEventPublisher publisher
    ) throws Exception {
        Path path = Paths.get("./config/group-config.yml");
        GroupConfig defaults = new GroupConfig();
        return new YamlConfigManager<>(path, defaults, GroupConfig.class, "group-config", watcherService, publisher);
    }

    @Bean
    public Supplier<GroupConfig> groupConfig(YamlConfigManager<GroupConfig> manager) {
        return manager::getCurrent;
    }

    @Bean
    public YamlConfigManager<InstructionsConfig> insConfigManager(YamlConfigWatcherService watcherService,
                                                                  ApplicationEventPublisher publisher
    ) throws Exception {
        Path path = Paths.get("./config/ins-config.yml");
        InstructionsConfig defaults = new InstructionsConfig();
        return new YamlConfigManager<>(path, defaults, InstructionsConfig.class, "ins-config", watcherService, publisher);
    }

    @Bean
    public Supplier<InstructionsConfig> insConfig(YamlConfigManager<InstructionsConfig> manager) {
        return manager::getCurrent;
    }
}
