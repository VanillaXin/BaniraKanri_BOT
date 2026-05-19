package xin.vanilla.banira.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import xin.vanilla.banira.config.entity.basic.PluginConfig;
import xin.vanilla.banira.config.other.OtherConfigRegistry;
import xin.vanilla.banira.plugin.socialmedia.SocialMediaSettings;

import java.util.function.Supplier;

/**
 * 其他配置自动装配。
 * 负责暴露 SharedConfig 的常用 Supplier Bean。
 */
@Configuration
public class OtherConfigAutoConfiguration {

    /**
     * 插件配置 Supplier。
     */
    @Bean
    public Supplier<PluginConfig> pluginConfig(OtherConfigRegistry otherConfigRegistry) {
        return otherConfigRegistry.sharedSupplier(PluginConfig.class);
    }

    /**
     * 社交媒体配置 Supplier。
     */
    @Bean
    public Supplier<SocialMediaSettings> socialMediaConfig(OtherConfigRegistry otherConfigRegistry) {
        return otherConfigRegistry.sharedSupplier(SocialMediaSettings.class);
    }
}
