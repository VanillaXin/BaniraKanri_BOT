package xin.vanilla.banira.plugin.help;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.annotation.Resource;
import org.springframework.aop.support.AopUtils;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.basic.PluginConfig;
import xin.vanilla.banira.plugin.RecorderPlugin;
import xin.vanilla.banira.plugin.common.BasePlugin;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.function.Supplier;

/**
 * 帮助主题注册中心，聚合各插件贡献的主题并按 capability 过滤
 */
@Component
public class HelpTopicRegistry {

    @Resource
    private ApplicationContext applicationContext;
    @Resource
    private Supplier<PluginConfig> pluginConfig;

    @Nonnull
    public List<HelpTopic> list(@Nullable Long groupId) {
        List<HelpTopic> topics = new ArrayList<>();
        Map<String, BasePlugin> pluginBeans = applicationContext.getBeansOfType(BasePlugin.class);
        for (BasePlugin plugin : pluginBeans.values()) {
            if (!isEnabled(plugin)) {
                continue;
            }
            List<HelpTopic> contributed = new ArrayList<>();
            plugin.registerHelpTopics(contributed, groupId);
            String ownerClass = resolvePluginClassName(plugin);
            contributed.forEach(topic -> {
                if (StringUtils.isNullOrEmptyEx(topic.ownerClass())) {
                    topic.ownerClass(ownerClass);
                }
                topics.add(topic);
            });
        }
        return topics.stream()
                .sorted(Comparator.comparingInt((HelpTopic t) -> t.order())
                        .thenComparing((HelpTopic t) -> t.name()))
                .toList();
    }

    @Nullable
    public HelpTopic resolve(@Nullable Long groupId, @Nullable String alias) {
        if (StringUtils.isNullOrEmptyEx(alias)) {
            return null;
        }
        return list(groupId).stream()
                .filter(topic -> topic.matches(alias))
                .findFirst()
                .orElse(null);
    }

    private boolean isEnabled(@Nonnull BasePlugin plugin) {
        if (plugin instanceof RecorderPlugin) {
            return false;
        }
        PluginConfig config = pluginConfig.get();
        if (config == null || config.capability() == null) {
            return false;
        }
        Integer capability = config.capability().get(resolvePluginClassName(plugin));
        return capability != null && capability > 0;
    }

    /** 解析 Spring 代理后的真实插件类名，与 PluginConfig.capability 键一致 */
    @Nonnull
    private static String resolvePluginClassName(@Nonnull BasePlugin plugin) {
        return AopUtils.getTargetClass(plugin).getName();
    }

}