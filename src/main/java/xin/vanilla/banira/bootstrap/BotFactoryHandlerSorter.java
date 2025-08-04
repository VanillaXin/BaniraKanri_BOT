package xin.vanilla.banira.bootstrap;

import com.mikuac.shiro.model.HandlerMethod;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;
import org.springframework.util.MultiValueMap;
import xin.vanilla.banira.config.ConfigReloadedEvent;
import xin.vanilla.banira.config.entity.GlobalConfig;
import xin.vanilla.banira.config.entity.basic.BaseConfig;
import xin.vanilla.banira.util.ReflectionUtils;

import java.util.*;
import java.util.function.Supplier;
import java.util.stream.Collectors;

/**
 * 通过GlobalConfig.base.capability配置修改插件排序
 */
@Slf4j
@Component
public class BotFactoryHandlerSorter implements ApplicationListener<ContextRefreshedEvent> {

    private final Supplier<GlobalConfig> globalConfig;
    private final Object botFactory;

    public BotFactoryHandlerSorter(Supplier<GlobalConfig> globalConfig, org.springframework.context.ApplicationContext ctx) {
        this.globalConfig = globalConfig;
        this.botFactory = ctx.getBean(com.mikuac.shiro.core.BotFactory.class);
    }

    @Override
    public void onApplicationEvent(ContextRefreshedEvent event) {
        sortHandlers("Initial sorting (ContextRefreshedEvent)");
    }

    @EventListener
    public void onConfigReloaded(ConfigReloadedEvent<GlobalConfig> event) {
        sortHandlers("Hot update sorting (GlobalConfig reload)");
    }

    private void sortHandlers(String reason) {
        try {
            // 获取 annotationMethodContainer 字段
            Object container = ReflectionUtils.getFieldValue(botFactory, "annotationMethodContainer", Object.class);
            if (container == null) {
                LOGGER.warn("[{}] annotationMethodContainer is null", reason);
                return;
            }

            // 获取 annotationHandler 字段
            @SuppressWarnings("unchecked")
            MultiValueMap<Class<? extends java.lang.annotation.Annotation>, HandlerMethod> annotationHandler =
                    ReflectionUtils.getFieldValue(container, "annotationHandler", MultiValueMap.class);
            if (annotationHandler == null) {
                LOGGER.warn("[{}] annotationHandler is null or type mismatch", reason);
                return;
            }

            Map<String, Integer> capabilityMap = Optional.ofNullable(globalConfig)
                    .map(Supplier::get)
                    .map(GlobalConfig::baseConfig)
                    .map(BaseConfig::capability)
                    .orElse(Collections.emptyMap());

            for (Class<? extends java.lang.annotation.Annotation> annotation : new ArrayList<>(annotationHandler.keySet())) {
                List<HandlerMethod> original = new ArrayList<>(annotationHandler.get(annotation));
                List<HandlerMethod> sorted = original.stream()
                        .sorted(Comparator
                                .comparingInt((HandlerMethod hm) -> {
                                    String className = hm.getType().getName();
                                    return capabilityMap.getOrDefault(className, 0);
                                })
                                .reversed()
                        )
                        .collect(Collectors.toList());

                annotationHandler.remove(annotation);
                annotationHandler.addAll(annotation, sorted);
            }

            LOGGER.info("[{}] re-sorted by capability (capability={})", reason, capabilityMap);
        } catch (Exception e) {
            LOGGER.error("Fail to sort handlers", e);
        }
    }
}
