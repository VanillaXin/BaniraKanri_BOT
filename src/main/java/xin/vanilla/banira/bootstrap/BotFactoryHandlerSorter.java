package xin.vanilla.banira.bootstrap;

import com.mikuac.shiro.core.BotFactory;
import com.mikuac.shiro.model.HandlerMethod;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;
import org.springframework.util.MultiValueMap;
import xin.vanilla.banira.event.ConfigReloadedEvent;
import xin.vanilla.banira.config.entity.GlobalConfig;
import xin.vanilla.banira.config.entity.basic.BaseConfig;
import xin.vanilla.banira.plugin.RecorderPlugin;
import xin.vanilla.banira.util.ReflectionUtils;

import java.lang.annotation.Annotation;
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
            BotFactory.AnnotationMethodContainer container = ReflectionUtils.getFieldValue(botFactory
                    , "annotationMethodContainer"
                    , BotFactory.AnnotationMethodContainer.class
            );
            if (container == null) {
                LOGGER.warn("[{}] annotationMethodContainer is null", reason);
                return;
            }

            MultiValueMap<Class<? extends java.lang.annotation.Annotation>, HandlerMethod> annotationHandler =
                    container.getAnnotationHandler();

            this.sort(annotationHandler, reason);
        } catch (Exception e) {
            LOGGER.error("Fail to sort handlers", e);
        }
    }

    private void sort(MultiValueMap<Class<? extends Annotation>, HandlerMethod> annotationHandler, String reason) {
        if (annotationHandler.isEmpty()) {
            LOGGER.debug("No handlers to sort");
        } else {
            Map<String, Integer> capabilityMap = Optional.ofNullable(globalConfig)
                    .map(Supplier::get)
                    .map(GlobalConfig::baseConfig)
                    .map(BaseConfig::capability)
                    .orElse(Collections.emptyMap());

            LOGGER.debug("Starting to sort handlers by capability");
            annotationHandler.keySet().forEach((annotation) -> {
                LOGGER.debug("Sorting handlers for annotation: {}", annotation.getSimpleName());
                List<HandlerMethod> handlers = annotationHandler.get(annotation);
                handlers = handlers.stream().distinct().sorted(Comparator.comparing((handlerMethod) -> {
                    if (handlerMethod.getType().equals(RecorderPlugin.class)) return -1;
                    int orderValue = capabilityMap.getOrDefault(handlerMethod.getType().getName(), 0);
                    LOGGER.debug("Method: {}#{} has order value: {}", handlerMethod.getType().getName(), handlerMethod.getMethod().getName(), orderValue);
                    return orderValue;
                })).collect(Collectors.toCollection(ArrayList::new));
                LOGGER.debug("Sorted {} handlers for annotation: {}", handlers.size(), annotation.getSimpleName());
                annotationHandler.put(annotation, handlers);
                LOGGER.debug("Handler sorting completed");
            });
            LOGGER.info("[{}] re-sorted by capability (capability={})", reason, capabilityMap);
        }
    }
}
