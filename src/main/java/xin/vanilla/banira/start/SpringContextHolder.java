package xin.vanilla.banira.start;

import org.springframework.beans.factory.DisposableBean;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.core.ResolvableType;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;

@Component
public class SpringContextHolder implements ApplicationContextAware, DisposableBean {

    private static ApplicationContext context;

    @Override
    public void setApplicationContext(@NonNull ApplicationContext applicationContext) {
        SpringContextHolder.context = applicationContext;
    }

    public static <T> T getBean(Class<T> requiredType) {
        Assert.state(context != null, "Spring ApplicationContext not initialized or cleaned");
        return context.getBean(requiredType);
    }

    @SuppressWarnings("unchecked")
    public static <T> T getBean(ResolvableType requiredType) {
        Assert.state(context != null, "Spring ApplicationContext not initialized or cleaned");
        String[] names = context.getBeanNamesForType(requiredType);
        if (names.length == 0) throw new IllegalStateException("No bean of type " + requiredType + " found");
        return (T) context.getBean(names[0]);
    }

    public static Object getBean(String name) {
        Assert.state(context != null, "Spring ApplicationContext not initialized or cleaned");
        return context.getBean(name);
    }

    @Override
    public void destroy() {
        context = null;
    }
}
