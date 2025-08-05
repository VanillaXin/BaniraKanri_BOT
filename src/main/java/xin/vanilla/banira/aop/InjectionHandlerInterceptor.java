package xin.vanilla.banira.aop;

import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.stereotype.Component;

/**
 * 拦截 InjectionHandler 的 invoke，配合 ShiroAnnotationInterceptor 实现事件拦截
 */
@Slf4j
@Aspect
@Component
public class InjectionHandlerInterceptor {

    /**
     * 切入 private &lt;T&gt; void invoke(Bot, T, Class&lt;? extends Annotation&gt;)
     */
    @SuppressWarnings("all")
    @Around("execution(private void com.mikuac.shiro.handler.injection.InjectionHandler.invoke(com.mikuac.shiro.core.Bot, java.lang.Object, java.lang.Class))")
    public Object aroundInvoke(ProceedingJoinPoint pjp) throws Throwable {
        ShiroCallContextHolder.clear();
        return pjp.proceed();
    }

    /**
     * 切入 public void invokeMessage(Bot, MessageEvent, Optional&lt;List&lt;HandlerMethod&gt;&gt;)
     */
    @Around("execution(public void com.mikuac.shiro.handler.injection.InjectionHandler.invokeMessage(com.mikuac.shiro.core.Bot, com.mikuac.shiro.dto.event.message.MessageEvent, java.util.Optional))")
    public Object aroundInvokeMessage(ProceedingJoinPoint pjp) throws Throwable {
        ShiroCallContextHolder.clear();
        return pjp.proceed();
    }

}
