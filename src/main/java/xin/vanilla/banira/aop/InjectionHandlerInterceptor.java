package xin.vanilla.banira.aop;

import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.stereotype.Component;

import java.lang.reflect.Method;

/**
 * 拦截 InjectionHandler 的 invoke，配合 ShiroAnnotationInterceptor 实现事件拦截
 */
@Slf4j
@Aspect
@Component
public class InjectionHandlerInterceptor {

    /**
     * 切入InjectionHandler
     */
    @Around("execution(public * com.mikuac.shiro.handler.injection.InjectionHandler.invoke*(..))")
    public Object aroundInvokeInjectionHandlerMethods(ProceedingJoinPoint pjp) throws Throwable {
        MethodSignature sig = (MethodSignature) pjp.getSignature();
        String methodName = resolveActualMethod(pjp, sig.getMethod()).getName();
        if (!methodName.equalsIgnoreCase("invokeHeartbeat")
                && !methodName.equalsIgnoreCase("invokeLifecycle")
        ) ShiroCallContextHolder.clear();
        return pjp.proceed();
    }

    private Method resolveActualMethod(ProceedingJoinPoint pjp, Method declared) {
        try {
            return pjp.getTarget().getClass()
                    .getMethod(declared.getName(), declared.getParameterTypes());
        } catch (NoSuchMethodException e) {
            return declared;
        }
    }

}
