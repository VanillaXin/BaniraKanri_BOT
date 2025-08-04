package xin.vanilla.banira.aop;

import com.mikuac.shiro.core.BotPlugin;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.GlobalConfig;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Supplier;

/**
 * 通过GlobalConfig.base.capability配置检查插件是否启用
 */
@Slf4j
@Aspect
@Component
public class ShiroAnnotationInterceptor {

    private static final String TARGET_PACKAGE = "com.mikuac.shiro.annotation";

    private final Supplier<GlobalConfig> globalConfig;

    private static final ThreadLocal<Boolean> BLOCKED = ThreadLocal.withInitial(() -> false);
    private static final ThreadLocal<Object> BLOCK_RESULT = new ThreadLocal<>();
    private static final ThreadLocal<Integer> DEPTH = ThreadLocal.withInitial(() -> 0);

    private final Map<Method, Boolean> hasTargetAnnotationCache = new ConcurrentHashMap<>();

    public ShiroAnnotationInterceptor(Supplier<GlobalConfig> globalConfig) {
        this.globalConfig = globalConfig;
    }

    @Around("@within(com.mikuac.shiro.annotation.common.Shiro)")
    public Object aroundShiroClassMethods(ProceedingJoinPoint pjp) throws Throwable {
        // 增加深度计数（用于根调用清理）
        DEPTH.set(DEPTH.get() + 1);
        boolean isRoot = DEPTH.get() == 1;

        try {
            // 如果已经短路，直接返回同一个结果（不再执行任何逻辑）
            if (Boolean.TRUE.equals(BLOCKED.get())) {
                return BLOCK_RESULT.get();
            }

            MethodSignature sig = (MethodSignature) pjp.getSignature();
            Method declaredMethod = sig.getMethod();
            Method actualMethod = resolveActualMethod(pjp, declaredMethod);
            String className = pjp.getTarget().getClass().getName();
            String methodName = actualMethod.getName();

            // 如果方法上没有来自目标包的注解，则直接放行（不影响短路逻辑）
            if (!hasTargetAnnotation(actualMethod)) {
                return pjp.proceed();
            }

            // 判断是否允许执行
            boolean allowed = shouldProceed(className);
            Object result;
            if (!allowed) {
                LOGGER.debug("Plugin {}#{} is disabled", className, methodName);
                result = defaultReturnValue(actualMethod);
            } else {
                // 正常执行前置逻辑
                result = pjp.proceed();
            }

            // 统一判断是否触发短路条件：true 或 BotPlugin.MESSAGE_BLOCK
            if (isBlockingResult(result)) {
                BLOCKED.set(true);
                BLOCK_RESULT.set(result);
                LOGGER.debug("Method {}#{} triggered blocking, result={}, further calls in the same thread are skipped", className, methodName, result);
            }

            return result;
        } finally {
            DEPTH.set(DEPTH.get() - 1);
            // 根调用结束时清理线程上下文，避免污染后续独立调用链
            if (isRoot) {
                BLOCKED.remove();
                BLOCK_RESULT.remove();
                DEPTH.remove();
            }
        }
    }

    private boolean hasTargetAnnotation(Method method) {
        return hasTargetAnnotationCache.computeIfAbsent(method, m -> {
            for (Annotation ann : m.getAnnotations()) {
                Package pkg = ann.annotationType().getPackage();
                if (pkg != null && pkg.getName().startsWith(TARGET_PACKAGE)) {
                    return true;
                }
            }
            return false;
        });
    }

    private boolean shouldProceed(String className) {
        if (globalConfig.get() == null || globalConfig.get().baseConfig() == null || globalConfig.get().baseConfig().capability() == null) {
            return false;
        }
        Integer cap = globalConfig.get().baseConfig().capability().get(className);
        return cap != null && cap > 0;
    }

    private Method resolveActualMethod(ProceedingJoinPoint pjp, Method declared) {
        try {
            return pjp.getTarget().getClass()
                    .getMethod(declared.getName(), declared.getParameterTypes());
        } catch (NoSuchMethodException e) {
            return declared;
        }
    }

    private boolean isBlockingResult(Object result) {
        if (result == null) return false;
        if (result instanceof Boolean && Boolean.TRUE.equals(result)) return true;
        if (result != null && result.equals(BotPlugin.MESSAGE_BLOCK)) return true;
        return false;
    }

    private Object defaultReturnValue(Method method) {
        Class<?> returnType = method.getReturnType();
        if (returnType.equals(void.class) || returnType.equals(Void.class)) return null;
        if (Number.class.isAssignableFrom(returnType) || isPrimitiveNumeric(returnType)) {
            return BotPlugin.MESSAGE_IGNORE;
        }
        if (returnType.equals(boolean.class) || returnType.equals(Boolean.class)) {
            return Boolean.FALSE;
        }
        return null;
    }

    private boolean isPrimitiveNumeric(Class<?> type) {
        return type.equals(int.class) || type.equals(long.class) ||
                type.equals(short.class) || type.equals(byte.class) ||
                type.equals(double.class) || type.equals(float.class);
    }
}
