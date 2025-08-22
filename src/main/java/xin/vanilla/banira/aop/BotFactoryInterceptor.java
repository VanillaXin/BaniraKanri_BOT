package xin.vanilla.banira.aop;

import com.mikuac.shiro.core.Bot;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.plugin.common.BaniraBot;

/**
 * 拦截 BotFactory.createBot() 方法使其返回 BaniraBot
 */
@Slf4j
@Aspect
@Component
public class BotFactoryInterceptor {

    @Around("execution(public com.mikuac.shiro.core.Bot com.mikuac.shiro.core.BotFactory.createBot(..))")
    public Bot aroundCreateBot(ProceedingJoinPoint pjp) throws Throwable {
        return new BaniraBot((Bot) pjp.proceed());
    }

}
