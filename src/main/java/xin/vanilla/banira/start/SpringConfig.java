package xin.vanilla.banira.start;

import org.apache.ibatis.plugin.Interceptor;
import org.mybatis.spring.annotation.MapperScan;
import org.quartz.Scheduler;
import org.quartz.impl.StdSchedulerFactory;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.*;
import xin.vanilla.banira.data.SQLitePageInterceptor;

@Configuration
@EnableConfigurationProperties
@MapperScan("xin.vanilla.**.mapper")
@EnableAspectJAutoProxy(proxyTargetClass = true)
@ImportResource(locations = {"classpath:spring.xml"})
@PropertySource(value = "classpath:git.properties", ignoreResourceNotFound = true)
public class SpringConfig {

    @Bean
    public Interceptor sqlitePageInterceptor() {
        return new SQLitePageInterceptor();
    }

    @Bean
    public Scheduler scheduler() throws Exception {
        Scheduler scheduler = StdSchedulerFactory.getDefaultScheduler();
        scheduler.start();
        return scheduler;
    }

}
