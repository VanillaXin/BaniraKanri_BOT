package xin.vanilla.banira.start;

import org.apache.ibatis.plugin.Interceptor;
import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.EnableAspectJAutoProxy;
import org.springframework.context.annotation.ImportResource;
import xin.vanilla.banira.data.SQLitePageInterceptor;

@Configuration
@EnableConfigurationProperties
@EnableAspectJAutoProxy(proxyTargetClass = true)
@MapperScan("xin.vanilla.**.mapper")
@ImportResource(locations = {"classpath:spring.xml"})
public class SpringConfig {

    @Bean
    public Interceptor sqlitePageInterceptor() {
        return new SQLitePageInterceptor();
    }

}
