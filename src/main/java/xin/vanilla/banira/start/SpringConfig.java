package xin.vanilla.banira.start;

import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.EnableAspectJAutoProxy;
import org.springframework.context.annotation.ImportResource;

@Configuration
@EnableConfigurationProperties
@EnableAspectJAutoProxy(proxyTargetClass = true)
@MapperScan("xin.vanilla.**.mapper")
@ImportResource(locations = {"classpath:spring.xml"})
public class SpringConfig {

}
