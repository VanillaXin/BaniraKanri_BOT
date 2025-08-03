package xin.vanilla.banira.starter;

import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.ComponentScan;

@SpringBootApplication
@EnableConfigurationProperties
@MapperScan("xin.vanilla.banira.mapper")
@ComponentScan("xin.vanilla.banira")
public class BaniraKanriApplication {

    public static void main(String[] args) {
        SpringApplication.run(BaniraKanriApplication.class, args);
    }

}
