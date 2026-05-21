package xin.vanilla.banira.data;

import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.context.ApplicationContext;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.event.DatabaseInitializedEvent;

import javax.sql.DataSource;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;

/**
 * sqlite schema initializer
 */
@Component
public class SQLiteSchemaInitializer implements ApplicationRunner {

    @jakarta.annotation.Resource
    private DataSource dataSource;

    @jakarta.annotation.Resource
    private ApplicationContext applicationContext;

    @Override
    public void run(ApplicationArguments args) throws Exception {
        Resource resource = new ClassPathResource("sql/1.init.sql");
        String sql = new String(resource.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        try (Connection conn = dataSource.getConnection()) {
            Statement stmt = conn.createStatement();
            for (String part : sql.split(";")) {
                String trimmed = part.trim();
                if (!trimmed.isEmpty()) {
                    stmt.execute(trimmed);
                }
            }
            migrate(stmt);
        }
        applicationContext.publishEvent(new DatabaseInitializedEvent(this));
    }

    private void migrate(Statement stmt) {
        try {
            stmt.execute("ALTER TABLE minecraft_record ADD COLUMN rcon_operators TEXT NOT NULL DEFAULT ''");
        } catch (SQLException ignored) {
            // 列已存在
        }
    }
}
